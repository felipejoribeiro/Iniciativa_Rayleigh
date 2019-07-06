module callbacks

  use opengl_gl

  integer(kind=GLint)  :: winWidth = 400, winHeight = 300
  integer(kind=GLuint) :: Hexagon

  real :: xc, yc

end module callbacks
program fgl03

  use opengl_gl
  use opengl_glu
  use opengl_glut

  use callbacks

  interface  
    subroutine draw_hexagon() bind(C)
    end subroutine draw_hexagon

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface  

  integer(kind=GLint) :: iwin

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl03 Reshape and Display lists"//char(0))

  call initialize
  
  call glutDisplayFunc( draw_hexagon )
  call glutReshapeFunc( reshape )

  call glutMainLoop()

end program fgl03
subroutine initialize

  use opengl_gl  
  use callbacks

  integer(GLint) :: ix, iy
  
  ! set circle center coordinates
  xc = 0.5*float(winWidth)
  yc = 0.5*float(winHeight)

  call glClearColor(1.0, 1.0, 1.0, 0.0)
  
  Hexagon = glGenLists(1)
  call glNewList( Hexagon, GL_COMPILE)
  
  call glColor3f( 1.0, 0.0, 0.0 )
  call glBegin(GL_POLYGON)
  do i=1,7
    theta = 6.2831853 * float(i)/ 6.0
    x  = xc + 150.*cos(theta)
    y  = yc + 150.*sin(theta) 
    ix = x
    iy = y
    call glVertex2i( ix, iy)
  end do

  call glEnd()
  call glEndList()
  
end subroutine initialize
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl
  use opengl_glu 
  use callbacks

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, Width, Height

  Zero   = 0.0
  Width  = newWidth
  Height = newHeight

  call glMatrixMode( GL_PROJECTION )
  call glLoadIdentity()
  
  call gluOrtho2D( Zero, Width, Zero, Height )
  
  call glClear( GL_COLOR_BUFFER_BIT )

end subroutine reshape
subroutine draw_hexagon() bind(C)

  use opengl_gl  
  use callbacks

  call glClear( GL_COLOR_BUFFER_BIT )

  call glCallList( Hexagon )
  
  call glFlush()
  
end subroutine draw_hexagon
