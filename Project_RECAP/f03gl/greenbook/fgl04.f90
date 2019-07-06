program fgl04

  use opengl_gl
  use opengl_glu
  use opengl_glut
  
  interface 
    subroutine graph() bind(C)
    end subroutine graph

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface 

  integer(kind=GLint) :: winWidth = 600, winHeight = 400
  integer(kind=GLint) :: iwin
  
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl04 Line chart data"//char(0))

  call glClearColor(1.0,1.0,1.0,1.0)
  call glMatrixMode(GL_PROJECTION)
  call gluOrtho2D(0.0_gldouble, 600.0_gldouble, &
                  0.0_gldouble, 400.0_gldouble  )
  
  call glutDisplayFunc( graph )
  call glutReshapeFunc( reshape )

  call glutMainLoop()
  
end program
subroutine graph() bind(C)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  character, dimension(36) :: label =  (/ &
  'J','a','n', 'F','e','b', 'M','a','r', 'A','p','r', &
  'M','a','y', 'J','u','n', 'J','u','l', 'A','u','g', &
  'S','e','p', 'O','k','t', 'N','o','v', 'D','e','c'  /)

  integer(kind=glint) :: xRaster = 25, yRaster =50
  integer(kind=glint) :: month, k, x = 30   
  
  integer(kind=glint), dimension(12) :: data = (/ &
                    420, 342, 324, 310, 262, 185, &
                    190, 196, 217, 240, 312, 438  /)
  
  call glClear(GL_COLOR_BUFFER_BIT)
  
  ! x1, y1 Specify one vertex of a rectangle. 
  ! x2, y2 Specify the opposite vertex of the rectangle
  call glColor3f(1.0,0.0,0.0)
  do i=1,12
    call glRecti(20+(i-1)*50,65,40+(i-1)*50,data(i)-100)
  end do
  
  call glColor3f(0.0,0.0,1.0)
  call glBegin(GL_LINE_STRIP)  
  do i=1,12
    call glVertex2i(x+(i-1)*50,data(i)-100)
  end do
  call GLend()

  call glColor3f(1.0,1.0,0.0)
  xRaster = 25
  do i=1,12
    call glRasterPos2i(xRaster+(i-1)*50,data(i)-4-100)
    call glutBitmapCharacter(GLUT_BITMAP_9_BY_15,iachar('*'))
  end do
  
  call glColor3f(0.0,0.0,0.0)
  xRaster = 20
  do i=1,12
    call glRasterPos2i(xraster,yraster)
    do j=3*(i-1)+1,3*(i-1)+3
      call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12,iachar(label(j)))
    end do
    xraster=xraster+50
  end do
  
  call glFlush()
  
end subroutine graph
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

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
  
  
  

