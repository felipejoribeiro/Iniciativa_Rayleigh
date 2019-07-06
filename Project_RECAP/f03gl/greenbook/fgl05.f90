module callbacks

  use opengl_gl

  integer(kind=GLint)  :: winWidth = 200, winHeight = 200

end module callbacks
program fgl05

  use opengl_gl
  use opengl_glu
  use opengl_glut
  use callbacks
    
  interface 
    subroutine square() bind(C)
    end subroutine square

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface 

  integer(kind=GLint) :: iwin

  real(kind=GLdouble) :: x0 = 100.0, y0 = 50.0, z0 = 50.0 ! viewing coor. origin
  real(kind=GLdouble) :: xr =  50.0, yr = 50.0, zr =  0.0 ! look at this point
  real(kind=GLdouble) :: Vx =   0.0, Vy =  1.0, Vz =  0.0 ! view-up vector
  real(kind=GLdouble) :: xwmin = -40.0, ywmin = -60.0, &
                         xwmax =  40.0, ywmax =  60.0, &
                         dnear =  25.0, dfar  = 125.0
    
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl05 Perspective view of a square"//char(0))

  call glClearColor(1.0,1.0,1.0,0.0)
  call glMatrixMode(GL_MODELVIEW)
  call gluLookAt(x0,y0,z0, xr,yr,zr, vx,vy,vz)

  call glMatrixMode(GL_PROJECTION)
  call glFrustum(xwmin,xwmax, ywmin,ywmax, dnear,dfar)
  
  call glutDisplayFunc( square )
  call glutReshapeFunc( reshape )

  call glutMainLoop()
  
end program
subroutine square() bind(C)

  use opengl_gl

  call glClear(GL_COLOR_BUFFER_BIT)
  
  call glColor3f(0.0,1.0,0.0)
  call glPolygonMode(GL_FRONT, GL_FILL)
  call glPolygonMode(GL_BACK, GL_LINE)  ! back face wire frame
  call glBegin(GL_QUADS)
    call glVertex3f(  0.0,   0.0, 0.0)
    call glVertex3f(100.0,   0.0, 0.0)
    call glVertex3f(100.0, 100.0, 0.0)
    call glVertex3f(  0.0, 100.0, 0.0)
  call glEnd()
  
  call glFlush()

end subroutine square
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl
  use callbacks

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight

  call glviewport(0,0,newWidth,newHeight)
  
  winWidth  = newWidth
  winHeight = newHeight

end subroutine reshape
  
  
  
  
  
  
  
