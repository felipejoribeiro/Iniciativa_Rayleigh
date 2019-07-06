program fgl06

  use opengl_gl
  use opengl_glu
  use opengl_glut
    
  interface 
    subroutine polyhedra() bind(C)
    end subroutine polyhedra

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface 

  integer(kind=GLint)  :: winWidth = 300, winHeight = 300
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
  iwin = glutCreateWindow("fgl06 GLUT Polyhedra"//char(0))

  call glClearColor(1.0,1.0,1.0,0.0)
  
  call glutDisplayFunc( polyhedra )
  call glutReshapeFunc( reshape )

  call glutMainLoop()
  
end program
subroutine polyhedra() bind(C)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  call glClear(GL_COLOR_BUFFER_BIT)

  call glLoadIdentity()
  
  call glColor3f(0.0,0.0,1.0)

  call gluLookAt( 5.0_gldouble, 5.0_gldouble, 5.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                  0.0_gldouble, 1.0_gldouble, 0.0_gldouble  )

  call glScalef( 1.5, 2.0, 1.0 )
  call glutWirecube( 1.0_gldouble ) 

  call glColor3f(0.0,1.0,1.0)

  call glScalef( 0.8, 0.5, 0.8 )
  call glTranslatef( -6.0, -5.0, 0.0 )
  call glutWireDodecahedron( ) 

  call glColor3f(1.0,0.0,1.0)

  call glTranslatef( 8.6, 8.6, 2.0 )
  call glScalef( 0.75, 0.75, 0.75 )
  call glutWireTetrahedron( ) 
  call glScalef( 1.333, 1.333, 1.333 )

  call glColor3f(1.0,0.0,0.0)

  call glTranslatef( -3.0, -1.0, 0.0 )
  call glutWireOctahedron( ) 

  call glColor3f(0.0,0.0,0.0)

  call glScalef( 0.8, 0.8, 1.0 )
  call glTranslatef( 4.3, -2.0, 0.5 )
  call glutWireIcosahedron( ) 
  
  call glFlush()

end subroutine polyhedra
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  
  call glLoadIdentity()

  call glFrustum( -1.0_gldouble,  1.0_gldouble, &
                  -1.0_gldouble,  1.0_gldouble, &
                   2.0_gldouble, 20.0_gldouble  )
  
  call glMatrixMode(GL_MODELVIEW)
  
  call glClear(GL_COLOR_BUFFER_BIT)
  
end subroutine reshape
  
  
  
  
  
  
  
