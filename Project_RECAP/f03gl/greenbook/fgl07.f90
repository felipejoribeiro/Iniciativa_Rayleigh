program fgl07

  use opengl_gl
  use opengl_glu
  use opengl_glut
    
  interface 
    subroutine quadrics() bind(C)
    end subroutine quadrics

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface 

  integer(kind=GLint) :: winWidth = 300, winHeight = 300
  integer(kind=GLint) :: iwin
    
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl07 Quadric Surfaces"//char(0))

  call glClearColor(1.0,1.0,1.0,0.0)
  
  call glutDisplayFunc( quadrics )
  call glutReshapeFunc( reshape )

  call glutMainLoop()
  
end program
subroutine quadrics() bind(C)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  type(C_PTR) :: ptr = c_null_ptr 

  call glClear(GL_COLOR_BUFFER_BIT)
  
  call glLoadIdentity()

  call glColor3f(0.0,0.0,1.0)

  call gluLookAt( 2.0_gldouble, 2.0_gldouble, 2.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 1.0_gldouble  )

  call glPushMatrix()
  call glTranslatef( 1.0, 1.0, 0.0 )
  call glutWireSphere( 0.75_gldouble, 8, 6 ) 
  call glPopMatrix()

  call glPushMatrix()
  call glTranslatef( 1.0, -0.5, 0.5 )
  call glutWireCone( 0.7_gldouble, 2.0_gldouble, 7, 6 ) 
  call glPopMatrix()

  call glPushMatrix()
  call glTranslatef( 0.0, 1.2, 0.8 )

  ptr = gluNewQuadric()
  call gluQuadricDrawStyle(ptr,GLU_LINE)
  call gluCylinder(ptr,0.6_gldouble,0.6_gldouble,1.5_gldouble,6,4)

  call glPopMatrix()

  call glFlush()

end subroutine quadrics
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  
  call glLoadIdentity()

  call glOrtho( -2.0_gldouble,  2.0_gldouble, &
                -2.0_gldouble,  2.0_gldouble, &
                 0.0_gldouble,  5.0_gldouble  )
  
  call glMatrixMode(GL_MODELVIEW)
  
  call glClear(GL_COLOR_BUFFER_BIT)
  
end subroutine reshape
  
  
  
  
  
  
  
