module callbacks

  use opengl_gl

  integer(kind=GLint)  :: winWidth = 300, winHeight = 300

end module callbacks
program fgl08

  use opengl_gl
  use opengl_glu
  use opengl_glut
    
  use callbacks
  
  interface 
    subroutine display() bind(C)
    end subroutine display

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape

    subroutine mouse(b,a,x,y) bind(C)
      use OpenGL_GL
      integer(GLint), intent(in), value :: b,a,x,y
    end subroutine mouse
  end interface 

  integer(kind=GLint) :: iwin
    
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl08 Mouse droppings"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)

  call glMatrixMode(GL_PROJECTION)

  call gluOrtho2D( 0.0_gldouble,  150.0_gldouble, &
                   0.0_gldouble,  150.0_gldouble  )
  
  call glutMouseFunc( mouse )
  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )

  call glutMainLoop()
  
end program
subroutine display() bind(C)

  use OpenGL_GL

  call glClear(GL_COLOR_BUFFER_BIT)

  call glColor3f(1.0,0.0,0.0)

  call glPointSize(3.0)
  call glFlush()

end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

  use callbacks

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, Width, Height

  Zero   = 0.0
  Width  = newWidth
  Height = newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call gluOrtho2D( Zero, Width, Zero, Height )
  
  call glClear(GL_COLOR_BUFFER_BIT)

  winWidth  = newWidth
  winHeight = newHeight
  
end subroutine reshape
subroutine plotpoint(ix,iy)
  
  use OpenGL_GL

  integer(GLint), intent(in) :: ix, iy  
  
  call glBegin(GL_POINTS)
  call glVertex2i(ix,iy)
  call glEnd()
  
end subroutine plotpoint
subroutine mouse( ibutton, iaction, ix, iy ) bind(C)

  use OpenGL_GL
  use OpenGL_GLUT
  use callbacks

  integer(GLint), intent(in), value :: ibutton, iaction, ix, iy  

  if( ibutton == GLUT_LEFT_BUTTON .and. &
      iaction == GLUT_DOWN )then
      
    call plotpoint(ix,winHeight-iy)
  
    call glFlush()
    
  endif
  
end subroutine mouse

