program fgl15

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
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl15 Example 4-1"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)
  call glShadeModel(GL_SMOOTH)

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
      
  call glClear(GL_COLOR_BUFFER_BIT)

  call glBegin(GL_TRIANGLES)
  call glColor3f(1.0,0.0,0.0) 
  call glVertex2f(5.0,5.0)       ! real == gfloat!
  call glColor3f(0.0,1.0,0.0) 
  call glVertex2f(25.0,5.0)      ! real == gfloat!
  call glColor3f(0.0,0.0,1.0) 
  call glVertex2f(5.0,25.0)      ! real == gfloat!

  call glend()
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
    call gluOrtho2D(zero,30.0_gldouble,zero,30.0/Aspect)
  else
    call gluOrtho2D(zero,30.0*Aspect,zero,30.0_gldouble)  
  endif
  call glMatrixMode(GL_MODELVIEW)

end subroutine reshape
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL
  use OpenGL_GLut

  integer(kind=GLbyte), intent(IN), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
