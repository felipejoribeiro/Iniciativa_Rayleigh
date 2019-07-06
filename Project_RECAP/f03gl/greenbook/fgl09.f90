module callbacks

  use opengl_gl

  integer(kind=GLint)  :: winWidth = 300, winHeight = 300

  type Point
    integer(kind=GLint) x        
    integer(kind=GLint) y        
  end type

  integer                    :: ip =   0  
  integer, parameter         :: NP = 100  
  type(Point), dimension(NP) :: Pt

end module callbacks
program fgl09

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
    
  Pt%x = 0
  Pt%y = 0

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl09 Mouse trails"//char(0))

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
  use callbacks

  call glClear(GL_COLOR_BUFFER_BIT)

  if( ip > 1 )then
    do i=2,ip
      call Drawline(Pt(i-1),Pt(i))    
    end do
  endif

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
subroutine drawline( p1, p2 )
  
  use OpenGL_GL
  use callbacks

  type(Point), intent(in)    :: p1, p2
  
  call glBegin(GL_LINES)
    call glVertex2i(p1%x,p1%y)
    call glVertex2i(p2%x,p2%y)
  call glEnd()
    
end subroutine drawline
subroutine mouse( ibutton, iaction, ix, iy ) bind(C)

  use OpenGL_GL
  use OpenGL_GLUT
  use callbacks

  integer(GLint), intent(in), value :: ibutton, iaction, ix, iy  

  if( ip == 0 )then
    if( ibutton == GLUT_LEFT_BUTTON .and. &
      iaction == GLUT_DOWN )then
      Pt(1)%x = ix
      Pt(1)%y = winHeight - iy
      ip = 1
    else
      if( ibutton == GLUT_RIGHT_BUTTON ) stop 'Done'
    endif
  else
    if( ibutton == GLUT_LEFT_BUTTON .and. &
      iaction == GLUT_DOWN )then
      ip = ip + 1
      Pt(ip)%x = ix
      Pt(ip)%y = winHeight - iy

      call Drawline(Pt(ip-1),Pt(ip))

    else
      if( ibutton == GLUT_RIGHT_BUTTON ) stop 'Done'
    endif
  endif

  call glFlush()
  
end subroutine mouse

