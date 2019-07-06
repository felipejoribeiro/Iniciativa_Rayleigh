program fgl21

  use opengl_gl
  use opengl_glu
  use opengl_glut

  integer, parameter :: Pan = 1, Rotate =2, Zoom = 3

  real, dimension(3) :: trans = (/ 0.0, 0.0, 0.0 /)
  real, dimension(3) :: rot   = (/ 0.0, 0.0, 0.0 /)
  integer :: mode = Pan
  
  common /fgl21c/ mode, trans, rot
  
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

    subroutine mouse(b,a,x,y) bind(C)
      use opengl_gl
      integer(kind=GLint), intent(in), value  :: b,a,x,y
    end subroutine mouse
  end interface

  call glutInit 
  call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGBA,GLUT_DEPTH)))
  call glutInitWindowPosition(100,100)
  call glutInitWindowSize(500,500)
  iwin = glutCreateWindow('fgl21'//char(0))

  call glutMouseFunc(mouse)
  call glutKeyboardFunc(keyboard)
  call glutDisplayFunc(display)
  call glutReshapeFunc(reshape)

  call glEnable(GL_DEPTH_TEST)
  call glutMainLoop
		  
end program fgl21
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL

  integer(kind=GLbyte), intent(in), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
subroutine update( state, ox, nx, oy, ny )

  integer, parameter  :: Pan = 1, Rotate =2, Zoom = 3
  
  integer, intent(in) :: state, ox, nx, oy, ny
  integer :: dx, dy
  
  real, dimension(3) :: trans  
  real, dimension(3) :: rot   
  integer :: mode  
  
  common /fgl21c/ mode, trans, rot

  dx = ox - nx
  dy = ny - oy
  
  select case( state )
    case(Pan)
      trans(1) = trans(1) - float(dx)/100.
      trans(2) = trans(2) - float(dy)/100.
    
    case(Rotate)
      rot(1) = rot(1) + (float(dy)*180.)/500.
      rot(2) = rot(2) + (float(dx)*180.)/500.
    
      if( rot(1) >  360. ) rot(1) = rot(1) - 360.
      if( rot(1) < -360. ) rot(1) = rot(1) + 360.
      if( rot(2) >  360. ) rot(2) = rot(2) - 360.
      if( rot(2) < -360. ) rot(2) = rot(2) + 360.
    
    case(Zoom)
      trans(3) = trans(3) - float(dx+dy)/100.
    
  end select
  
end subroutine update
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl
  use opengl_glu

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight

  call glviewport(0,0,newWidth,newHeight)
  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity
  
  if( newHeight == 0 )then
    call gluPerspective (60d0, dble(newWidth), 0.001d0, 100.0d0 )
  else
    call gluPerspective (60d0, dble(float(newWidth)/float(newHeight)), &
                         0.001d0, 100.0d0 )
  endif

  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity
  call glTranslatef(0.0,0.0,-3.0)

end subroutine reshape
subroutine display() bind(C)

  use opengl_gl
  use opengl_glut

  real, dimension(3) :: trans  
  real, dimension(3) :: rot   
  integer :: mode  
  
  common /fgl21c/ mode, trans, rot

  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call glPushMatrix
  call glTranslatef(trans(1), trans(2), trans(3))
  call glRotatef(rot(1), 1.0, 0.0, 0.0)
  call glRotatef(rot(2), 0.0, 1.0, 0.0)
  call glBegin(GL_TRIANGLES)
 
  call glColor3f(1.0, 0.0, 0.0); call glVertex3i(0, 1, 0)   !top
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(-1, -1, 1) !fl
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(1, -1, 1)  !fr
 
  call glColor3f(1.0, 0.0, 0.0); call glVertex3i(0, 1, 0)   !top
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(1, -1, 1)  !fr
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(1, -1, -1) !br
 
  call glColor3f(1.0, 0.0, 0.0); call glVertex3i(0, 1, 0)   !top
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(1, -1, -1) !br
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(-1, -1, -1)!bl
 
  call glColor3f(1.0, 0.0, 0.0); call glVertex3i(0, 1, 0)   !top
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(-1, -1, -1)!bl
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(-1, -1, 1) !fl

  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(1, -1, 1)  !fr
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(-1, -1, 1) !fl
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(-1, -1, -1)!bl
  
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(-1, -1, -1)!bl
  call glColor3f(0.0, 0.0, 1.0); call glVertex3i(1, -1, -1) !br
  call glColor3f(0.0, 1.0, 0.0); call glVertex3i(1, -1, 1)  !fr

  call glend
  call glPopMatrix
  call glFlush
  call glutSwapBuffers

end subroutine display
subroutine mouse(button,state,ix,iy) bind(C)

  use opengl_gl
  use opengl_glut
  
  integer(kind=GLint), intent(in), value :: button,state,ix,iy

  integer, parameter  :: Pan = 1, Rotate =2, Zoom = 3

  real, dimension(3) :: trans  
  real, dimension(3) :: rot   
  integer :: mode  
  
  common /fgl21c/ mode, trans, rot

  integer, save :: ox = 0, oy = 0

  if( button == GLUT_LEFT_BUTTON .and. &
      state  == GLUT_DOWN )then
    
    call update(Pan, ox, ix, oy, iy )

    write(*,*)'p',ox, ix, oy, iy 
    ox = ix
    oy = iy
    
    call glutPostRedisplay
    
  elseif( button == GLUT_RIGHT_BUTTON .and. &
  	  state  == GLUT_DOWN )then
        
    call update(Rotate, ox, ix, oy, iy )

    write(*,*)'r',ox, ix, oy, iy 
    ox = ix
    oy = iy
   
    call glutPostRedisplay

  endif
 
end subroutine mouse 
 
