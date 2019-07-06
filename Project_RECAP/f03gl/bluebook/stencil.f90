!
! bounce.f90
! Demonstrates a simple animated rectangle program with GLUT
! OpenGL SuperBible, 3rd Edition
! Richard S. Wright Jr.
! rwright@starstonesoftware.com
!
program bounce

   use opengl_gl
   use opengl_glut

   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ChangeSize(w,h) bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

   end interface

   real :: x, y, rsize, xstep, ystep, winw, winh
   common  x, y, rsize, xstep, ystep, winw, winh
   
   x     =  0.0
   y     =  0.0
   rsize = 25.0
   xstep =  1.0
   ystep =  1.0
   
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGBA,GLUT_STENCIL)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('OpenGL Stencil Test'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1);

   call glutMainLoop()

end program
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop
   
end subroutine KeyPressFunc
subroutine RenderScene bind(C)

   use opengl_gl
   use opengl_glut
  
   real :: x, y, rsize
   common  x, y, rsize

   ! Clear blue window
   call glClearColor(0.0, 0.0, 1.0, 0.0)
       
   ! Use 0 for clear stencil, enable stencil test
   call glClearStencil(0)
   call glEnable(GL_STENCIL_TEST)

   ! Clear color and stencil buffer
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_STENCIL_BUFFER_BIT))
   	       
   ! All drawing commands fail the stencil test, and are not
   ! drawn, but increment the value in the stencil buffer. 
   call glStencilFunc(GL_NEVER, z'00', z'00')
   call glStencilOp(GL_INCR, GL_INCR, GL_INCR)

   ! Spiral pattern will create stencil pattern
   ! Draw the spiral pattern with white lines. We 
   ! make the lines  white to demonstrate that the 
   ! stencil function prevents them from being drawn
   call glColor3f(1.0, 1.0, 1.0)
   call glBegin(GL_LINE_STRIP)
   
   radius = 0.1
   angle  = 0.0
   do while( angle < 400 )
     call glVertex2f(radius*cos(angle),radius*sin(angle))
     radius = radius*1.002
     angle  = angle + 0.1
   end do
   call glEnd
   	   
   ! Now, allow drawing, except where the stencil pattern is 0x1
   ! and do not make any further changes to the stencil buffer
   call glStencilFunc(GL_NOTEQUAL, z'01', z'01')
   call glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP)
       
   ! Now draw red bouncing square
   ! (x and y) are modified by a timer function
   call glColor3f(1.0, 0.0, 0.0)
   call glRectf(x, y, x + rsize, y - rsize)
   
   ! All done, do the buffer swap
   call glutSwapBuffers

end subroutine RenderScene
recursive subroutine TimerFunction( ivalue ) bind(C)

   use opengl_glut

   integer, intent(in), value :: ivalue

   real :: x, y, rsize, xstep, ystep, windowWidth, windowHeight
   common  x, y, rsize, xstep, ystep, windowWidth, windowHeight

   ! Reverse direction when you reach left or right edge
   if( x > windowWidth-rsize .or. x < -windowWidth ) xstep = -xstep

   ! Reverse direction when you reach top or bottom edge
   if( y > windowHeight .or. y < -windowHeight + rsize) ystep = -ystep

   ! Check bounds. This is in case the window is made
   ! smaller while the rectangle is bouncing and the 
   ! rectangle suddenly finds itself outside the new
   ! clipping volume
   if( x > (windowWidth-rsize) ) x = windowWidth-rsize-1
   
   if( y > windowHeight ) y = windowHeight-1 

   ! Actually move the square
   x = x + xstep
   y = y + ystep

   ! Redraw the scene with new coordinates
   call glutPostRedisplay()
   call glutTimerFunc(33,TimerFunction, 1)

end subroutine TimerFunction
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
 
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, windowWidth, windowHeight, Aspect

   real :: x, y, rsize, xstep, ystep, winw, winh
   common  x, y, rsize, xstep, ystep, winw, winh

   w = win
   h = hin
   
   ! Prevent a divide by zero, when window is too short
   ! (you cant make a window of zero width).
   if( h == 0 ) h = 1

   ! Set the viewport to be the entire window
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspectRatio = float(w)/float(h)

   ! Keep the square square
   if( w <= h )then
     windowWidth  = 100.0
     windowHeight = 100.0/aspectRatio
     ! Set the clipping volume
     call glOrtho( dble(-100.0), dble(100.0), &
                   -windowHeight, windowHeight,&
                   dble(1.0), dble(-1.0)      )
   else
     windowWidth  = 100.0*aspectRatio
     windowHeight = 100.0
     ! Set the clipping volume
     call glOrtho( -windowWidth, windowWidth,&
                   dble(-100.0), dble(100.0), &
		   dble(1.0), dble(-1.0)      )
   endif

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

   winw = windowWidth
   winh = windowHeight

end subroutine ChangeSize 
