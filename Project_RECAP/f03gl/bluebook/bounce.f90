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
     subroutine RenderScene()
     end subroutine RenderScene

     subroutine ChangeSize(w,h)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     subroutine TimerFunction(i)
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
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGBA,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Bounce'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1);

   call SetupRC

   call glutMainLoop

end program
subroutine KeyPressFunc(key, x, y)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop
   
end subroutine KeyPressFunc
subroutine RenderScene

   use opengl_gl
   use opengl_glut
  
   real :: x, y, rsize
   common  x, y, rsize

   ! Clear the window with current clearing color
   call glClear(GL_COLOR_BUFFER_BIT)

   ! Set current drawing color to red
   call glColor3f(1.0, 0.0, 0.0)

   ! Draw a filled rectangle with current color
   call glRectf(x, y, x + rsize, y - rsize)

   ! Flush drawing commands and swap
   call glutSwapBuffers()

end subroutine RenderScene
subroutine TimerFunction( ivalue )

   use opengl_glut

   integer, intent(in), value :: ivalue

   real :: x, y, rsize, xstep, ystep, windowWidth, windowHeight
   common  x, y, rsize, xstep, ystep, windowWidth, windowHeight

   ! Reverse direction when you reach left or right edge
   if( x > windowWidth-rsize .or. x < -windowWidth ) xstep = -xstep

   ! Reverse direction when you reach top or bottom edge
   if( y > windowHeight .or. y < -windowHeight + rsize) ystep = -ystep

   ! Actually move the square
   x = x + xstep
   y = y + ystep

   ! Check bounds. This is in case the window is made
   ! smaller while the rectangle is bouncing and the 
   ! rectangle suddenly finds itself outside the new
   ! clipping volume
   if( x > (windowWidth-rsize + xstep) )then
     x = windowWidth-rsize-1
   else if( x < -(windowWidth + xstep) )then
     x = -windowWidth -1
   endif
   
   if( y > (windowHeight + ystep) )then
     y = windowHeight-1 
   else if( y < -(windowHeight - rsize + ystep) )then
     y = -windowHeight + rsize - 1
   endif

   ! Redraw the scene with new coordinates
   call glutPostRedisplay()
   call glutTimerFunc(33,TimerFunction, 1)

end subroutine TimerFunction
subroutine SetupRC

   use opengl_gl

   ! Set clear color to blue
   call glClearColor(0.0, 0.0, 1.0, 1.0)

end subroutine setuprc
subroutine ChangeSize(win, hin)
   
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
