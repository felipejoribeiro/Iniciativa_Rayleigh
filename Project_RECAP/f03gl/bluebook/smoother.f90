!
! Smoother.cpp
! OpenGL SuperBible
! Demonstrates point, line, and polygon smoothing
! Original program by Richard S. Wright Jr.
!
module smooth

   integer, parameter :: SMALL_STARS  =  40
   integer, parameter :: MEDIUM_STARS =  20
   integer, parameter :: LARGE_STARS  =  10

   integer, parameter :: SCREEN_X =  800
   integer, parameter :: SCREEN_Y =  600

   real, dimension(SMALL_STARS,2)  :: vSmallStars
   real, dimension(MEDIUM_STARS,2) :: vMediumStars
   real, dimension(LARGE_STARS,2)  :: vLargeStars

end module smooth
program smoother

   use opengl_gl
   use opengl_glut

   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ProcessMenu(i) bind(C)
       integer, intent(in), value :: i
     end subroutine ProcessMenu

     subroutine ChangeSize(w,h) bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface
   
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Smoothing Out The Jaggies'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Antialiased Rendering'//char(0),1)
   call glutAddMenuEntry('Normal Rendering'//char(0),2)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop()

end program
subroutine ProcessMenu(i) bind(C)

   use opengl_gl 
   use opengl_glut

   integer, intent(in), value :: i

   select case(i)
     case(1)
       ! Turn on antialiasing, and give hint to do the best
       ! job possible.
       call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
       call glEnable(GL_BLEND)
       call glEnable(GL_POINT_SMOOTH)
       call glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
       call glEnable(GL_LINE_SMOOTH)
       call glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
       call glEnable(GL_POLYGON_SMOOTH)
       call glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST)

     case(2)
       ! Turn off blending and all smoothing
       call glDisable(GL_BLEND)
       call glDisable(GL_LINE_SMOOTH)
       call glDisable(GL_POINT_SMOOTH)
       call glDisable(GL_POLYGON_SMOOTH)

   end select

   ! Trigger a redraw
   call glutPostRedisplay
    
end subroutine ProcessMenu
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use smooth
   
   real(kind=GLfloat) :: x =   700.0   ! Location and radius of moon
   real(kind=GLfloat) :: y =   500.0
   real(kind=GLfloat) :: r =    50.0
   real(kind=GLfloat) :: angle = 0.0   ! Another looping variable
                       
   ! Clear the window
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
        
   ! Everything is white
   call glColor3f(1.0, 1.0, 1.0)
   
   ! Draw small stars
   call glPointSize(1.0)
   call glBegin(GL_POINTS)
   do i=1,SMALL_STARS
     call glVertex2fv(vSmallStars(i,1:2))
   end do
   call glEnd
        
   ! Draw medium sized stars
   call glPointSize(3.05)
   call glBegin(GL_POINTS)
   do i=1,MEDIUM_STARS 
     call glVertex2fv(vMediumStars(i,1:2))
   end do
   call glEnd
        
   ! Draw largest stars
   call glPointSize(5.5)
   call glBegin(GL_POINTS)
   do i=1,LARGE_STARS
     call glVertex2fv(vLargeStars(i,1:2))
   end do
   call glEnd
        
   ! Draw the "moon"
   call glBegin(GL_TRIANGLE_FAN)
   call glVertex2f(x, y)
   angle = 0.0
   da    = 6.2831853071795862/31.
   
   do i=0,31
     call glVertex2f(x + cos(angle) * r, y + sin(angle) * r)
     call glVertex2f(x + cos(angle+da) * r, y + sin(angle+da) * r)
     angle = angle + da
   end do
   call glEnd

   call glBegin(GL_TRIANGLE_FAN)
   call glVertex2f(x, y)
   angle = 0.0 + 0.5 * da 
   
   do i=0,31
     call glVertex2f(x + cos(angle) * r, y + sin(angle) * r)
     call glVertex2f(x + cos(angle+da) * r, y + sin(angle+da) * r)
     angle = angle + da
   end do
   call glEnd

   ! Draw distant horizon
   call glLineWidth(3.5)
   call glBegin(GL_LINE_STRIP)
     call glVertex2f(  0.0,  25.0)
     call glVertex2f( 50.0, 100.0)
     call glVertex2f(100.0,  25.0)
     call glVertex2f(225.0, 125.0)
     call glVertex2f(300.0,  50.0)
     call glVertex2f(375.0, 100.0)
     call glVertex2f(460.0,  25.0)
     call glVertex2f(525.0, 100.0)
     call glVertex2f(600.0,  20.0)
     call glVertex2f(675.0,  70.0)
     call glVertex2f(750.0,  25.0)
     call glVertex2f(800.0,  90.0)    
   call glEnd

   ! Swap buffers
   call glutSwapBuffers

end subroutine RenderScene
subroutine SetupRC

   use OpenGL_GL

   use smooth
   
   ! Populate star list
   do i=1,SMALL_STARS 
     call random_number(r)
     vSmallStars(i,1) = r * float(SCREEN_X)
     call random_number(r)
     vSmallStars(i,2) = r * float((SCREEN_Y - 125)) + 125.0 
   end do
           
   ! Populate star list
   do i=1,MEDIUM_STARS 
     call random_number(r)
     vMediumStars(i,1) = r * float(SCREEN_X)
     call random_number(r)
     vMediumStars(i,2) = r * float((SCREEN_Y - 125)) + 125.0 
   end do

   ! Populate star list
   do i=1,LARGE_STARS 
     call random_number(r)
     vLargeStars(i,1)  = r * float(SCREEN_X)
     call random_number(r)
     vLargeStars(i,2)  = r * float((SCREEN_Y - 125)) + 125.0 
   end do
                      
   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0)

   ! Set drawing color to white
   call glColor3f(1.0, 1.0, 1.0)
    
end subroutine SetupRC
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu

   use smooth
    
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, Aspect

   w = win
   h = hin
   
   if( h == 0 ) h = 1
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluOrtho2D(dble(0.0), dble(SCREEN_X), dble(0.0), dble(SCREEN_Y))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc

 
