!
! Triangle.f90
! OpenGL SuperBible
! Demonstrates OpenGL Triangle Fans, backface culling, and depth testing
! Original program by Richard S. Wright Jr.
!
program triangle

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

   real    :: xrot, yrot
   integer :: icull, ioutline, idepth
   common  xrot, yrot, icull, ioutline, idepth

   xrot     =  0.0
   yrot     =  0.0
   icull    =   0
   ioutline =   0
   idepth   =   0
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Triangle Culling Example'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Toggle depth test'//char(0),1)
   call glutAddMenuEntry('Toggle cull backface'//char(0),2)
   call glutAddMenuEntry('Toggle outline back'//char(0),3)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop()

end program
subroutine SetupRC

   use OpenGL_GL

   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0 )

   ! Set drawing color to green
   call glColor3f(0.0, 1.0, 0.0)

   ! Set color shading model to flat
   call glShadeModel(GL_FLAT)

   ! Clock wise wound polygons are front facing, this is reversed
   ! because we are using triangle fans
   call glFrontFace(GL_CW)

end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop

end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLint), intent(in), value  :: key, x, y

   real    :: xrot, yrot
   integer :: icull, ioutline, idepth
   common  xrot, yrot, icull, ioutline, idepth

   if( key == GLUT_KEY_UP)    xRot = xrot - 5.0
   if( key == GLUT_KEY_DOWN)  xRot = xrot + 5.0
   if( key == GLUT_KEY_LEFT)  yRot = yrot - 5.0
   if( key == GLUT_KEY_RIGHT) yRot = yrot + 5.0

   if( xrot > 356.0 )then
     xRot = 0.0
   else if( xrot < -1.0 )then
     xRot = 355.0
   endif

   if( yrot > 356.0 )then
     yRot = 0.0
   else if( yrot < -1.0 )then
     yRot = 355.0
   endif

   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine ChangeSize(win, hin) bind(C)

   use OpenGL_GL

   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, Range = 100.0, Aspect

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
   aspect = float(w)/float(h)

   ! Keep the square square
   if( w <= h )then
     call glOrtho( -Range, Range, &
                   -Range/Aspect, Range/Aspect,&
                   -Range, Range  )
   else
     call glOrtho( -Range*Aspect, Range*Aspect, &
                   -Range, Range, &
                   -Range, Range  )
   endif

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   real    :: xrot, yrot
   integer :: icull, ioutline, idepth
   common  xrot, yrot, icull, ioutline, idepth

   ! Clear the window and the depth buffer
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Turn culling on if flag is set
   if( iCull == 1 )then
     call glEnable(GL_CULL_FACE)
   else
     call glDisable(GL_CULL_FACE)
   endif

   ! Enable depth testing if flag is set
   if( iDepth == 1 )then
     call glEnable(GL_DEPTH_TEST)
   else
     call glDisable(GL_DEPTH_TEST)
   endif

   ! Draw back side as a polygon only, if flag is set
   if( iOutline == 1)then
     call glPolygonMode(GL_BACK,GL_LINE)
   else
     call glPolygonMode(GL_BACK,GL_FILL)
   endif
		
   ! Save matrix state and do the rotation
   call glPushMatrix
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   ! Begin a triangle fan
   call glBegin(GL_TRIANGLE_FAN)

   ! Pinnacle of cone is shared vertex for fan, moved up Z axis
   ! to produce a cone instead of a circle
   call glVertex3f(0.0, 0.0, 75.0)
	
   ! Loop around in a circle and specify even points along the circle
   ! as the vertices of the triangle fan
   ipivot = 0
   angle  = 0.0
   do while( angle <= (2.0*3.1415)*3.0 )
     ! Calculate x and y position of the next vertex
     x = 50.0*sin(angle)
     y = 50.0*cos(angle)
	
     ! Alternate color between red and green
     if( mod(iPivot,2) == 0 )then
       call glColor3f(0.0, 1.0, 0.0)
     else
       call glColor3f(1.0, 0.0, 0.0)
     endif
    
     ! Increment pivot to change color next time
     iPivot = ipivot + 1

     ! Specify the next vertex for the triangle fan
     call glVertex2f(x, y)
		
     angle = angle + 3.1415/8.0
   end do

   ! Done drawing fan for cone
   call glEnd

   ! Begin a new triangle fan to cover the bottom
   call glBegin(GL_TRIANGLE_FAN)

   ! Center of fan is at the origin
   call glVertex2f(0.0, 0.0)

   ipivot = 0
   angle  = 0.0
   do while( angle <= (2.0*3.1415)*3.0 )
     ! Calculate x and y position of the next vertex
     x = 50.0*sin(angle)
     y = 50.0*cos(angle)
	
     ! Alternate color between red and green
     if( mod(iPivot,2) == 0 )then
       call glColor3f(0.0, 1.0, 0.0)
     else
       call glColor3f(1.0, 0.0, 0.0)
     endif
    
     ! Increment pivot to change color next time
     iPivot = ipivot + 1

     ! Specify the next vertex for the triangle fan
     call glVertex2f(x, y)
		
     angle = angle + 3.1415/8.0
   end do

   ! Done drawing the fan that covers the bottom
   call glEnd

   ! Restore transformations
   call glPopMatrix

   call glutSwapBuffers
  
end subroutine RenderScene
subroutine ProcessMenu(i) bind(C)
	
   use opengl_glut

   integer, intent(in), value :: i

   real    :: xrot, yrot
   integer :: icull, ioutline, idepth
   common  xrot, yrot, icull, ioutline, idepth

   select case(i)
      case(1)
	iDepth = abs(iDepth-1)
	
      case(2)
	iCull = abs(iCull-1)

      case(3)
        iOutline = abs(iOutline-1)

   end select

   call glutPostRedisplay
	
end subroutine ProcessMenu








