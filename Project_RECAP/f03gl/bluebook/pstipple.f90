!
! PStipple.f90
! OpenGL SuperBible
! Demonstrates OpenGL Polygon Stippling
! Original program by Richard S. Wright Jr.
!
program pstipple

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

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface

   real    :: xrot, yrot
   common  xrot, yrot

   xrot     =  0.0
   yrot     =  0.0
   
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Polygon Stippling'//char(0))
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop()

end program
subroutine SetupRC

   use OpenGL_GL

   ! Bitmap of camp fire
   integer(GLubyte), dimension(128) :: fire
   
   data fire / z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'00', &
	       z'00', z'00', z'00', z'c0', &
	       z'00', z'00', z'01', z'f0', &
	       z'00', z'00', z'07', z'f0', &
	       z'0f', z'00', z'1f', z'e0', &
	       z'1f', z'80', z'1f', z'c0', &
	       z'0f', z'c0', z'3f', z'80', &
	       z'07', z'e0', z'7e', z'00', &
	       z'03', z'f0', z'ff', z'80', &
	       z'03', z'f5', z'ff', z'e0', &
	       z'07', z'fd', z'ff', z'f8', &
	       z'1f', z'fc', z'ff', z'e8', &
	       z'ff', z'e3', z'bf', z'70', &
	       z'de', z'80', z'b7', z'00', &
	       z'71', z'10', z'4a', z'80', &
	       z'03', z'10', z'4e', z'40', &
	       z'02', z'88', z'8c', z'20', &
	       z'05', z'05', z'04', z'40', &
	       z'02', z'82', z'14', z'40', &
	       z'02', z'40', z'10', z'80', &
	       z'02', z'64', z'1a', z'80', &
	       z'00', z'92', z'29', z'00', &
	       z'00', z'b0', z'48', z'00', &
	       z'00', z'c8', z'90', z'00', &
	       z'00', z'85', z'10', z'00', &
	       z'00', z'03', z'00', z'00', &
	       z'00', z'00', z'10', z'00'  /

   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0 )

   ! Set drawing color to red
   call glColor3f(1.0, 0.0, 0.0)

   ! Enable polygon stippling
   call glEnable(GL_POLYGON_STIPPLE)
	
   ! Specify a specific stipple pattern
   call glPolygonStipple(fire)

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
   call glutPostRedisplay()

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
   call glLoadIdentity()

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
   call glLoadIdentity()

end subroutine ChangeSize
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   real    :: xrot, yrot
   common  xrot, yrot

   ! Clear the window and the depth buffer
   call glClear(GL_COLOR_BUFFER_BIT)

   ! Save matrix state and do the rotation
   call glPushMatrix()
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   ! Begin the stop sign shape,
   ! use a standard polygon for simplicity
   call glBegin(GL_POLYGON);
     call glVertex2f(-20.0,  50.0)
     call glVertex2f( 20.0,  50.0)
     call glVertex2f( 50.0,  20.0)
     call glVertex2f( 50.0, -20.0)
     call glVertex2f( 20.0, -50.0)
     call glVertex2f(-20.0, -50.0)
     call glVertex2f(-50.0, -20.0)
     call glVertex2f(-50.0,  20.0)
   call glEnd

   ! Restore transformations
   call glPopMatrix()

   call glutSwapBuffers()
  
end subroutine RenderScene
