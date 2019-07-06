!
! pointsz.f90
! OpenGL SuperBible
! Demonstrates OpenGL Primative GL_POINTS with point size
! Original program by Richard S. Wright Jr.
!
program pointsz

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

   real :: xrot, yrot
   common  xrot, yrot
   
   xrot =  0.0
   yrot =  0.0
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Points Size Example'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop

end program
subroutine SetupRC

   use OpenGL_GL
   
   ! Black background
   call	glClearColor(0.0, 0.0, 0.0, 1.0 )

   ! Set drawing color to green
   call	glColor3f(0.0, 1.0, 0.0)

end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop

   !write(*,*)'key>',key
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLint), intent(in), value  :: key, x, y

   real :: xrot, yrot
   common  xrot, yrot

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
subroutine RenderScene bind(C)

   use opengl_gl
   use opengl_glut
  
   real(kind=GLfloat), dimension(2) :: sizes =(/ 0.0, 0.0 /)
   real(kind=GLfloat), dimension(1) :: step
  
   real :: xrot, yrot
   common  xrot, yrot
 
   ! Clear the window with current clearing color
   call glClear(GL_COLOR_BUFFER_BIT)

   !Save matrix state and do the rotation
   call glPushMatrix
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   ! Get supported point size range and step size
   call glGetFloatv(GL_POINT_SIZE_RANGE,sizes)
   call glGetFloatv(GL_POINT_SIZE_GRANULARITY,step)

   ! Set the initial point size
   curSize = sizes(1)

   ! Set beginning z coordinate
   z = -50.0

   ! Loop around in a circle three times
   angle = 0.0
   do while( angle <= (2.0*3.1415)*3.0 )
     ! Calculate x and y values on the circle
     x = 50.0*sin(angle)
     y = 50.0*cos(angle)

     ! Specify the point size before the primative is specified
     call glPointSize(curSize)

     ! Draw the point
     call glBegin(GL_POINTS)
       call glVertex3f(x, y, z)
     call glEnd

     ! Bump up the z value and the point size
     z = z + 0.5
     curSize = curSize + step(1)
    
     angle = angle + 0.1
   end do
   
   ! Restore matrix state
   call glPopMatrix

   ! Flush drawing commands
   call glutSwapBuffers

end subroutine RenderScene
