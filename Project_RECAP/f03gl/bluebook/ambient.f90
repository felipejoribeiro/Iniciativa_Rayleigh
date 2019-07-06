!
! Ambient.f90
! OpenGL SuperBible
! Beginning of OpenGL lighting sample
! Demonstrates Ambient Lighting
! Original program by Richard S. Wright Jr.
!
program ambient

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
   common  xrot, yrot

   xrot     =  0.0
   yrot     =  0.0
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Ambient Light Jet'//char(0))
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop

end program
subroutine SetupRC

   use OpenGL_GL

   ! Light values
   ! Bright white light
   real(kind=GLfloat), dimension(4) :: ambientLight = (/ 1., 1., 1., 1. /)

   call glEnable(GL_DEPTH_TEST)  ! Hidden surface removal
   call glEnable(GL_CULL_FACE)   ! Do not calculate inside of jet
   call glFrontFace(GL_CCW)      ! Counter clock-wise polygons face out

   ! Lighting stuff
   call glEnable(GL_LIGHTING)    ! Enable lighting      

   ! Set light model to use ambient light specified by ambientLight
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambientLight);

   call glEnable(GL_COLOR_MATERIAL) ! Enable Material color tracking

   ! Front material ambient and diffuse colors track glColor
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

   ! Nice light blue
   call glClearColor(0.0,0.0,0.5,1.0)

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
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   integer(kind=GLubyte), parameter :: i000 = z'00'
   integer(kind=GLubyte), parameter :: i064 = z'40'
   integer(kind=GLubyte), parameter :: i192 = z'C0'
   integer(kind=GLubyte), parameter :: i255 = z'FF'

   real    :: xrot, yrot
   common  xrot, yrot

   ! Clear the window and the depth buffer
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the matrix state
   call glPushMatrix()
     call glRotatef(xRot, 1.0, 0.0, 0.0)
     call glRotatef(yRot, 0.0, 1.0, 0.0)

     ! Nose Cone /////////////////////////////
     ! Bright Green
     call glColor3ub(i000, i255, i000)
     call glBegin(GL_TRIANGLES)
      call glVertex3f(  0.0,  0.0, 60.0)
      call glVertex3f(-15.0,  0.0, 30.0)
      call glVertex3f( 15.0,  0.0, 30.0)

      call glVertex3f( 15.0,  0.0, 30.0)
      call glVertex3f(  0.0, 15.0, 30.0)
      call glVertex3f(  0.0,  0.0, 60.0)

      call glVertex3f(  0.0,  0.0, 60.0)
      call glVertex3f(  0.0, 15.0, 30.0)
      call glVertex3f(-15.0,  0.0, 30.0)

      ! Body of the Plane ////////////////////////
      ! light gray
      call glColor3ub(i192,i192,i192)
      call glVertex3f(-15.0,  0.0,  30.0)
      call glVertex3f(  0.0, 15.0,  30.0)
      call glVertex3f(  0.0,  0.0, -56.0)

      call glVertex3f(  0.0,  0.0, -56.0)
      call glVertex3f(  0.0, 15.0,  30.0)
      call glVertex3f( 15.0,  0.0,  30.0)       

      call glVertex3f( 15.0,  0.0,  30.0)
      call glVertex3f(-15.0,  0.0,  30.0)
      call glVertex3f(  0.0,  0.0, -56.0)

      !//////////////////////////////////////////////
      ! Left wing
      ! Dark gray
      call glColor3ub(i064,i064,i064)
      call glVertex3f(  0.0, 2.0, 27.0)
      call glVertex3f(-60.0, 2.0, -8.0)
      call glVertex3f( 60.0, 2.0, -8.0)

      call glVertex3f( 60.0, 2.0, -8.0)
      call glVertex3f(  0.0, 7.0, -8.0)
      call glVertex3f(  0.0, 2.0, 27.0)

      call glVertex3f( 60.0, 2.0, -8.0)
      call glVertex3f(-60.0, 2.0, -8.0)
      call glVertex3f(  0.0, 7.0, -8.0)
      
      ! Other wing top section
      call glVertex3f(  0.0, 2.0, 27.0)
      call glVertex3f(  0.0, 7.0, -8.0)
      call glVertex3f(-60.0, 2.0, -8.0)

      ! Tail section///////////////////////////////
      ! Bottom of back fin
      call glColor3ub(i255,i255,i000)
      call glVertex3f(-30.0, -0.50, -57.0)
      call glVertex3f( 30.0, -0.50, -57.0)
      call glVertex3f(  0.0, -0.50, -40.0)
   
      ! top of left side
      call glVertex3f(  0.0, -0.5, -40.0)
      call glVertex3f( 30.0, -0.5, -57.0)
      call glVertex3f(  0.0,  4.0, -57.0)

      ! top of right side
      call glVertex3f(  0.0,  4.0, -57.0)
      call glVertex3f(-30.0, -0.5, -57.0)
      call glVertex3f(  0.0, -0.5, -40.0)
 
      ! back of bottom of tail
      call glVertex3f( 30.0, -0.5, -57.0)
      call glVertex3f(-30.0, -0.5, -57.0)
      call glVertex3f(  0.0,  4.0, -57.0)

      ! Top of Tail section left
      call glColor3ub(i255,i000,i000)
      call glVertex3f(  0.0,  0.5, -40.0)
      call glVertex3f(  3.0,  0.5, -57.0)
      call glVertex3f(  0.0, 25.0, -65.0)

      call glVertex3f(  0.0, 25.0, -65.0)
      call glVertex3f( -3.0,  0.5, -57.0)
      call glVertex3f(  0.0,  0.5, -40.0)

      ! Back of horizontal section
      call glVertex3f(  3.0,  0.5, -57.0)
      call glVertex3f( -3.0,  0.5, -57.0)
      call glVertex3f(  0.0, 25.0, -65.0)
    call glEnd()

  call glPopMatrix()

  ! Display the results
  call glutSwapBuffers()
  
end subroutine RenderScene






