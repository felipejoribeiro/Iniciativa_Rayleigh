!
! Snowman.cpp
! Demonstrates using Quadric Objects
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   real(kind=GLfloat), dimension(4) :: whitelight  = (/ 0.05, 0.05,0.05, 1. /)
   real(kind=GLfloat), dimension(4) :: sourcelight = (/ 0.25, 0.25,0.25, 1. /)
   real(kind=GLfloat), dimension(4) :: lightpos    = (/ -10.,  5.0, 5.0, 1. /)

   real :: xrot = 0.0
   real :: yrot = 0.0

end module spheremod
program sneeuwman

   use opengl_gl
   use opengl_glut

   use spheremod
   
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
        
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Modeling with Quadrics'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)

   call setuprc
   
   call glutMainLoop

end program
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu
 
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

   aspect = float(w)/float(h)

   ! Produce the perspective projection
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(40.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLu

   use spheremod

   call glEnable(GL_DEPTH_TEST)    ! Hidden surface removal
   call glFrontFace(GL_CCW)        ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)     ! Do not calculate inside

   ! Enable lighting
   call glEnable(GL_LIGHTING)

   ! Setup and enable light 0
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,whiteLight)
   call glLightfv(GL_LIGHT0,GL_AMBIENT,sourceLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,sourceLight)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

   ! Set Material properties to follow call glColor values
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

   ! Black blue background
   call glClearColor(0.25, 0.25, 0.50, 1.0)
      
end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

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
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glu
   use opengl_glut

   use spheremod

   type(C_PTR) :: pObj

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the matrix state and do the rotations
   call glPushMatrix()
     ! Move object back and do in place rotation
     call glTranslatef(0.0, -0.5, -5.0)
     call glRotatef(xRot, 1.0, 0.0, 0.0)
     call glRotatef(yRot, 0.0, 1.0, 0.0)

     ! Draw something
     pObj = gluNewQuadric()
     call gluQuadricNormals(pObj,GLU_SMOOTH)
     
     ! Main Body
     call glPushMatrix()
       call glColor3f(1.0, 1.0, 1.0)
       call gluSphere(pObj,dble(.40), 26, 13)  ! Bottom
     
       call glTranslatef(0.0, .550, 0.0) ! Mid section
       call gluSphere(pObj,dble(.3), 26, 13)
     
       call glTranslatef(0.0, 0.45, 0.0) ! Head
       call gluSphere(pObj,dble(0.24), 26, 13)
     
       ! Eyes
       call glColor3f(0.0, 0.0, 0.0)
       call glTranslatef(0.1, 0.1, 0.21)
       call gluSphere(pObj,dble(0.02), 26, 13)
     
       call glTranslatef(-0.2, 0.0, 0.0)
       call gluSphere(pObj,dble(0.02), 26, 13)
     
       ! Nose
       call glColor3f(1.0, 0.3, 0.3)
       call glTranslatef(0.1, -0.12, 0.0)
       call gluCylinder(pObj,dble(0.04),dble(0.0),dble(0.3), 26, 13)
     call glPopMatrix()
     
     ! Hat
     call glPushMatrix()
       call glColor3f(0.0, 0.0, 0.0)
       call glTranslatef(0.0, 1.17, 0.0)
       call glRotatef(-90.0, 1.0, 0.0, 0.0)
       call gluCylinder(pObj,dble(0.17),dble(0.17),dble(0.4), 26, 13)
       
       ! Hat brim
       call glDisable(GL_CULL_FACE)
       call gluDisk(pObj,dble(0.17),dble(0.28), 26, 13)
       call glEnable(GL_CULL_FACE)
       
       call glTranslatef(0.0, 0.0, 0.40)
       call gluDisk(pObj,dble(0.0),dble(0.17), 26, 13)
     call glPopMatrix()
     
   ! Restore the matrix state
   call glPopMatrix()

   ! Buffer swap
   call glutSwapBuffers()

end subroutine renderscene
