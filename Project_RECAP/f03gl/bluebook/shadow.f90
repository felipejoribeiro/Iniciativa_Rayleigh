!
! Shdaow.f90
! OpenGL SuperBible
! Demonstrates simple planar shadows
! Original program by Richard S. Wright Jr.
!
program shadow

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

   real, dimension(4,4) :: shadowMat
   real                 :: xrot, yrot
   common  xrot, yrot, shadowMat

   xrot     =  0.0
   yrot     =  0.0
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Shadow'//char(0))
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop

end program
subroutine SetupRC

   use OpenGL_GL

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .3,  .3,  .3, 1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/   .7,  .7,  .7, 1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  1.,  1.,  1.,  1. /)
   real(kind=GLfloat), dimension(4) :: specref      = (/  1.,  1.,  1.,  1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/-75.,150.,-50.,  0. /)

   real, dimension(3,3) :: points
   real, dimension(4)   :: vPlaneEquation
   
   real, dimension(4,4) :: shadowMat
   real                 :: xrot, yrot
   common  xrot, yrot, shadowMat

   points(1,:) = (/ -30.0, -149.0, -20.0 /) 
   points(2,:) = (/ -30.0, -149.0,  20.0 /) 
   points(3,:) = (/  40.0, -149.0,  20.0 /)

   call glEnable(GL_DEPTH_TEST)    ! Hidden surface removal
   call glFrontFace(GL_CCW)        ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)     ! Do not calculate inside of jet

   ! Setup and enable light 0
   call glLightfv(GL_LIGHT0,GL_AMBIENT,ambientLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,diffuseLight)
   call glLightfv(GL_LIGHT0,GL_SPECULAR,specular)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

   ! Set Material properties to followcall glColor values
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

   ! All materials hereafter have full specular reflectivity
   ! with a high shine
   call glMaterialfv(GL_FRONT,GL_SPECULAR,specref)
   call glMateriali(GL_FRONT,GL_SHININESS,128)

   ! Light blue background
   call glClearColor(0.0, 0.0, 1.0, 1.0 )

   ! Get the plane equation from three points on the ground
   call m3dGetPlaneEquation(vPlaneEquation, points(1,:), points(2,:), points(3,:))

   ! Calculate projection matrix to draw shadow on the ground
   call m3dMakePlanarShadowMatrix(shadowMat, vPlaneEquation, lightPos)
   
   call glEnable(GL_NORMALIZE)

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

   real, dimension(4,4) :: shadowMat
   real                 :: xrot, yrot
   common  xrot, yrot, shadowMat

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
   use OpenGL_GLU

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-75.,150.,-50.,  0. /)

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

   call gluPerspective(dble(60.0), Aspect, dble(200.0), dble(500.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity
   
   ! Move out Z axis so we can see everything
   call glTranslatef(0.0, 0.0, -400.0)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)

end subroutine ChangeSize
subroutine DrawJet(iShadow)

   use opengl_gl
   use opengl_glut

   integer(kind=GLubyte), parameter :: i000 = z'00'
   integer(kind=GLubyte), parameter :: i064 = z'40'
   integer(kind=GLubyte), parameter :: i128 = z'80'
   integer(kind=GLubyte), parameter :: i192 = z'C0'
   integer(kind=GLubyte), parameter :: i255 = z'FF'

   real,dimension(3,3) :: vpoints
   real,dimension(3  ) :: vnormal

   real, dimension(4,4) :: shadowMat
   real                 :: xrot, yrot
   common  xrot, yrot, shadowMat

   if( iShadow == 0 )then
     call glColor3ub(i128,i128,i128)
   else
     call glColor3ub(i000,i000,i000)
   endif

   ! Nose Cone - Points straight down
   ! Set material color
   call glBegin(GL_TRIANGLES)

     call glNormal3f(  0.0,-1.0,  0.0)
     call glVertex3f(  0.0, 0.0, 60.0)
     call glVertex3f(-15.0, 0.0, 30.0)
     call glVertex3f( 15.0, 0.0, 30.0)
                
     ! Verticies for this panel
     vPoints(1,1:3) = (/ 15.0,  0.0, 30.0 /)
     vPoints(2,1:3) = (/  0.0, 15.0, 30.0 /)
     vPoints(3,1:3) = (/  0.0,  0.0, 60.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0,  0.0, 60.0 /)
     vPoints(2,1:3) = (/  0.0, 15.0, 30.0 /)
     vPoints(3,1:3) = (/-15.0,  0.0, 30.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     ! Body of the Plane ////////////////////////
     vPoints(1,1:3) = (/-15.0,  0.0, 30.0 /)
     vPoints(2,1:3) = (/  0.0, 15.0, 30.0 /)
     vPoints(3,1:3) = (/  0.0,  0.0,-56.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0,  0.0,-56.0 /)
     vPoints(2,1:3) = (/  0.0, 15.0, 30.0 /)
     vPoints(3,1:3) = (/ 15.0,  0.0, 30.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     call glNormal3f(0.0, -1.0, 0.0)
     call glVertex3f( 15.0, 0.0, 30.0)
     call glVertex3f(-15.0, 0.0, 30.0)
     call glVertex3f(  0.0, 0.0,-56.0)

     !//////////////////////////////////////////////
     ! Left wing
     ! Large triangle for bottom of wing
     vPoints(1,1:3) = (/  0.0,  2.0, 27.0 /)
     vPoints(2,1:3) = (/-60.0,  2.0, -8.0 /)
     vPoints(3,1:3) = (/ 60.0,  2.0, -8.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/ 60.0,  2.0, -8.0 /)
     vPoints(2,1:3) = (/  0.0,  7.0, -8.0 /)
     vPoints(3,1:3) = (/  0.0,  2.0, 27.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/ 60.0,  2.0, -8.0 /)
     vPoints(2,1:3) = (/-60.0,  2.0, -8.0 /)
     vPoints(3,1:3) = (/  0.0,  7.0, -8.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0,  2.0, 27.0 /)
     vPoints(2,1:3) = (/  0.0,  7.0, -8.0 /)
     vPoints(3,1:3) = (/-60.0,  2.0, -8.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     ! Tail section///////////////////////////////
     ! Bottom of back fin
     call glNormal3f(0.0, -1.0, 0.0)
     call glVertex3f(-30.0, -0.50, -57.0)
     call glVertex3f( 30.0, -0.50, -57.0)
     call glVertex3f(  0.0, -0.50, -40.0)

     vPoints(1,1:3) = (/  0.0, -0.5,-40.0 /)
     vPoints(2,1:3) = (/ 30.0, -0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0,  4.0,-57.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0,  4.0,-57.0 /)
     vPoints(2,1:3) = (/-30.0, -0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0, -0.5,-40.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/ 30.0, -0.5,-57.0 /)
     vPoints(2,1:3) = (/-30.0, -0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0,  4.0,-57.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0,  0.5,-40.0 /)
     vPoints(2,1:3) = (/  3.0,  0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0, 25.0,-65.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  0.0, 25.0,-65.0 /)
     vPoints(2,1:3) = (/ -3.0,  0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0,  0.5,-40.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

     vPoints(1,1:3) = (/  3.0,  0.5,-57.0 /)
     vPoints(2,1:3) = (/ -3.0,  0.5,-57.0 /)
     vPoints(3,1:3) = (/  0.0, 25.0,-65.0 /)

     call m3dFindNormal(vNormal, &
          vPoints(1,:), vPoints(2,:), vPoints(3,:))
     call glNormal3fv(vNormal)
     call glVertex3fv(vPoints(1,:))
     call glVertex3fv(vPoints(2,:))
     call glVertex3fv(vPoints(3,:))

   call glEnd()
 
end subroutine DrawJet
subroutine RenderScene() bind(C)
   
   use opengl_gl
   use opengl_glut

   integer(kind=GLubyte), parameter :: i000 = z'00'
   integer(kind=GLubyte), parameter :: i032 = z'20'
   integer(kind=GLubyte), parameter :: i064 = z'40'
   integer(kind=GLubyte), parameter :: i128 = z'80'
   integer(kind=GLubyte), parameter :: i192 = z'C0'
   integer(kind=GLubyte), parameter :: i255 = z'FF'

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-75.,150.,-50.,  0. /)

   real, dimension(4,4) :: shadowMat
   real                 :: xrot, yrot
   common  xrot, yrot, shadowMat

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Draw the ground, we do manual shading to a darker green
   ! in the background to give the illusion of depth
   call glBegin(GL_QUADS)
     call glColor3ub(i000,i032,i000)
     call glVertex3f( 400.0, -150.0, -200.0)
     call glVertex3f(-400.0, -150.0, -200.0)
     call glColor3ub(i000,i255,i000)
     call glVertex3f(-400.0, -150.0,  200.0)
     call glVertex3f( 400.0, -150.0,  200.0)
   call glEnd

   ! Save the matrix state and do the rotations
   call glPushMatrix 

   ! Draw jet at new orientation, put light in correct position
   ! before rotating the jet
   call glEnable(GL_LIGHTING)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   call DrawJet(0)

   ! Restore original matrix state
   call glPopMatrix	

   ! Get ready to draw the shadow and the ground
   ! First disable lighting and save the projection state
   call glDisable(GL_DEPTH_TEST)
   call glDisable(GL_LIGHTING)
   call glPushMatrix

   ! Multiply by shadow projection matrix
   call glMultMatrixf(shadowMat)

   ! Now rotate the jet around in the new flattend space
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   ! Pass true to indicate drawing shadow
   call DrawJet(1)	

   ! Restore the projection to normal
   call glPopMatrix

   ! Draw the light source
   call glPushMatrix
   call glTranslatef(lightPos(1),lightPos(2), lightPos(3))
   call glColor3ub(i255,i255,i000)
   call glutSolidSphere(dble(5.0),10,10)
   call glPopMatrix

   ! Restore lighting state variables
   call glEnable(GL_DEPTH_TEST)

   ! Display the results
   call glutSwapBuffers()
    
end subroutine RenderScene 
