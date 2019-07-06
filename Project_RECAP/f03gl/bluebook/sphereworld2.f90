!
! SphereWorld2.cpp
! OpenGL SuperBible
! Demonstrates an immersive 3D environment using actors
! and a camera. This version adds lights and material properties
! and shadows.
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use GLframes

   integer, parameter :: NUM_SPHERES = 30

   type(GLframe), allocatable :: spheres(:)
   type(GLframe), save        :: frameCamera

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .3,  .3,  .3,  1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/   .7,  .7,  .7,  1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  1.,  1.,  1.,   1. /)
   real(kind=GLfloat), dimension(4) :: specref      = (/  1.,  1.,  1.,   1. /)

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-100.,100., 50.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real(kind=GLfloat), dimension(4) :: vplaneequation
   real(kind=GLfloat), dimension(16):: shadowmatrix

   real :: yRot = 0.0
   
end module spheremod
program sphereworld2

   use opengl_gl
   use opengl_glut

   use spheremod
   
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

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface
     
   allocate(spheres(NUM_SPHERES))
   
   write(*,*)'bollen'

   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('OpenGL SphereWorld Demo + Lights and Shadow'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)
   call glutTimerFunc(33, TimerFunction, 1)

   call setuprc
   
   call glutMainLoop

end program
subroutine SetupRC
	
   use OpenGL_GL

   use spheremod
   
   real, dimension(3,3) :: points
   
   points(1,:) = (/  0.0, -0.4,  0.0 /)
   points(2,:) = (/ 10.0, -0.4,  0.0 /)
   points(3,:) = (/  5.0, -0.4, -5.0 /)
      
   ! Grayish background
   call glClearColor(lowlight(1),lowlight(2),lowlight(3),lowlight(4))
         
   ! Cull backs of polygons
   call glCullFace(GL_BACK)
   call glFrontFace(GL_CCW)
   call glEnable(GL_CULL_FACE)
   call glEnable(GL_DEPTH_TEST)
    
   ! Setup light parameters
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, NoLight)
   call glLightfv(GL_LIGHT0, GL_AMBIENT, LowLight)
   call glLightfv(GL_LIGHT0, GL_DIFFUSE, BrightLight)
   call glLightfv(GL_LIGHT0, GL_SPECULAR, BrightLight)
   call glEnable(GL_LIGHTING)
   call glEnable(GL_LIGHT0)

   ! Get the plane equation from three points on the ground
   call m3dGetPlaneEquation(vPlaneEquation,Points(1,:),Points(2,:),Points(3,:))

   ! Calculate projection matrix to draw shadow on the ground
   call m3dMakePlanarShadowMatrix(ShadowMatrix, vPlaneEquation, LightPos)
 
   ! Mostly use material tracking
   call glEnable(GL_COLOR_MATERIAL)
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
   call glMateriali(GL_FRONT, GL_SHININESS, 128)

   ! Randomly place the sphere inhabitants
   do i=1,NUM_SPHERES 
     call random_number(r)
     x = (r - 0.5) * 40
     call random_number(r)
     z = (r - 0.5) * 40

     call SetOrigin(spheres(i), x,0.0,z)
   end do

end subroutine SetupRC
subroutine DrawGround 
!
!  Draw a gridded ground
!	
   use OpenGL_GL

   fExtent = 20.0
   fStep   =  1.0
   y       = -0.4
   
   x = -fExtent
   do while( x <= fExtent)
     call glBegin(GL_TRIANGLE_STRIP)
     
     call glNormal3f(0.0, 1.0, 0.0)  ! All Point up

     x2 = fExtent
     do while( x2 >= -fExtent)
       call glVertex3f( x,       y, x2) 
       call glVertex3f( x+fstep, y, x2) 
       x2 = x2 - fstep
     end do
     x = x + fstep
     call glEnd
   end do
   

end subroutine DrawGround
recursive subroutine TimerFunction( ivalue ) bind(C)
!
! Called by GLUT library when idle (window not being
! resized or moved)
!
   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(3,TimerFunction, 1)

end subroutine TimerFunction
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

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(50.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     deallocate(spheres)
     stop
   endif
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)    call MoveForward(frameCamera, 0.1)
   if( key == GLUT_KEY_DOWN)  call MoveForward(frameCamera,-0.1)
   if( key == GLUT_KEY_LEFT)  call RotateLocalY(frameCamera, 0.1)
   if( key == GLUT_KEY_RIGHT) call RotateLocalY(frameCamera,-0.1)
   
   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine DrawInhabitants(iShadow)

   use opengl_gl
   use opengl_glut

   use spheremod

   if( iShadow == 0 )then
     yrot = yrot + 0.5
   else
     call glColor4f(0.0, 0.0, 0.0, 0.5)
   endif
   
   if( iShadow == 0 ) call glColor3f(0.0, 1.0, 0.0)
   
   do i=1,NUM_SPHERES    
     call glPushMatrix
     call ApplyActorTransform(spheres(i))
     call glutSolidSphere(dble(0.3), 17, 9)
     call glPopMatrix
   end do

   call glPushMatrix
     call glTranslatef(0.0, 0.1, -2.5) 
    
     if( iShadow == 0 ) call glColor3f(0.0, 0.0, 1.0)
     
     call glPushMatrix 
       call glRotatef(-yRot * 2.0, 0.0, 1.0, 0.0) 
       call glTranslatef(1.0, 0.0, 0.0) 
       call glutSolidSphere(dble(0.1), 17, 9) 
     call glPopMatrix 
    
     if( iShadow == 0 )then
       ! Torus alone will be specular
       call glColor3f(1.0, 0.0, 0.0)
       call glMaterialfv(GL_FRONT, GL_SPECULAR, BrightLight)
     endif
     
     call glRotatef(yRot, 0.0, 1.0, 0.0) 
     call gltDrawTorus(0.35, 0.15, 61, 37) 
     call glMaterialfv(GL_FRONT, GL_SPECULAR, NoLight)
   call glPopMatrix 

end subroutine DrawInhabitants
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   call glPushMatrix
   
   call ApplyCameraTransform(frameCamera,.false.)
        
     ! Position light before any other transformations
     call glLightfv(GL_LIGHT0, GL_POSITION, LightPos)
        
     call glColor3f(0.60,0.40,0.10)   ! Draw the ground
     call DrawGround
     
     ! Draw shadows first
     call glDisable(GL_DEPTH_TEST)
     call glDisable(GL_LIGHTING)
     call glPushMatrix
       call glMultMatrixf(ShadowMatrix)
       call DrawInhabitants(1)
     call glPopMatrix
     call glEnable(GL_LIGHTING)
     call glEnable(GL_DEPTH_TEST)

     ! Draw inhabitants normally
     call DrawInhabitants(0)
   call glPopMatrix 

   call glutSwapBuffers
   call glutPostRedisplay

end subroutine RenderScene
