!
! SphereWorld.cpp
! OpenGL SuperBible
! Demonstrates an immersive 3D environment using actors
! and a camera.
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use GLframes

   integer, parameter :: NUM_SPHERES = 50

   type(GLframe), allocatable :: spheres(:)
   type(GLframe), save        :: frameCamera

end module spheremod
program sphereworld

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
   iwin = glutCreateWindow('OpenGL SphereWorld Demo'//char(0))

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
   
   ! Bluish background
   call glClearColor(0.0, 0.0, 0.5, 1.0)
         
   ! Draw everything as wire frame
   call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)

   ! Randomly place the sphere inhabitants
   do i=1,NUM_SPHERES 
     !
     ! Pick a random location between -20 and 20 at .1 increments
     !
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
   
   call glBegin(GL_LINES)
   x = -fExtent
   do while( x <= fExtent)
     call glVertex3f(x, y,  fExtent)   ! Draw Z lines
     call glVertex3f(x, y, -fExtent) 
   
     call glVertex3f( fExtent, y, x) 
     call glVertex3f(-fExtent, y, x) 
     x = x + fstep
   end do
   
   call glEnd

end subroutine DrawGround
recursive subroutine TimerFunction( ivalue ) bind(C)

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
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   real(kind=GLfloat), dimension(16) :: transformationMatrix

   real, save :: yRot = 0.0
   
   real m3dDegToRad
   external m3dDegToRad

   yrot = yrot + 0.5
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   call glPushMatrix
   
   call ApplyCameraTransform(frameCamera,.false.)
        
     ! Draw the ground
     call DrawGround
     
     ! Draw the randomly located spheres
     do i=1,NUM_SPHERES    
       call glPushMatrix
       call ApplyActorTransform(spheres(i))
       call glutSolidSphere(dble(0.1), 13, 26)
       call glPopMatrix
     end do

     call glPushMatrix
       call glTranslatef(0.0, 0.0, -2.5) 
    
       call glPushMatrix 
         call glRotatef(-yRot * 2.0, 0.0, 1.0, 0.0) 
         call glTranslatef(1.0, 0.0, 0.0) 
         call glutSolidSphere(dble(0.1), 13, 26) 
       call glPopMatrix 
    
       call glRotatef(yRot, 0.0, 1.0, 0.0) 
       call gltDrawTorus(0.35, 0.15, 40, 20) 
     call glPopMatrix 
   call glPopMatrix 

   call glutSwapBuffers

end subroutine RenderScene
