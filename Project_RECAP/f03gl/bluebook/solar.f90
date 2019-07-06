!
! solar.f90
! OpenGL SuperBible
! Demonstrates OpenGL nested coordinate transformations
! and perspective
! Original program by Richard S. Wright Jr.
!
program solar

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

     subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

   end interface

   common fmoonrot, fearthrot

   fmoonrot  = 0.0
   fearthrot = 0.0
   
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Earth/Moon/Sun System'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1);

   call SetupRC
   
   call glutMainLoop()

end program
subroutine SetupRC
	
   use OpenGL_GL

   real(kind=GLfloat), dimension(4) :: whiteLight  = (/ .2, .2, .2, 1./)
   real(kind=GLfloat), dimension(4) :: sourceLight = (/ .8, .8, .8, 1./) 
   real(kind=GLfloat), dimension(4) :: lightPos    = (/ .0, .0, .0, 1./) 

   ! Light values and coordinates
   call glEnable(GL_DEPTH_TEST) ! Hidden surface removal
   call glFrontFace(GL_CCW)     ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)  ! Do not calculate inside of jet

   ! Enable lighting
   call glEnable(GL_LIGHTING)

   ! Setup and enable light 0
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,whiteLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,sourceLight)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)
 
   ! Set Material properties to followcallcall   call glColor values
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

   ! Black blue background
   call glClearColor(0.0, 0.0, 0.0, 1.0 )

end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop
   
end subroutine KeyPressFunc
subroutine RenderScene bind(C)

   use opengl_gl
   use opengl_glut
  
   real(kind=GLfloat), dimension(4) :: lightPos    = (/ .0, .0, .0, 1./) 

   common fmoonrot, fearthrot

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the matrix state and do the rotations
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix

   ! Translate the whole scene out and into view  
   call glTranslatef(0.0, 0.0, -300.0)      

   ! Set material color, Red
   ! Sun
   call glDisable(GL_LIGHTING)
   call glColor3f(1.0,1.0,0.0)
   call glutSolidSphere(dble(15.), 30, 17)
   call glEnable(GL_LIGHTING)

   ! Move the light after we draw the sun!
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)

   ! Rotate coordinate system
   call glRotatef(fEarthRot, 0.0, 1.0, 0.0)

   ! Draw the Earth
   call glColor3f(0.0,0.0,1.0)
   call glTranslatef(105.0,0.0,0.0)
   call glutSolidSphere(dble(15.), 30, 17)

   ! Rotate from Earth based coordinates and draw Moon
   call glColor3f(0.8,0.8,0.8)
   call glRotatef(fMoonRot,0.0, 1.0, 0.0)
   call glTranslatef(30.0, 0.0, 0.0)
   
   fMoonRot = fmoonrot + 15.0
   if( fMoonRot > 360.0) fMoonRot = 0.0

   call glutSolidSphere(dble(6.), 30, 17)

   ! Restore the matrix state
   call glPopMatrix                  ! Modelview matrix

   ! Step earth orbit 5 degrees
   fEarthRot = fearthrot + 5.0
   if( fEarthRot > 360.0) fEarthRot = 0.0

   ! Show the image
   call glutSwapBuffers

end subroutine RenderScene
recursive subroutine TimerFunction( ivalue ) bind(C)

   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(100,TimerFunction, 1)

end subroutine TimerFunction
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu
 
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, Aspect

   w = win
   h = hin
   
   ! Prevent a divide by zero
   if( h == 0 ) h = 1

   ! Set the viewport to be the entire window
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)

   !field of view of 45 degrees, near and far planes 1.0 and 425
   call gluPerspective(dble(45.), Aspect, dble(1.), dble(425.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 

