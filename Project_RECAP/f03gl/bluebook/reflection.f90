!
! Reflection.cpp
! OpenGL SuperBible
! Demonstrates using blending/transparency
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use OpenGL_GL
   
   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight  = (/   .3,  .3,  .3,  1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight  = (/   .7,  .7,  .7,  1. /)
   real(kind=GLfloat), dimension(4) :: specular      = (/  1.,  1.,  1.,   1. /)
   real(kind=GLfloat), dimension(4) :: specref       = (/  1.,  1.,  1.,   1. /)

   real(kind=GLfloat), dimension(4) :: lightPos      = (/-100., 100., 50.,  1. /)
   real(kind=GLfloat), dimension(4) :: lightPosMirror= (/-100.,-100., 50.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight       = (/   0.,   0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight      = (/ 0.25, 0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight   = (/   1.,   1.,  1.,  1. /)

   real :: yRot = 0.0
   
end module spheremod
program reflection

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
     
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('OpenGL Blending and Transparency'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(10, TimerFunction, 1)

   call setuprc
   
   call glutMainLoop()

end program
subroutine SetupRC
	
   use OpenGL_GL

   use spheremod
         
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
 
   ! Mostly use material tracking
   call glEnable(GL_COLOR_MATERIAL)
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
   call glMateriali(GL_FRONT, GL_SHININESS, 128)

end subroutine SetupRC
subroutine DrawGround 
!
! Draw the ground as a series of triangle strips. The 
! shading model and colors are set such that we end up 
! with a black and white checkerboard pattern.
!
   use OpenGL_GL

   fExtent = 20.0
   fStep   =  0.5
   y       =  0.0
   iBounce =   0
   
   call glShadeModel(GL_FLAT)
   
   x = -fExtent
   do while( x <= fExtent)
     call glBegin(GL_TRIANGLE_STRIP)
     
     z = fExtent
     do while( z >= -fExtent)
       if( iBounce == 0 )then
         color = 1.0
       else
         color = 0.0
       endif
       
       call glColor4f(Color, Color, Color, 0.5)

       call glVertex3f( x,       y, z) 
       call glVertex3f( x+fstep, y, z) 
       z = z - fstep
       iBounce = abs(iBounce-1)
     end do
     x = x + fstep
     call glEnd
   end do

   call glShadeModel(GL_SMOOTH)

end subroutine DrawGround
recursive subroutine TimerFunction( ivalue ) bind(C)
!
! Called by GLUT library when idle (window not being
! resized or moved)
!
   use opengl_glut

   use spheremod

   integer, intent(in), value :: ivalue

   yRot = yRot + 1
   
   call glutPostRedisplay                 ! Redraw the scene
   call glutTimerFunc(1,TimerFunction, 1) ! Reset timer

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
   call glTranslatef(0.0, -0.4, 0.0)

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
subroutine DrawWorld

   use opengl_gl
   use opengl_glut

   use spheremod

   call glColor3f(1.0, 0.0, 0.0)

   call glPushMatrix
     call glTranslatef(0.0, 0.5, -3.5) 
    
     call glPushMatrix 
       call glRotatef(-yRot * 2.0, 0.0, 1.0, 0.0) 
       call glTranslatef(1.0, 0.0, 0.0) 
       call glutSolidSphere(dble(0.1), 17, 9) 
     call glPopMatrix 
         
     call glRotatef(yRot, 0.0, 1.0, 0.0) 
     call gltDrawTorus(0.35, 0.15, 61, 37) 
   call glPopMatrix 

end subroutine DrawWorld
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   call glPushMatrix
   
     ! Move light under floor to light the "reflected" world
     call glLightfv(GL_LIGHT0,gl_POSITION, LightPosMirror)
     call glPushMatrix
       call glFrontFace(GL_CW) ! geometry is mirrored, swap orientation
       call glScalef(1.0, -1.0, 1.0)
       call DrawWorld 
       call glFrontFace(GL_CCW)
     call glPopMatrix
    
     ! Draw the ground transparently over the reflection
     call glDisable(GL_LIGHTING)
     call glEnable(GL_BLEND)
     call glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
     
     call DrawGround
     
     call glDisable(GL_BLEND)
     call glEnable(GL_LIGHTING)
      
     ! Restore correct lighting and draw the world correctly
     call glLightfv(GL_LIGHT0,GL_POSITION, LightPos)
     call DrawWorld
   call glPopMatrix
        
   ! Do the buffer Swap
   call glutSwapBuffers
   
end subroutine RenderScene
