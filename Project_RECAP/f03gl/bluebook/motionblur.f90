!
! Motionblur.cpp
! OpenGL SuperBible
! Demonstrates Motion Blur with the Accumulation buffer
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use opengl_gl

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .3,  .3,  .3,  1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/   .7,  .7,  .7,  1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  1.,  1.,  1.,   1. /)
   real(kind=GLfloat), dimension(4) :: specref      = (/  1.,  1.,  1.,   1. /)

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-100.,100., 50.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real :: yRot = 45.0
   
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
    
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
        ior(GLUT_RGB,ior(GLUT_DEPTH,GLUT_ACCUM))))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('Motion Blur with the Accumulation Buffer'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

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
!  Draw a gridded ground
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
subroutine DrawGeometry 
	
   use OpenGL_GL
   use OpenGL_GLut

   use spheremod

   ! Clear the window with current clearing color
   call  glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
        
   call glPushMatrix
     call DrawGround
        
     ! Place the moving sphere
     call glColor3f(1.0, 0.0, 0.0)
     call glTranslatef(0.0, 0.5, -3.5)
     call glRotatef(-yRot*2.0, 0.0, 1.0, 0.0)
     call glTranslatef(1.0, 0.0, 0.0)
     call glutSolidSphere(dble(0.1), 17, 9)
   call glPopMatrix
  
end subroutine DrawGeometry 
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
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   ! Set the current rotation back a few degrees
   yRot = 35.0
            
   do i=1,10
     yRot = yRot + 0.75
     
     ! Draw sphere
     call DrawGeometry
        
     ! Accumulate to back buffer
     if( i == 1)then
       call glAccum(GL_LOAD, 0.5)
     else
       call glAccum(GL_ACCUM, 0.5 * 0.1)
     endif

   end do
   
   ! copy accumulation buffer to color buffer and
   ! do the buffer Swap
   call glAccum(GL_RETURN, 1.0)
   
   call glutSwapBuffers

end subroutine RenderScene
