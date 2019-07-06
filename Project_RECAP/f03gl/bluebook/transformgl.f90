!
! transformgl.f90
! OpenGL SuperBible
! Demonstrates letting OpenGL do the transformations
! Original program by Richard S. Wright Jr.
!
program transform

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

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

   end interface
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('OpenGL Transformations Demo'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1);

   call setuprc
   
   call glutMainLoop

end program
subroutine SetupRC
	
   use OpenGL_GL

   ! Bluish background
   call glClearColor(0.0, 0.0, 0.5, 1.0)
         
   ! Draw everything as wire frame
   call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)

end subroutine SetupRC
recursive subroutine TimerFunction( ivalue ) bind(C)

   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(33,TimerFunction, 1)

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

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop
   
end subroutine KeyPressFunc
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut
  
   real(kind=GLfloat), dimension(16) :: transformationMatrix

   real, save :: yRot = 0.0
   
   real m3dDegToRad
   external m3dDegToRad

   yrot = yrot + 0.5
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Build a rotation matrix
   call m3dRotationMatrix44(transformationMatrix, m3dDegToRad(yRot), 0., 1.,0.)
   
   transformationMatrix(13) =  0.0
   transformationMatrix(14) =  0.0
   transformationMatrix(15) = -2.5
        
   call glLoadMatrixf(transformationMatrix)
   
   call gltDrawTorus(0.35, 0.15, 40, 20)

   ! Show the image
   call glutSwapBuffers

end subroutine RenderScene
