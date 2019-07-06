!
! transform.f90
! OpenGL SuperBible
! Demonstrates manual transformations
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
       use opengl_glu
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(in), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

   end interface
   
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Manual Transformations Demo'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1);

   call SetupRC
   
   call glutMainLoop()

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
   real :: yRotrad    = 0.0
   
   real m3dDegToRad
   external m3dDegToRad

   yrot = yrot + 0.5
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Build a rotation matrix
   yRotrad = m3dDegToRad(yRot)
   call m3dRotationMatrix44(transformationMatrix, yRotrad, 0., 1.,0.)
   
   transformationMatrix(13) =  0.0
   transformationMatrix(14) =  0.0
   transformationMatrix(15) = -2.5
        
   call DrawTorus(transformationMatrix)

   ! Show the image
   call glutSwapBuffers

end subroutine RenderScene
subroutine DrawTorus(mTransform)
!
! Draw a torus (doughnut), using the current 1D texture for light shading
! 
   use opengl_gl

   real, dimension(3), intent(in) :: mtransform   

   integer(kind=GLint),parameter :: numMajor = 40
   integer(kind=GLint),parameter :: numMinor = 20

   real(kind=GLfloat)  :: majorRadius = 0.35
   real(kind=GLfloat)  :: minorRadius = 0.15
   
   real, dimension(3)  :: objectVertex       ! Vertex in object/eye space
   real, dimension(3)  :: transformedVertex  ! New Transformed vertex   

   real(kind=GLfloat)  :: majorStep = 2.0*3.1415926 / numMajor
   real(kind=GLfloat)  :: minorStep = 2.0*3.1415926 / numMinor

   do i=0,numMajor-1

     a0 = i * majorStep
     a1 = a0 + majorStep

     x0 = cos(a0)
     y0 = sin(a0)
     x1 = cos(a1)
     y1 = sin(a1)

     call glBegin(GL_TRIANGLE_STRIP)
     do j=0,numMinor 

       b = j * minorStep
       c = cos(b)
       r = minorRadius * c + majorRadius
       z = minorRadius * sin(b)

       ! First point
       objectVertex(1) = x0*r
       objectVertex(2) = y0*r
       objectVertex(3) = z
       call m3dTransformVector3(transformedVertex, objectVertex, mTransform)
       call glVertex3fv(transformedVertex)

       ! Second point
       objectVertex(1) = x1*r
       objectVertex(2) = y1*r
       objectVertex(3) = z
       call m3dTransformVector3(transformedVertex, objectVertex, mTransform)
       call glVertex3fv(transformedVertex)
    end do
    call glEnd
  end do
          
end subroutine DrawTorus
