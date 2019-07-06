!
! Bez3d.cpp
! Demonstrates OpenGL evaluators to draw bezier curve
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   integer, parameter :: NumPoints = 3
   
   real(kind=GLfloat), dimension(4) :: ambientLight = (/ 0.3, 0.3, 0.3, 1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/ 0.7, 0.7, 0.7, 1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/ 20., 0.0, 0.0, 0. /)

   real(kind=GLfloat), dimension(3,3,3) :: ctrlPoints 
   
   data ctrlPoints / -4.0, -2.0, 4.0, -4.0, -2.0, 4.0, -4.0, -2.0,  4.0, &                     
                      0.0,  4.0, 0.0,  0.0,  4.0, 0.0,  0.0,  4.0,  0.0, &
                      4.0,  4.0, 4.0,  0.0,  0.0, 0.0, -4.0, -4.0, -4.0  /

end module spheremod
program bezier

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
   iwin = glutCreateWindow('Lit 3D Bezier Surface'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call setuprc
   
   call glutMainLoop

end program
subroutine DrawPoints
! This function is used to superimpose the control points over the curve

   use opengl_gl

   use spheremod
      
   ! Set point size larger to make more visible
   call glPointSize(5.0)

   ! Loop through all control points for this example
   call glColor3f(1.0,0.0,0.0)
   call glBegin(GL_POINTS)
   do i=1,NumPoints
     do j=1,3
       call glVertex3fv(ctrlPoints(i,j,:))
     end do
   end do
   call glEnd

   call glColor3f(0.0,0.0,1.0)

end subroutine DrawPoints
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glu
   use opengl_glut

   use spheremod

   real, dimension(NumPoints*3*3) :: tmp

   ic = 1
   do i=1,NumPoints
     do j=1,3
       tmp(ic:ic+2) = ctrlPoints(i,j,:)
       ic = ic + 3
     end do
   end do

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the modelview matrix stack
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix

   ! Rotate the mesh around to make it easier to see
   call glRotatef(45.0, 0.0, 1.0, 0.0)
   call glRotatef(60.0, 1.0, 0.0, 0.0)

   ! Sets up the bezier
   ! This actually only needs to be called once and could go in
   ! the setup function
   call glMap2f(GL_MAP2_VERTEX_3,   & ! Type of data generated              
      0.0 ,                         & ! Lower u range
     10.0 ,                         & ! Upper u range
       3  ,                         & ! Distance between points in the data
       3  ,                         & ! Dimension in u direction (order)
      0.0 ,                         & ! Lover v range
     10.0 ,                         & ! Upper v range
       9  ,                         & ! Distance between points in the data
       3  ,                         & ! Dimension in v direction (order)
     tmp)                             ! array of control points

   ! Enable the evaluator
   call glEnable(GL_MAP2_VERTEX_3)

   ! Use higher level functions to map to a grid, then evaluate the
   ! entire thing.

   ! Map a grid of 10 points from 0 to 10
   call glMapGrid2f(10,0.0,10.0,10,0.0,10.0)

   ! Evaluate the grid, using lines
   call glEvalMesh2(GL_FILL,0,10,0,10)

   ! Draw the Control Points
   call DrawPoints

   ! Restore the modelview matrix
   call glPopMatrix

   ! Flush drawing commands
   call glutSwapBuffers
  
end subroutine renderscene
subroutine SetupRC

   use OpenGL_GL

   use spheremod

   ! Clear Window to white
   call glClearColor(1.0, 1.0, 1.0, 1.0)

   call glEnable(GL_DEPTH_TEST)       ! Hidden surface removal

   ! Enable lighting
   call glEnable(GL_LIGHTING)

   ! Setup light 0
   call glLightfv(GL_LIGHT0,GL_AMBIENT,ambientLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,diffuseLight)

   ! Position and turn on the light
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

   ! Set Material properties to follow glColor values
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)

   ! Automatically generate normals for evaluated surfaces
   call glEnable(GL_AUTO_NORMAL)

   !Draw in Blue
   call glColor3f(0.0, 0.0, 1.0)	
     
end subroutine SetupRC
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

   call glOrtho(dble(-10.0),dble( 10.0),dble(-10.0), &
                dble( 10.0),dble(-10.0),dble( 10.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
