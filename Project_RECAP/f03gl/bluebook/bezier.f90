!
! Bezier.cpp
! Demonstrates OpenGL evaluators to draw bezier curve
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   integer, parameter :: NumPoints = 4
   
   real(kind=GLfloat), dimension(4) :: whitelight  = (/ 0.05, 0.05,0.05, 1. /)
   real(kind=GLfloat), dimension(4) :: sourcelight = (/ 0.25, 0.25,0.25, 1. /)
   real(kind=GLfloat), dimension(4) :: lightpos    = (/ -10.,  5.0, 5.0, 1. /)

   real(kind=GLfloat), dimension(4,3) :: ctrlPoints 
   
   data ctrlPoints / -4.0, -6.0,  6.0, 4.0, &                     
                      0.0,  4.0, -4.0, 0.0, &
                      0.0,  0.0,  0.0, 0.0  /

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
   iwin = glutCreateWindow('2D Bezier Curve'//char(0))

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
   call glBegin(GL_POINTS)
   do i=1,NumPoints
     call glVertex2fv(ctrlPoints(i,:))
   end do
   call glEnd

end subroutine DrawPoints
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glu
   use opengl_glut

   use spheremod

   real, dimension(NumPoints*3) :: tmp

   ic = 1
   do i=1,NumPoints
     tmp(ic:ic+2) = ctrlPoints(i,:)
     ic = ic + 3
   end do

   ! Clear the window with current clearing color
   call glClear(GL_COLOR_BUFFER_BIT)

   ! Sets up the bezier
   ! This actually only needs to be called once and could go in
   ! the setup function
   call glMap1f(GL_MAP1_VERTEX_3,   & ! Type of data generated
      0.0 ,                         & ! Lower u range
    100.0 ,                         & ! Upper u range
      3   ,                         & ! Distance between points in the data
    NumPoints,                      & ! number of control points
    tmp       )                       ! array of control points

   ! Enable the evaluator
   call glEnable(GL_MAP1_VERTEX_3)

   ! Use a line strip to "connect-the-dots"
   call glBegin(GL_LINE_STRIP)
   do i=0,100 
     call glEvalCoord1f(float(i))     ! Evaluate the curve at this point
   end do
   call glEnd

   ! Use higher level functions to map to a grid, then evaluate the
   ! entire thing.
   ! Put these two functions in to replace above loop

   ! Map a grid of 100 points from 0 to 100
   call glMapGrid1d(100,dble(0.0),dble(100.0))

   ! Evaluate the grid, using lines
   !call glEvalMesh1(GL_LINE,0,100)

   ! Draw the Control Points
   call DrawPoints

   ! Flush drawing commands
   call glutSwapBuffers
  
end subroutine renderscene
subroutine SetupRC

   use OpenGL_GL

   ! Clear Window to white
   call glClearColor(1.0, 1.0, 1.0, 1.0)

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

   call gluOrtho2D(dble(-10.0),dble(10.0),dble(-10.0),dble(10.0))

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
