!
! Scissor.f90
! OpenGL SuperBible, 3rd Edition
! Richard S. Wright Jr.
! rwright@starstonesoftware.com
!
!
! PStipple.f90
! OpenGL SuperBible
! Demonstrates OpenGL Polygon Stippling
! Original program by Richard S. Wright Jr.
!
program pstipple

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

   end interface
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,GLUT_RGB))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('OpenGL Scissor'//char(0))
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call glutMainLoop

end program
subroutine KeyPressFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop

end subroutine KeyPressFunc
subroutine ChangeSize(win, hin) bind(C)

   use OpenGL_GL
   use OpenGL_GLU

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

   ! Set 2D Coordinate system
   call gluOrtho2D(dble(-4.0), dble(4.0), dble(-3.0), dble(3.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize
subroutine RenderScene bind(C)

   use opengl_gl
   use opengl_glut

   ! Clear the window 
   call glClearColor(0.0, 0.0, 1.0, 0.0)
   call glClear(GL_COLOR_BUFFER_BIT)

   ! Now set scissor to smaller red sub region
   call glClearColor(1.0, 0.0, 0.0, 0.0)
   call glScissor(100, 100, 600, 400)
   call glEnable(GL_SCISSOR_TEST)
   call glClear(GL_COLOR_BUFFER_BIT)
   
   ! Finally, an even smaller green rectangle
   call glClearColor(0.0, 1.0, 0.0, 0.0)
   call glScissor(200, 200, 400, 200)
   call glClear(GL_COLOR_BUFFER_BIT)
   
   ! Turn scissor back off for next render
   call glDisable(GL_SCISSOR_TEST)
   
   call glutSwapBuffers
  
end subroutine RenderScene
