!
! CubeDX.cpp
! OpenGL SuperBible
! Demonstrates rendering with Indexed Vertex Arrays
! Program by Richard S. Wright Jr.
!
module spheremod

    integer, dimension(24), target :: indexes
   real, dimension(24), target:: corners
   
   ! Rotation amounts
   real :: xRot = 0.0
   real :: yRot = 0.0

end module spheremod
program cubedx

   use opengl_gl
   use opengl_glut

   use spheremod
   
   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ProcessMenu(i) bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: i
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

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface
        
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
        ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('Cube DX'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc) 

   call setuprc

   call glutMainLoop

end program
subroutine RenderScene() bind(C)
    
   use opengl_gl
   use opengl_glut

   use spheremod
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Make the cube a wire frame
   call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)

   ! Save the matrix state
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix()
   call glTranslatef(0.0, 0.0, -200.0)
   
   ! Rotate about x and y axes
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 0.0, 1.0)

   ! Enable and specify the vertex array
   call glEnableClientState(GL_VERTEX_ARRAY)
   call glVertexPointer(3, GL_FLOAT, 0, c_loc(corners))
      
   ! Using Drawarrays
  !call glDrawElements(GL_QUADS, 24, GL_UNSIGNED_byte, c_loc(indexes)) 
   call glDrawElements(GL_QUADS, 24, GL_UNSIGNED_INT, c_loc(indexes))  !<=======
  
   call glPopMatrix()

   ! Swap buffers
   call glutSwapBuffers
    
end subroutine RenderScene
subroutine SetupRC

   use OpenGL_GL

   use spheremod

   ! White background
   call glClearColor(1.0, 1.0, 1.0, 1.0 )

   ! Set drawing color to black
   call glColor3f(0.0, 0.0, 0.0)

   !
   ! NOTE: arrays defined in the C-way
   !
   ! Array containing the six vertices of the cube
   corners(:) = (/ -25.0,  25.0,  25.0, &  ! 1 // Front of cube
                    25.0,  25.0,  25.0, &  ! 2
                    25.0, -25.0,  25.0, &  ! 3
   		   -25.0, -25.0,  25.0, &  ! 4
   		   -25.0,  25.0, -25.0, &  ! 5 // Back of cube
                    25.0,  25.0, -25.0, &  ! 6
   	            25.0, -25.0, -25.0, &  ! 7
   	           -25.0, -25.0, -25.0  /) ! 8

   ! Array of indexes to create the cube
   indexes(:)   = (/ 0, 1, 2, 3, &  ! Front Face
                     4, 5, 1, 0, &  ! Top Face
                     3, 2, 6, 7, &  ! Bottom Face
                     5, 4, 7, 6, &  ! Back Face
                     1, 5, 6, 2, &  ! Right Face
                     4, 0, 3, 7  /) ! Left Face


end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 ) stop

end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)    xRot = xrot - 5.0
   if( key == GLUT_KEY_DOWN)  xRot = xrot + 5.0
   if( key == GLUT_KEY_LEFT)  yRot = yrot - 5.0
   if( key == GLUT_KEY_RIGHT) yRot = yrot + 5.0

   if( xrot > 356.0 )then
     xRot = 0.0
   else if( xrot < -1.0 )then
     xRot = 355.0
   endif

   if( yrot > 356.0 )then
     yRot = 0.0
   else if( yrot < -1.0 )then
     yRot = 355.0
   endif

   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine ChangeSize(win, hin) bind(C)

   use OpenGL_GL
   use OpenGL_GLu

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

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)
   call gluPerspective(dble(35.0),aspect,dble(1.0),dble(1000.))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize


