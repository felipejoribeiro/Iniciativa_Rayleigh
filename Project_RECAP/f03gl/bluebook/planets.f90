!
! Planets.c
! OpenGL SuperBible
! Original program by Richard S. Wright Jr.
! rwright@starstonesoftware.com
!
module spheremod

   use opengl_gl
   
   enum, bind(C)
     enumerator :: SUN = 1, MERCURY, VENUS, EARTH, MARS
   end enum

   real(kind=GLfloat), dimension(4) :: dimLight     = (/   .1 ,   .1 ,  .1 , 1. /)
   real(kind=GLfloat), dimension(4) :: sourceLight  = (/   .65,   .65,  .65, 1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/  0.  ,  0.,   0.  , 1. /)

   integer, parameter :: BUFFER_LENGTH = 64
   
end module spheremod
program planets

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

     subroutine MouseCallback(button,state,x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  ::button,state,x,y
     end subroutine MouseCallback
   end interface
     
   call glutInit 
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800,600)
   iwin = glutCreateWindow('Pick a Planet'//char(0))
   call glutKeyboardFunc(KeyPressFunc)
   call glutReshapeFunc(ChangeSize)
   call glutMouseFunc(MouseCallback)
   call glutDisplayFunc(RenderScene)
   
   call SetupRC
   
   call glutMainLoop
   
end program
subroutine DrawSphere(radius)

   use opengl_gl 
   use opengl_glu
   
   use spheremod
   
   type(C_PTR) :: pObj
   
   pObj = gluNewQuadric()
   call gluQuadricNormals(pObj, GLU_SMOOTH)
   call gluSphere(pObj, dble(radius), 26, 13)
   call gluDeleteQuadric(pObj)
   
end subroutine DrawSphere
subroutine RenderScene() bind(C)

   use opengl_gl 
   use opengl_glu
   use opengl_glut
   
   use spheremod
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the matrix state and do the rotations
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix

     ! Translate the whole scene out and into view  
     call glTranslatef(0.0, 0.0, -300.0)      

     ! Initialize the names stack
     call glInitNames
     call glPushName(0)

     ! Name and draw the Sun
     call glColor3f(1.0, 1.0, 0.0)
     call glLoadName(SUN)
     call DrawSphere(15.0)

     ! Draw Mercury
     call glColor3f(0.5, 0.0, 0.0)
     call glPushMatrix
       call glTranslatef(24.0, 0.0, 0.0)
       call glLoadName(MERCURY)
       call DrawSphere(2.0)
     call glPopMatrix

     ! Draw Venus
     call glColor3f(0.5, 0.5, 1.0)
     call glPushMatrix
       call glTranslatef(60.0, 0.0, 0.0)
       call glLoadName(VENUS)
       call DrawSphere(4.0)
     call glPopMatrix

     ! Draw the Earth
     call glColor3f(0.0, 0.0, 1.0)
     call glPushMatrix
       call glTranslatef(100.0,0.0,0.0)
       call glLoadName(EARTH)
       call DrawSphere(8.0)
     call glPopMatrix

     ! Draw Mars
     call glColor3f(1.0, 0.0, 0.0)
     call glPushMatrix
       call glTranslatef(150.0, 0.0, 0.0)
       call glLoadName(MARS)
       call DrawSphere(4.0)
     call glPopMatrix

   ! Restore the matrix state
   call glPopMatrix             ! Modelview matrix

   call glutSwapBuffers
  
end subroutine renderscene
subroutine ProcessPlanet(id)

   use opengl_gl 
   use opengl_glut
   
   use spheremod

   select case(id)
     case(SUN)
       call glutSetWindowTitle('You clicked on the Sun!'//char(0))
 
     case(MERCURY)
       call glutSetWindowTitle('You clicked on Mercury!'//char(0))

     case(VENUS)
       call glutSetWindowTitle('You clicked on Venus!'//char(0))

     case(EARTH)
       call glutSetWindowTitle('You clicked on Earth!'//char(0))

     case(MARS)
       call glutSetWindowTitle('You clicked on Mars!'//char(0))

     case default
       call glutSetWindowTitle('Nothing was clicked on!'//char(0))

   end select 

end subroutine ProcessPlanet 
subroutine ProcessSelection(xPos, yPos)

   use opengl_gl 
   use opengl_glu
   use opengl_glut
   
   use spheremod

   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene
   end interface
   
   real, intent(in) :: xPos, yPos
 	
   real(kind=GLfloat) :: Aspect

   ! Space for selection buffer
   integer(kind=GLuint), save, target :: selectBuff(BUFFER_LENGTH)

   ! Hit counter and viewport storage
   integer(kind=GLint) :: hits, viewport(4), nErr

   ! Setup selection buffer
   call glSelectBuffer(BUFFER_LENGTH, c_loc(selectBuff))

   ! Get the viewport
   call glGetIntegerv(GL_VIEWPORT, viewport)

   ! Switch to projection and save the matrix
   call glMatrixMode(GL_PROJECTION)
   call glPushMatrix

   ! Change render mode
   mode = glRenderMode(GL_SELECT)

   ! Establish new clipping volume to be unit cube around
   ! mouse cursor point (xPos, yPos) and extending two pixels
   ! in the vertical and horizontal direction
   call glLoadIdentity
   call gluPickMatrix(dble(xPos),dble(viewport(4) - yPos + viewport(2)), &
                      dble(2.0), dble(2.0), viewport)

   ! Apply perspective matrix 
   Aspect = float(viewport(3)) / float(viewport(4))
   call gluPerspective(dble(45.0), dble(Aspect), dble(1.0), dble(425.0))

   ! Draw the scene
   call RenderScene()

   ! Collect the hits
   hits = glRenderMode(GL_RENDER)

   nErr = glGetError()
   ! If a single hit occurred, display the info.
   if(hits == 1)then
     call ProcessPlanet(selectBuff(4))
   else
     call glutSetWindowTitle('Nothing was clicked on!')
   endif
   
   ! Restore the projection matrix
   call glMatrixMode(GL_PROJECTION)
   call glPopMatrix

   ! Go back to modelview for normal rendering
   call glMatrixMode(GL_MODELVIEW)
 
end subroutine ProcessSelection
subroutine MouseCallback(button,state,x,y) bind(C)

   use opengl_gl
   use opengl_glut

   integer(kind=GLint), intent(in), value  ::button,state,x,y
   
   if( button == GLUT_LEFT_BUTTON .and. &
       state  == GLUT_DOWN ) call ProcessSelection(float(x), float(y))

end subroutine MouseCallback
subroutine SetupRC 

   use opengl_gl 
   
   use spheremod

   ! Light values and coordinates
   call glEnable(GL_DEPTH_TEST)	   ! Hidden surface removal
   call glFrontFace(GL_CCW)	           ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)	   ! Do not calculate insides

   ! Enable lighting
   call glEnable(GL_LIGHTING)

   ! Setup and enable light 0
   call glLightfv(GL_LIGHT0, GL_AMBIENT, dimLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,sourceLight)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

   ! Set Material properties to follow glColor values
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)

   ! Gray background
   call glClearColor(0.60, 0.60, 0.60, 1.0)

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

   aspect = float(w)/float(h)

   ! Produce the perspective projection
   call gluPerspective(dble(45.), Aspect, dble(1.), dble(425.0))

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


