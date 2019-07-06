!
! Planets.c
! OpenGL SuperBible
! Origininal program by Richard S. Wright Jr.
! rwright@starstonesoftware.com
!
module spheremod

   use opengl_gl
   
   enum, bind(C)
     enumerator :: EARTH = 1, MARS, MOON1, MOON2
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
   iwin = glutCreateWindow('Pick a Planet or Moon'//char(0))
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

     ! Draw the Earth
     call glPushMatrix
     call glColor3f(0.0, 0.0, 1.0)
     call glTranslatef(-100.0,0.0,0.0)
     call glLoadName(EARTH)
     call DrawSphere(30.0)

     ! Draw the Moon
     call glTranslatef(45.0, 0.0, 0.0)
     call glColor3f(0.85, 0.85, 0.85)
     call glPushName(MOON1)
     call DrawSphere(5.0)
     call glPopName
     call glPopMatrix

     ! Draw Mars
     call glColor3f(1.0, 0.0, 0.0)
     call glPushMatrix()
     call glTranslatef(100.0, 0.0, 0.0)
     call glLoadName(MARS)
     call DrawSphere(20.0)

     ! Draw Moon1
     call glTranslatef(-40.0, 40.0, 0.0)
     call glColor3f(0.85, 0.85, 0.85)
     call glPushName(MOON1)
     call DrawSphere(5.0)
     call glPopName

     ! Draw Moon2
     call glTranslatef(0.0, -80.0, 0.0)
     call glPushName(MOON2)
     call DrawSphere(5.0)
     call glPopName
     call glPopMatrix
 
   ! Restore the matrix state
   call glPopMatrix             ! Modelview matrix

   call glutSwapBuffers
  
end subroutine renderscene
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine ProcessPlanet(pSelectBuff)

   use opengl_gl 
   use opengl_glut
   
   use spheremod

   integer(kind=GLuint), intent(in), dimension(*) :: pSelectBuff 
   character(len=64) :: str
   
   ! How many names on the name stack
   icount = pSelectBuff(1)

   ! Bottom of the name stack
   id = pSelectBuff(4)

   str = 'Error, no selection detected'
   select case(id)
     case(EARTH)
       str = 'You clicked Earth'

       ! If there is another name on the name stack,
       ! then it must be the moon that was selected
       ! This is what was actually clicked on
       if( icount == 2 ) str = str(1:lens(str))//' - Specifically the moon.'

     case(MARS)
       str = 'You clicked Mars'

       ! We know the name stack is only two deep. The precise
       ! moon that was selected will be here.
       if( icount == 2 )then
	 if( pSelectBuff(5) == MOON1 )then
           str = str(1:lens(str))//' - Specifically the moon 1.'
	 else
           str = str(1:lens(str))//' - Specifically the moon 2.'
         endif
       endif
   end select 

   call glutSetWindowTitle(str(1:lens(str))//char(0))

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
   ! in the vertical and horzontal direction. Remember OpenGL specifies the
   ! y coordinate from the bottom, Windows from the top. So windows position
   ! (as measured from the top) subtract the height and you get it in terms 
   ! OpenGL Likes.

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
   if( hits == 1 )then
     call ProcessPlanet(selectBuff)
   else
     call glutSetWindowTitle('You clicked empty space!//char(0)')
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


