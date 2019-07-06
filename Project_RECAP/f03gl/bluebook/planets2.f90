!
! Planets.c 2nd
! OpenGL SuperBible
! Original program by Richard S. Wright Jr.
! rwright@starstonesoftware.com
!
module spheremod

   use opengl_gl
   
   enum, bind(C)
     enumerator :: SUN = 1, MERCURY, VENUS, EARTH, MARS
   end enum

   enum, bind(C)
     enumerator :: TORUS = 1, SPHERE
   end enum

   real(kind=GLfloat), dimension(4) :: dimLight     = (/   .1 ,   .1 ,  .1 , 1. /)
   real(kind=GLfloat), dimension(4) :: sourceLight  = (/   .65,   .65,  .65, 1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/  0.  ,  0.,   0.  , 1. /)

   integer, parameter :: BUFFER_LENGTH  =    64
   integer, parameter :: FEED_BUFF_SIZE = 32768

    type :: Rectangle
      integer :: top
      integer :: bottom
      integer :: left
      integer :: right
    end type Rectangle
      
    type(Rectangle) :: BoundingRect             ! Bounding rectangle
    integer(kind=GLuint) :: selectedObject = 0  ! Who is selected
    real(kind=GLfloat)   :: fAspect             ! Display aspect ratio
    
    real, parameter :: Pi = 3.14159265
    
end module spheremod
program planets2

   use opengl_gl
   use opengl_glut

   use spheremod
   
   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ProcessMenu(i)bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: i
     end subroutine ProcessMenu

     subroutine ChangeSize(w,h)bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y)bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     recursive subroutine TimerFunction(i)bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

     subroutine KeySpecialFunc(key, x,y)bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc

     subroutine MouseCallback(button,state,x,y)bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  ::button,state,x,y
     end subroutine MouseCallback
   end interface
     
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800,600)
   iwin = glutCreateWindow('Select an Object'//char(0))
   call glutKeyboardFunc(KeyPressFunc)
   call glutReshapeFunc(ChangeSize)
   call glutMouseFunc(MouseCallback)
   call glutDisplayFunc(RenderScene)
   
   call SetupRC
   
   call glutMainLoop()
   
end program
subroutine DrawTorus(numMajor,numMinor)
	
   use opengl_gl 
   use spheremod

   real :: majorRadius = 0.35
   real :: minorRadius = 0.15
   
   real :: majorStep, minorStep
   
   majorStep = 2.0*PI /float(numMajor)
   minorStep = 2.0*PI /float(numMinor)
   
   call glEnable(GL_NORMALIZE)
   do i=0,numMajor-1
     a0 = float(i) * majorStep
     a1 = a0 + majorStep
     x0 = cos(a0)
     y0 = sin(a0)
     x1 = cos(a1)
     y1 = sin(a1)

     call glBegin(GL_TRIANGLE_STRIP)
     do j=0,numMinor
       b = float(j) * minorStep
       c = cos(b)
       r = minorRadius * c + majorRadius
       z = minorRadius * sin(b)

       call glTexCoord2f(float(i)/float(numMajor), float(j)/float(numMinor))
       call glNormal3f(x0*c, y0*c, z/minorRadius)
       call glVertex3f(x0*r, y0*r, z)

       call glTexCoord2f(float(i+1)/float(numMajor), float(j)/float(numMinor))
       call glNormal3f(x1*c, y1*c, z/minorRadius)
       call glVertex3f(x1*r, y1*r, z)
     end do
     call glEnd
   end do
   call glDisable(GL_NORMALIZE)

end subroutine DrawTorus
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
subroutine DrawObjects

   use opengl_gl 
   use opengl_glu
   
   use spheremod

   ! Save the matrix state and do the rotations
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix

   ! Translate the whole scene out and into view  
   call glTranslatef(-0.75, 0.0, -2.5)	   

   ! Initialize the names stack
   call glInitNames()
   call glPushName(0)

   ! Set material color, Yellow torus
   call glColor3f(1.0, 1.0, 0.0)
   call glLoadName(TORUS)
   call glPassThrough(float(TORUS))
   call DrawTorus(40, 20)

   ! Draw Sphere
   call glColor3f(0.5, 0.0, 0.0)
   call glTranslatef(1.5, 0.0, 0.0)
   call glLoadName(SPHERE)
   call glPassThrough(float(SPHERE))
   call DrawSphere(0.5)

   ! Restore the matrix state
   call glPopMatrix             ! Modelview matrix

end subroutine DrawObjects
subroutine RenderScene() bind(C)

   use opengl_gl 
   use opengl_glu
   use opengl_glut
   
   use spheremod
   
   integer, dimension(4) :: viewport
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Draw the objects in the scene
   call DrawObjects

   ! If something is selected, draw a bounding box around it
   if(selectedObject /= 0)then
     ! Get the viewport
     call glGetIntegerv(GL_VIEWPORT, viewport)

     ! Remap the viewing volume to match window coordinates (approximately)
     call glMatrixMode(GL_PROJECTION)
     call glPushMatrix
     call glLoadIdentity
   
     ! Establish clipping volume (left, right, bottom, top, near, far)
     call glOrtho( dble(viewport(1)), dble(viewport(3)), &
                   dble(viewport(4)), dble(viewport(2)), &
		   dble(-1), dble(1) )
     call glMatrixMode(GL_MODELVIEW)

     call glDisable(GL_LIGHTING)
     call glColor3f(1.0, 0.0, 0.0)
     call glBegin(GL_LINE_LOOP)
       call glVertex2i(boundingRect%left, boundingRect%top)
       call glVertex2i(boundingRect%left, boundingRect%bottom)
       call glVertex2i(boundingRect%right, boundingRect%bottom)
       call glVertex2i(boundingRect%right, boundingRect%top)
     call glEnd
     call glEnable(GL_LIGHTING)
   endif

   call glMatrixMode(GL_PROJECTION)
   call glPopMatrix
   call glMatrixMode(GL_MODELVIEW)

   ! Restore the matrix state
   call glPopMatrix             ! Modelview matrix

   call glutSwapBuffers
  
end subroutine renderscene
subroutine MakeSelection(nChoice)

   use opengl_gl 
   use opengl_glu
   use opengl_glut
   
   use spheremod  

   ! Space for the feedback buffer
   real(kind=GLfloat), save, target :: feedBackBuff(FEED_BUFF_SIZE)

   ! Storage for counters, etc.
   integer :: size, i , j, count

   ! Initial minimum and maximum values
   boundingRect%right  =  -999999
   boundingRect%bottom = boundingRect%right  
   boundingRect%left =   999999
   boundingRect%top  = boundingRect%left 

   ! Set the feedback buffer
   call glFeedbackBuffer(FEED_BUFF_SIZE,GL_2D,c_loc(feedBackBuff))

   j = glRenderMode(GL_FEEDBACK)   ! Enter feedback mode
   call DrawObjects                ! Redraw the scene
   size = glRenderMode(GL_RENDER)  ! Leave feedback mode
   
   ! Parse the feedback buffer and get the
   ! min and max X and Y window coordinates
   i = 1
   do while( i <= size )
     ! Search for appropriate token
     if( feedBackBuff(i) == GL_PASS_THROUGH_TOKEN )then
       if( feedBackBuff(i+1) == float(nChoice) )then
         i = i + 2
         ! Loop until next token is reached
         do while( i <= size .and. &
	           feedBackBuff(i) /= GL_PASS_THROUGH_TOKEN )
   	   ! Just get the polygons
   	   if( feedBackBuff(i) == GL_POLYGON_TOKEN )then
   	     ! Get all the values for this polygon
   	     i= i + 1
   	     count = int(feedBackBuff(i))   ! How many vertices
   	     i= i + 1

   	     do j=0,count-1	            ! Loop for each vertex
   	       ! Min and Max X
   	       if(feedBackBuff(i) > boundingRect%right) &
   	  	       boundingRect%right = int(feedBackBuff(i))

   	       if(feedBackBuff(i) < int(boundingRect%left)) &
   	  	       boundingRect%left = int(feedBackBuff(i))
   	       i = i + 1

   	       ! Min and Max Y
   	       if(feedBackBuff(i) > boundingRect%bottom) &
   	  	       boundingRect%bottom = int(feedBackBuff(i))

   	       if( feedBackBuff(i) < boundingRect%top) &
   	  	       boundingRect%top = int(feedBackBuff(i))
   	       i = i + 1
	     end do
   	   else
   	     i = i + 1  ! Get next index and keep looking
   	   endif
         end do
         exit
       endif
     endif
     i = i + 1
   end do

end subroutine MakeSelection
subroutine ProcessSelection(xPos, yPos)

   use opengl_gl 
   use opengl_glu
   use opengl_glut
   
   use spheremod

   real, intent(in) :: xPos, yPos
 	
   ! Space for selection buffer
   integer(kind=GLuint), save, target :: selectBuff(BUFFER_LENGTH)

   ! Hit counter and viewport storage
   integer(kind=GLint) :: hits, viewport(4)

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
   call gluPerspective(dble(45.0), dble(fAspect), dble(1.0), dble(425.0))

   ! Draw the scene
   call DrawObjects

   ! Collect the hits
   hits = glRenderMode(GL_RENDER)
   
   ! Restore the projection matrix
   call glMatrixMode(GL_PROJECTION)
   call glPopMatrix

   ! Go back to modelview for normal rendering
   call glMatrixMode(GL_MODELVIEW)
 
   ! If a single hit occurred, display the info.
   if( hits == 1 )then
     call MakeSelection(selectBuff(4))
     if( selectedObject == selectBuff(4) )then
       selectedObject = 0
     else
       selectedObject = selectBuff(4)
     endif
   endif
   
   call glutPostRedisplay

end subroutine ProcessSelection
subroutine SetupRC 

   use opengl_gl 
   
   use spheremod

   ! Light values and coordinates
   call glEnable(GL_DEPTH_TEST)	   ! Hidden surface removal
   call glFrontFace(GL_CCW)	   ! Counter clock-wise polygons face out
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
   call glLineWidth(2.0)

end subroutine SetupRC 
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu
   
   use spheremod
 
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h

   w = win
   h = hin
   
   if( h == 0 ) h = 1
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   faspect = float(w)/float(h)

   ! Produce the perspective projection
   call gluPerspective(dble(60.), dble(fAspect), dble(1.), dble(425.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine MouseCallback(button,state,x,y) bind(C)

   use opengl_gl
   use opengl_glut

   integer(kind=GLint), intent(in), value  ::button,state,x,y
   
   if( button == GLUT_LEFT_BUTTON .and. &
       state  == GLUT_DOWN ) call ProcessSelection(float(x), float(y))

end subroutine MouseCallback
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
