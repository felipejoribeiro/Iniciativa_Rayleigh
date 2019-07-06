!
! Spot.f90
! OpenGL SuperBible
! Demonstrates OpenGL TSpotlight
! Original program by Richard S. Wright Jr.
!
program triangle

   use opengl_gl
   use opengl_glut

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

   real    :: xrot, yrot
   integer :: iShade, iTess, &
              MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
              MODE_MEDIUM, MODE_VERYHIGH

   common xrot, yrot,                           &
          iShade, iTess,                        &
          MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
          MODE_MEDIUM, MODE_VERYHIGH  

   mode_flat     = 1
   mode_smooth   = 2
   mode_verylow  = 3
   mode_medium   = 4
   mode_veryhigh = 5
   
   xrot = 0.0
   yrot = 0.0
   
   iShade = mode_flat
   iTess  = mode_verylow
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Spot Light'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Flat Shading'//char(0),1)
   call glutAddMenuEntry('Smooth Shading'//char(0),2)
   call glutAddMenuEntry('Very Low Tess'//char(0),3)
   call glutAddMenuEntry('Medium Tess'//char(0),4)
   call glutAddMenuEntry('Very High Tess'//char(0),5)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop

end program
subroutine SetupRC

   use OpenGL_GL

   real(kind=GLfloat), dimension(4) :: lightPos    = (/ .0, .0, 75.0, 1./) 
   real(kind=GLfloat), dimension(4) :: specular    = (/1.0,1.0,  1.0, 1./) 
   real(kind=GLfloat), dimension(4) :: specref     = (/1.0,1.0,  1.0, 1./) 
   real(kind=GLfloat), dimension(4) :: ambientLight= (/0.5,0.5,  0.5, 1./) 
   real(kind=GLfloat), dimension(3) :: spotdir     = (/ .0, .0, -1.0 /) 

   call glEnable(GL_DEPTH_TEST)    ! Hidden surface removal
   call glFrontFace(GL_CCW)        ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)     ! Do not try to display the back sides

   ! Enable lighting
   call glEnable(GL_LIGHTING)

   ! Setup and enable light 0
   ! Supply a slight ambient light so the objects can be seen
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambientLight)

   ! The light is composed of just a diffuse and specular components
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,ambientLight)
   call glLightfv(GL_LIGHT0,GL_SPECULAR,specular)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)

   ! Specific spot effects
   ! Cut off angle is 60 degrees
   call glLightf(GL_LIGHT0,GL_SPOT_CUTOFF,50.0)

   ! Enable this light in particular
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

   ! Set Material properties to followcall glColor values
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

   ! All materials hereafter have full specular reflectivity
   ! with a high shine
   call glMaterialfv(GL_FRONT,GL_SPECULAR,specref)
   call glMateriali(GL_FRONT,GL_SHININESS,128)

   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0 )
 
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

   integer(kind=GLint), intent(in), value  :: key, x, y

   real    :: xrot, yrot
   integer :: iShade, iTess, &
              MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
              MODE_MEDIUM, MODE_VERYHIGH

   common xrot, yrot,                           &
          iShade, iTess,                        &
          MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
          MODE_MEDIUM, MODE_VERYHIGH  

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

   ! Establish viewing volume 
   aspect = float(w)/float(h)

   call gluPerspective(dble(35.0), Aspect, dble(1.0), dble(500.0) )

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity
   call glTranslatef(0.0, 0.0, -250.0)

end subroutine ChangeSize
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   integer(kind=GLubyte), parameter :: i000 = z'00'
   integer(kind=GLubyte), parameter :: i064 = z'40'
   integer(kind=GLubyte), parameter :: i128 = z'80'
   integer(kind=GLubyte), parameter :: i192 = z'C0'
   integer(kind=GLubyte), parameter :: i255 = z'FF'

   real(kind=GLfloat), dimension(4) :: lightPos    = (/ .0, .0, 75.0, 1./) 
   real(kind=GLfloat), dimension(4) :: specular    = (/1.0,1.0,  1.0, 1./) 
   real(kind=GLfloat), dimension(4) :: specref     = (/1.0,1.0,  1.0, 1./) 
   real(kind=GLfloat), dimension(4) :: ambientLight= (/0.5,0.5,  0.5, 1./) 
   real(kind=GLfloat), dimension(3) :: spotdir     = (/ .0, .0, -1.0 /) 

   real    :: xrot, yrot
   integer :: iShade, iTess, &
              MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
              MODE_MEDIUM, MODE_VERYHIGH

   common xrot, yrot,                           &
          iShade, iTess,                        &
          MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
          MODE_MEDIUM, MODE_VERYHIGH  

   if( iShade == MODE_FLAT )then
     call glShadeModel(GL_FLAT)
   else ! iShade = MODE_SMOOTH
     call glShadeModel(GL_SMOOTH)
   endif

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! First place the light 
   ! Save the coordinate transformation
   call glPushMatrix()     
     ! Rotate coordinate system
     call glRotatef(yRot, 0.0, 1.0, 0.0)
     call glRotatef(xRot, 1.0, 0.0, 0.0)

     ! Specify new position and direction in rotated coords.
     call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
     call glLightfv(GL_LIGHT0,GL_SPOT_DIRECTION,spotDir)

     ! Draw a red cone to enclose the light source
     call glColor3ub(i255,i000,i000)    

     ! Translate origin to move the cone out to where the light
     ! is positioned.
     call glTranslatef(lightPos(1),lightPos(2),lightPos(3))
     call glutSolidCone(dble(4.0),dble(6.0),15,15)

     ! Draw a smaller displaced sphere to denote the light bulb
     ! Save the lighting state variables
     call glPushAttrib(GL_LIGHTING_BIT)

       ! Turn off lighting and specify a bright yellow sphere
       call glDisable(GL_LIGHTING)
       call glColor3ub(i255,i255,i000)
       call glutSolidSphere(dble(3.0), 15, 15)

     ! Restore lighting state variables
     call glPopAttrib()

   ! Restore coordinate transformations
   call glPopMatrix()

   ! Set material color and draw a sphere in the middle
   call glColor3ub(i000, i000, i255)

   if( iTess == MODE_VERYLOW )then
     call glutSolidSphere(dble(30.0), 7, 7)
   else 
     if( iTess == MODE_MEDIUM )then
       call glutSolidSphere(dble(30.0), 15, 15)
     else ! iTess = MODE_VERYHIGH
       call glutSolidSphere(dble(30.0), 50, 50)
     endif
   endif
   ! Display the results
   call glutSwapBuffers()
  
end subroutine RenderScene
subroutine ProcessMenu(i) bind(C)
	
   use opengl_glut

   integer, intent(in), value :: i

   real    :: xrot, yrot
   integer :: iShade, iTess, &
              MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
              MODE_MEDIUM, MODE_VERYHIGH

   common xrot, yrot,                           &
          iShade, iTess,                        &
          MODE_FLAT, MODE_SMOOTH, MODE_VERYLOW, &
          MODE_MEDIUM, MODE_VERYHIGH  

   select case(i)
      case(1)
	iShade = mode_flat
	
      case(2)
	iShade = mode_smooth

      case(3)
        iTess  = mode_verylow

      case(4)
        iTess  = mode_medium

      case(5)
        iTess  = mode_veryhigh
        
   end select

   call glutPostRedisplay
	
end subroutine ProcessMenu








