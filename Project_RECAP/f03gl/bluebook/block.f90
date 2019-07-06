!
! block.f90
! OpenGL SuperBible, Chapter 1
! Demonstrates an assortment of basic 3D concepts
! Original program by Richard S. Wright Jr.
!
program block

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

   integer(kind=GLuint), dimension(4) :: textures
   real, dimension(16)   :: mCubeTransform
   real, dimension(4)    :: pPlane
   integer :: nStep 

   common nStep, textures, mCubeTransform, pPlane

   nStep = 0
   
   write(*,*)'3D effects demo'
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGBA,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('3D Effects Demo'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop()
   call glDeleteTextures(4,textures)

end program
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut
  
   integer(kind=GLubyte), parameter :: i255 = z'FF'

   real(GLfloat), dimension(4) :: lightAmbient   = (/ 0.2, 0.2, 0.2, 1.0 /)
   real(GLfloat), dimension(4) :: lightDiffuse   = (/ 0.7, 0.7, 0.7, 1.0 /)
   real(GLfloat), dimension(3) :: lightSpecular  = (/ 0.9, 0.9, 0.9 /)
   real(GLfloat), dimension(3) :: materialColor  = (/ 0.8, 0.0, 0.0 /)
   real(GLfloat), dimension(4) :: vLightPos      = (/-80.,120.,100., 0.0 /)

   real(GLfloat), dimension(3,3) :: Ground 
   
   integer(kind=GLuint), dimension(4) :: textures
   real, dimension(16)   :: mCubeTransform
   real, dimension(4)    :: pPlane
   integer :: nStep 

   common nStep, textures, mCubeTransform, pPlane

   Ground(1,:) = (/  0.0, -25.0,  0.0 /)
   Ground(2,:) = (/ 10.0, -25.0,  0.0 /)
   Ground(3,:) = (/ 10.0, -25.0,-10.0 /)
                  
   write(*,*)'RenderScene'
   
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
   call glShadeModel(GL_SMOOTH)
   call glEnable(GL_NORMALIZE)

   call glPushMatrix

   ! Draw plane that the cube rests on
   call glDisable(GL_LIGHTING);
   if( nStep == 5)then
     write(*,*)'Textures'
     call glColor3ub(i255,i255,i255)
     call glEnable(GL_TEXTURE_2D)
     call glBindTexture(GL_TEXTURE_2D, textures(1))
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0, 0.0)
       call glVertex3f(-100.0, -25.3, -100.0)
       call glTexCoord2f(0.0, 1.0)
       call glVertex3f(-100.0, -25.3, 100.0)
       call glTexCoord2f(1.0, 1.0)
       call glVertex3f(100.0,  -25.3, 100.0)
       call glTexCoord2f(1.0, 0.0)
       call glVertex3f(100.0,  -25.3, -100.0)
     call glEnd 
   else
     call glColor3f(0.0, 0.0, 0.90) ! Blue
     call glBegin(GL_QUADS)
       call glVertex3f(-100.0, -25.3, -100.0)
       call glVertex3f(-100.0, -25.3,  100.0)
       call glVertex3f( 100.0, -25.3,  100.0)
       call glVertex3f( 100.0, -25.3, -100.0)
     call glEnd
   endif
   
   ! Set drawing color to Red
   call glColor3f(1.0, 0.0, 0.0)

   ! Enable, disable lighting
   if( nStep > 2 )then
     write(*,*)'Lighting'
     call glEnable(GL_DEPTH_TEST)
     call glDepthFunc(GL_LEQUAL)
     call glEnable(GL_COLOR_MATERIAL)

     call glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmbient)
     call glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiffuse)
     call glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpecular)
     call glEnable(GL_LIGHTING)
     call glEnable(GL_LIGHT0)
     call glMaterialfv(GL_FRONT, GL_SPECULAR,lightSpecular)
     call glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, materialColor)
     call glMateriali(GL_FRONT, GL_SHININESS,128)
   endif
   
   ! Move the cube slightly forward and to the left
   call glTranslatef(-10.0, 0.0, 10.0)
   
   select case(nStep)          
     ! Just draw the wire framed cube
     case(0)

       write(*,*)'Wire cube'
       call glutWireCube(dble(50.0))
 
     ! Same wire cube with hidden line removal simulated
     case(1)

       write(*,*)'Hidden line cube'
       ! Front Face (before rotation)
       call glBegin(GL_LINES)
         call glVertex3f( 25.0, 25.0, 25.0)
         call glVertex3f( 25.0,-25.0, 25.0)

         call glVertex3f( 25.0,-25.0, 25.0)
         call glVertex3f(-25.0,-25.0, 25.0)

         call glVertex3f(-25.0,-25.0, 25.0)
         call glVertex3f(-25.0, 25.0, 25.0)

         call glVertex3f(-25.0, 25.0, 25.0)
         call glVertex3f( 25.0, 25.0, 25.0)
       call glEnd();

       ! Top of cube
       call glBegin(GL_LINES)
         ! Front Face
         call glVertex3f( 25.0, 25.0, 25.0)
         call glVertex3f( 25.0, 25.0,-25.0)

         call glVertex3f( 25.0, 25.0,-25.0)
         call glVertex3f(-25.0, 25.0,-25.0)

         call glVertex3f(-25.0, 25.0,-25.0)
         call glVertex3f(-25.0, 25.0, 25.0)

         call glVertex3f(-25.0, 25.0, 25.0)
         call glVertex3f( 25.0, 25.0, 25.0)
       call glEnd

       ! Last two segments for effect
       call glBegin(GL_LINES)
         call glVertex3f( 25.0, 25.0,-25.0)
         call glVertex3f( 25.0,-25.0,-25.0)

         call glVertex3f( 25.0,-25.0,-25.0)
         call glVertex3f( 25.0,-25.0, 25.0)
       call glEnd

     ! Uniform colored surface, looks 2D and goofy
     case(2)

       write(*,*)'Solid cube'
       call glutSolidCube(dble(50.0))

     case(3)

       write(*,*)'Solid lit cube'
       call glutSolidCube(dble(50.0))
 
     ! Draw a shadow with some lighting
     case(4)

       write(*,*)'Shadowed lit cube'
       call glGetFloatv(GL_MODELVIEW_MATRIX, mCubeTransform)
       call glutSolidCube(dble(50.0))
       call glPopMatrix

       ! Disable lighting, we'll just draw the shadow as black
       call glDisable(GL_LIGHTING)

       call glPushMatrix

       call m3dGetPlaneEquation(pPlane, ground(1,:), ground(2,:), ground(3,:))
       call m3dMakePlanarShadowMatrix(mCubeTransform, pPlane, vLightPos)
       
       call glMultMatrixf(mCubeTransform)

       call glTranslatef(-10.0, 0.0, 10.0)

       ! Set drawing color to Black
       call glColor3f(0.0, 0.0, 0.0)

       call glutSolidCube(dble(50.0))

     case(5)
  
       write(*,*)'Textured cube'
       call glColor3ub(i255,i255,i255)
       call glGetFloatv(GL_MODELVIEW_MATRIX, mCubeTransform)

       ! Front Face (before rotation)
       call glBindTexture(GL_TEXTURE_2D, textures(2))
       call glBegin(GL_QUADS)
         call glTexCoord2f(1.0, 1.0)
         call glVertex3f(25.0,25.0,25.0)
         call glTexCoord2f(1.0, 0.0)
         call glVertex3f(25.0,-25.0,25.0)
         call glTexCoord2f(0.0, 0.0)
         call glVertex3f(-25.0,-25.0,25.0)
         call glTexCoord2f(0.0, 1.0)
         call glVertex3f(-25.0,25.0,25.0)
       call glEnd

       ! Top of cube
       call glBindTexture(GL_TEXTURE_2D, textures(3))
       call glBegin(GL_QUADS)
         ! Front Face
         call glTexCoord2f(0.0, 0.0)
         call glVertex3f(25.0,25.0,25.0)
         call glTexCoord2f(1.0, 0.0)
         call glVertex3f(25.0,25.0,-25.0)
         call glTexCoord2f(1.0, 1.0)
         call glVertex3f(-25.0,25.0,-25.0)
         call glTexCoord2f(0.0, 1.0)
         call glVertex3f(-25.0,25.0,25.0)
       call glEnd

       ! Last two segments for effect
       call glBindTexture(GL_TEXTURE_2D, textures(4))
       call glBegin(GL_QUADS)
         call glTexCoord2f(1.0, 1.0)
         call glVertex3f(25.0,25.0,-25.0)
         call glTexCoord2f(1.0, 0.0)
         call glVertex3f(25.0,-25.0,-25.0)
         call glTexCoord2f(0.0, 0.0)
         call glVertex3f(25.0,-25.0,25.0)
         call glTexCoord2f(0.0, 1.0)
         call glVertex3f(25.0,25.0,25.0)
       call glEnd

       call glPopMatrix

       ! Disable lighting, we'll just draw the shadow as black
       call glDisable(GL_LIGHTING)
       call glDisable(GL_TEXTURE_2D)

       call glPushMatrix

       call m3dGetPlaneEquation(pPlane, ground(1,:), ground(2,:), ground(3,:))
       call m3dMakePlanarShadowMatrix(mCubeTransform, pPlane, vLightPos)
       call glMultMatrixf(mCubeTransform)

       call glTranslatef(-10.0, 0.0, 10.0)

       ! Set drawing color to Black
       call glColor3f(0.0, 0.0, 0.0)
       call glutSolidCube(dble(50.0))

   end select

   call glPopMatrix

   ! Flush drawing commands
   call glutSwapBuffers

   write(*,*)'RenderScene done'

end subroutine RenderScene
subroutine SetupRC

   use opengl_gl

   interface
     function fgltLoadTGA(FileName,iw,ih,ic,eform,image) 
       use, intrinsic :: iso_c_binding
       type(C_PTR), target :: fgltLoadTGA
       character(len=*), intent(in) :: FileName
       integer(C_INT), intent(out)  :: iw,ih 
       integer(C_INT), intent(out)  :: ic,eform
       integer(C_CHAR), dimension(:), allocatable, intent(out), target :: image
     end function
   end interface

   type(C_PTR) :: img1, img2, img3, img4 
   
   integer(kind=C_INT) :: i1 = 0
   integer(kind=C_INT) :: i2 = 0
   
   integer(kind=C_INT) :: nWidth, nHeight
   integer(kind=C_INT) :: nComponents
   integer(kind=C_INT) :: imformat
   integer(C_CHAR), dimension(:), allocatable :: image

   integer(kind=GLuint), dimension(4) :: textures
   real, dimension(16)   :: mCubeTransform
   real, dimension(4)    :: pPlane
   integer :: nStep 

   common nStep, textures, mCubeTransform, pPlane

   write(*,*)'SetupRC'
   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0)

   call glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE, GL_MODULATE)
   call glGenTextures(4, textures)

   write(*,*)'Loading first texture'
   ! Load the texture objects
  !img1 = gltLoadTGA('data/floor.tga'//char(0), nWidth, nHeight, nComponents, imformat)
   img1 = fgltLoadTGA('data/floor.tga', nWidth, nHeight, nComponents, &
                       imformat, image)
   write(*,*)'Loaded first texture'
   
   call glBindTexture(GL_TEXTURE_2D, textures(1))
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glTexImage2D(GL_TEXTURE_2D,i1,nComponents,nWidth, nHeight, i2, &
                     imformat, GL_UNSIGNED_BYTE, img1)

   if( allocated(image) )then
     deallocate(image)
     img1 = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

  !img2 = gltLoadTGA('data/Block4.tga'//char(0), &
  !                   nWidth, nHeight, nComponents, imformat)
   img2 = fgltLoadTGA('data/Block4.tga', nWidth, nHeight, nComponents, &
                       imformat, image)

   call glBindTexture(GL_TEXTURE_2D, textures(2))
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glTexImage2D(GL_TEXTURE_2D,0,nComponents,nWidth, nHeight, 0, &
                     imformat, GL_UNSIGNED_BYTE, img2)

   if( allocated(image) )then
     deallocate(image)
     img1 = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

  !img3 = gltLoadTGA('data/Block5.tga'//char(0), &
  !                   nWidth, nHeight, nComponents, imformat)
   img3 = fgltLoadTGA('data/Block5.tga', nWidth, nHeight, nComponents, &
                       imformat, image)

   call glBindTexture(GL_TEXTURE_2D, textures(3))
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glTexImage2D(GL_TEXTURE_2D,0,nComponents,nWidth, nHeight, 0, &
                     imformat, GL_UNSIGNED_BYTE, img3)

   if( allocated(image) )then
     deallocate(image)
     img3 = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

  !img4 = gltLoadTGA('data/Block6.tga'//char(0), &
  !                   nWidth, nHeight, nComponents, imformat)
   img4 = fgltLoadTGA('data/Block6.tga', nWidth, nHeight, nComponents, &
                       imformat, image)

   call glBindTexture(GL_TEXTURE_2D, textures(4))
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glTexImage2D(GL_TEXTURE_2D,0,nComponents,nWidth, nHeight, 0, &
                     imformat, GL_UNSIGNED_BYTE, img4)

   if( allocated(image) )then
     deallocate(image)
     img4 = C_NULL_PTR
     write(*,*)'image deallocated'
   endif
   
   write(*,*)'SetupRC done'

end subroutine setuprc
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   integer(kind=GLuint), dimension(4) :: textures
   real, dimension(16)   :: mCubeTransform
   real, dimension(4)    :: pPlane
   integer :: nStep 

   common nStep, textures, mCubeTransform, pPlane

   write(*,*)'KeyPressFunc'
   if( key == 32 )then
     nStep = nStep + 1
     if( nStep > 6 ) nStep = 0
   endif

   if( key == 27 ) stop

   ! Refresh the Window
   call glutPostRedisplay
   
   write(*,*)'KeyPressFunc done'

end subroutine KeyPressFunc
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
 
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, windowWidth, windowHeight, Aspect

   real(GLfloat), dimension(4) :: vLightPos      = (/-80.,120.,100., 0.0 /)

   write(*,*)'ChangeSize'
   w = win
   h = hin
   
   ! Prevent a divide by zero, when window is too short
   ! (you cant make a window of zero width).
   if( h == 0 ) h = 1

   ! Keep the square square
   if( w <= h )then
     windowHeight = 100.0*float(h)/float(w)
     windowWidth  = 100.0
   else
     windowWidth  = 100.0*float(w)/float(h)
     windowHeight = 100.0
   endif
   ! Set the viewport to be the entire window
   call glViewport(0, 0, w, h)

   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   ! Set the clipping volume
   call glOrtho(dble(-100.0), windowWidth, &
                dble(-100.0), windowHeight,&
                dble(-200.0), dble(200.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

   call glLightfv(GL_LIGHT0,GL_POSITION, vLightPos)

   call glRotatef(30.0, 1.0, 0.0, 0.0)
   call glRotatef(330.0, 0.0, 1.0, 0.0)

   write(*,*)'ChangeSize done'

end subroutine ChangeSize 
   

