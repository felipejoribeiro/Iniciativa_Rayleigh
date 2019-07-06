!
! SphereWorld5.cpp
! OpenGL SuperBible
! Demonstrates an immersive 3D environment using actors
! and a camera. This version adds lights and material properties
! and shadows.
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use GLframes

   integer, parameter :: NUM_SPHERES = 30

   type(GLframe), allocatable :: spheres(:)
   type(GLframe), save        :: frameCamera

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .3,   .3,  .3, 1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/   .7,   .7,  .7, 1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  1. ,  1. , 1. , 1. /)
   real(kind=GLfloat), dimension(4) :: specref      = (/  1. ,  1. , 1. , 1. /)

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-100.,100., 50.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real(kind=GLfloat), dimension(4) :: vplaneequation
   real(kind=GLfloat), dimension(16):: shadowmatrix

   real :: yRot = 0.0

   integer, parameter :: GROUND_TEXTURE = 1
   integer, parameter :: TORUS_TEXTURE  = 2
   integer, parameter :: SPHERE_TEXTURE = 3

   integer, parameter :: NUM_TEXTURES   = 3
   integer(kind=GLuint) :: textureObjects(NUM_TEXTURES)

   character(len=14), dimension(3) :: TexFiles = &
      (/ 'data/grass.tga', 'data/wood.tga ', 'data/orb.tga  ' /)

   ! Display list identifiers
   integer(kind=GLint) :: sphereList, groundList, torusList

   integer :: Method  = 0 ! Without display lists
   integer :: iFrames = 0 
   real    :: TimeElapsed = 0.0

end module spheremod
program sphereworld4

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
     
   TimeElapsed = secnds(0.0)
   allocate(spheres(NUM_SPHERES))
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
        ior(GLUT_RGB,ior(GLUT_DEPTH,GLUT_STENCIL))))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('OpenGL SphereWorld Demo + Texture Maps'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Without Display Lists'//char(0), 0)
   call glutAddMenuEntry('With Display Lists'//char(0),1)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call glutTimerFunc(33, TimerFunction, 1)

   call setuprc

   call glutMainLoop

end program
subroutine ProcessMenu(in) bind(C)
	
   use opengl_glut

   use spheremod
   
   integer, intent(in), value :: in

   Method = in
      
   call glutPostRedisplay
	
end subroutine ProcessMenu
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine SetupRC
	
   use OpenGL_GL
   use OpenGL_GLu
   use OpenGL_GLut

   use spheremod

   interface
     function fgltLoadTGA(FileName,iw,ih,ic,eform,image) 
       use, intrinsic :: iso_c_binding
       type(C_PTR), target :: fgltLoadTGA
       character, dimension(*), intent(IN) :: FileName
       integer(C_INT), intent(OUT)  :: iw,ih 
       integer(C_INT), intent(OUT) :: ic,eform
       integer(C_CHAR), dimension(:), allocatable, intent(out), target :: image
     end function
   end interface
 
   type(C_PTR), dimension(NUM_TEXTURES) :: img 
   integer(kind=GLsizei) :: nW, nH
   integer(kind=GLenum)  :: nC 
   integer(kind=GLenum)  :: eform
   integer(C_CHAR),dimension(:), allocatable :: image
  
   real, dimension(3,3) :: points
   
   points(1,:) = (/  0.0, -0.4,  0.0 /)
   points(2,:) = (/ 10.0, -0.4,  0.0 /)
   points(3,:) = (/  5.0, -0.4, -5.0 /)
         
   ! Grayish background
   call glClearColor(lowlight(1),lowlight(2),lowlight(3),lowlight(4))
         
   ! Clear stencil buffer with zero, increment by one whenever anybody
   ! draws into it. When stencil function is enabled, only write where
   ! stencil value is zero. This prevents the transparent shadow from drawing
   ! over itself
   call glStencilOp(GL_INCR, GL_INCR, GL_INCR)
   call glClearStencil(0)
   call glStencilFunc(GL_EQUAL, z'00', z'01')
   
   ! Cull backs of polygons
   call glCullFace(GL_BACK)
   call glFrontFace(GL_CCW)
   call glEnable(GL_CULL_FACE)
   call glEnable(GL_DEPTH_TEST)
  !call glEnable(GLUT_MULTISAMPLE)
  !call glEnable(GL_MULTISAMPLE_ARB)
    
   ! Setup light parameters
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, NoLight)
   call glLightfv(GL_LIGHT0, GL_AMBIENT, LowLight)
   call glLightfv(GL_LIGHT0, GL_DIFFUSE, BrightLight)
   call glLightfv(GL_LIGHT0, GL_SPECULAR, BrightLight)
   call glEnable(GL_LIGHTING)
   call glEnable(GL_LIGHT0)

   call glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR)
   
   ! Get the plane equation from three points on the ground
   call m3dGetPlaneEquation(vPlaneEquation,Points(1,:),Points(2,:),Points(3,:))

   ! Calculate projection matrix to draw shadow on the ground
   call m3dMakePlanarShadowMatrix(ShadowMatrix, vPlaneEquation, LightPos)
 
   ! Mostly use material tracking
   call glEnable(GL_COLOR_MATERIAL)
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
   call glMaterialfv(GL_FRONT, GL_SPECULAR, BrightLight)
   call glMateriali(GL_FRONT, GL_SHININESS, 128)

   ! Randomly place the sphere inhabitants
   do i=1,NUM_SPHERES 
     call random_number(r)
     x = (r - 0.5) * 40
     call random_number(r)
     z = (r - 0.5) * 40

     call SetOrigin(spheres(i), x,0.0,z)
   end do

   call SetOrigin(frameCamera,0.0,0.0,0.0)

   ! Set up texture maps
   call glEnable(GL_TEXTURE_2D)
   call glGenTextures(NUM_TEXTURES, textureObjects)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

   do i=1,NUM_TEXTURES

     call glBindTexture(GL_TEXTURE_2D, textureObjects(i))
     
     ! Load this texture map
     !write(*,*) i,' >',TexFiles(i)(1:lens(TexFiles(i))),'<'
     !img(i) = gltLoadTGA(TexFiles(i)(1:lens(TexFiles(i)))//char(0), &
     !                    nW, nH, nC, eform)

     img(i) = fgltLoadTGA(TexFiles(i), nW, nH, nC, eForm, image)

     j = gluBuild2DMipmaps(GL_TEXTURE_2D, nC, nW, nH, eForm, &
                           GL_UNSIGNED_BYTE, img(i))

     !free(pBytes)
     if( allocated(image) )then
       deallocate(image)
       img(i) = C_NULL_PTR
       write(*,*)'image deallocated'
     endif
     
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

   end do

   ! Create display lists
   groundList = glGenLists(3)
   sphereList = groundList + 1
   torusList  = sphereList + 1
   
   ! Create sphere display list
   call glNewList(sphereList, GL_COMPILE)
     call gltDrawSphere(0.1, 40, 20)
   call glEndList

   ! Create torus display list
   call glNewList(torusList, GL_COMPILE)
     call gltDrawTorus(0.35, 0.15, 61, 37)
   call glEndList
   
   ! Create the ground display list
   call glNewList(groundList, GL_COMPILE)
     call DrawGround()
   call glEndList
   
end subroutine SetupRC
subroutine DrawGround 
!
!  Draw a gridded ground
!	
   use OpenGL_GL

   use spheremod

   fExtent = 20.0
   fStep   =  1.0
   y       = -0.4

   s = 0.0
   t = 0.0
   texStep = 1.0/(fExtent * 0.075)
   
   call glBindTexture(GL_TEXTURE_2D, textureObjects(GROUND_TEXTURE))

  !write(*,*)'GL_REPEAT:',GL_REPEAT
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   
   x = -fExtent
   do while( x <= fExtent)
     t = 0.0
     call glBegin(GL_TRIANGLE_STRIP)
     
     z = fExtent
     do while( z >= -fExtent)
       call glTexCoord2f(s,t)
       call glNormal3f(0.0, 1.0, 0.0)  ! All Point up
       call glVertex3f( x,       y, z) 

       call glTexCoord2f(s + texStep, t)
       call glNormal3f(0.0, 1.0, 0.0)  ! All Point up
       call glVertex3f( x+fstep, y, z) 
       
       t = t + texstep
       z = z - fstep
     end do
     s = s + fstep
     x = x + fstep
     call glEnd
   end do
   

end subroutine DrawGround
recursive subroutine TimerFunction( ivalue ) bind(C)
!
! Called by GLUT library when idle (window not being
! resized or moved)
!
   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(3,TimerFunction, 1)

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

   use spheremod

   integer(kind=GLbyte), intent(in), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     call glDeleteLists(groundList, 3)
     call glDeleteTextures(NUM_TEXTURES, textureObjects)
     stop
   endif
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)    call MoveForward (frameCamera, 0.1)
   if( key == GLUT_KEY_DOWN)  call MoveForward (frameCamera,-0.1)
   if( key == GLUT_KEY_LEFT)  call RotateLocalY(frameCamera, 0.1)
   if( key == GLUT_KEY_RIGHT) call RotateLocalY(frameCamera,-0.1)
   
   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine DrawInhabitants(iShadow)

   use opengl_gl
   use opengl_glut

   use spheremod

   if( iShadow == 0 )then
     yrot = yrot + 0.5
     call glColor4f(1.0, 1.0, 1.0, 1.0)
   else
     call glColor4f(0.0, 0.0, 0.0, 0.6) ! Shadow color
   endif
      
   call glBindTexture(GL_TEXTURE_2D, textureObjects(SPHERE_TEXTURE))
   do i=1,NUM_SPHERES    
     call glPushMatrix
     call ApplyActorTransform(spheres(i))
     if( Method == 0 )then
       call gltDrawSphere(0.1, 40, 20);
     else
       call glCallList(sphereList);
     endif
     call glPopMatrix
   end do

   call glPushMatrix
     call glTranslatef(0.0, 0.1, -2.5) 
    
     call glPushMatrix 
       call glRotatef(-yRot * 2.0, 0.0, 1.0, 0.0) 
       call glTranslatef(1.0, 0.0, 0.0) 
       if( Method == 0 )then
         call gltDrawSphere(0.1, 40, 20)
       else
         call glCallList(sphereList)
       endif
     call glPopMatrix 
    
     if( iShadow == 0 )then
       ! Torus alone will be specular
       call glMaterialfv(GL_FRONT, GL_SPECULAR, BrightLight)
     endif
     
     call glRotatef(yRot, 0.0, 1.0, 0.0) 
     call glBindTexture(GL_TEXTURE_2D, textureObjects(TORUS_TEXTURE))
     if( Method == 0 )then   
       call gltDrawTorus(0.35, 0.15, 61, 37) 
     else
       call glCallList(torusList)
     endif
     call glMaterialfv(GL_FRONT, GL_SPECULAR, NoLight)
   call glPopMatrix 

end subroutine DrawInhabitants
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT, &
        ior(GL_DEPTH_BUFFER_BIT,GL_STENCIL_BUFFER_BIT)))

   call glPushMatrix
     call ApplyCameraTransform(frameCamera)
        
     ! Position light before any other transformations
     call glLightfv(GL_LIGHT0, GL_POSITION, LightPos)
        
     call glColor3f(1.0,1.0,1.0)   ! Draw the ground
     if( Method == 0 )then
       call DrawGround
     else
       call glCallList(groundList)
     endif
     
     ! Draw shadows first
     call glDisable(GL_DEPTH_TEST)
     call glDisable(GL_LIGHTING)
     call glDisable(GL_TEXTURE_2D)

     call glEnable(GL_BLEND)
     call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
     call glEnable(GL_STENCIL_TEST)

     call glPushMatrix
       call glMultMatrixf(ShadowMatrix)
       call DrawInhabitants(1)
     call glPopMatrix
     
     call glDisable(GL_STENCIL_TEST)
     call glDisable(GL_BLEND)

     call glEnable(GL_LIGHTING)
     call glEnable(GL_TEXTURE_2D)
     call glEnable(GL_DEPTH_TEST)

     ! Draw inhabitants normally
     call DrawInhabitants(0)
   call glPopMatrix 

   call glutSwapBuffers

   ! Do periodic frame rate calculation
   iFrames = iFrames + 1
   if( iFrames == 500 )then
     
     T0 = TimeElapsed
     TimeElapsed = secnds(0.0)
     dT = TimeElapsed - T0
     
     fps = 500.0 /(dT+1.e-6)
     if( Method == 0 )then
       write(*,*) 'OpenGL SphereWorld without Display Lists ',fps,' fps'
     else
       write(*,*) 'OpenGL SphereWorld with Display Lists ',fps,' fps'
     endif
     	
     !call glutSetWindowTitle(cBuffer);
     iFrames = 0
     
   endif

end subroutine RenderScene
