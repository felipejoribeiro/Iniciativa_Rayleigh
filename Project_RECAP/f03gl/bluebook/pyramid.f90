!
! Pyramid.cpp
! Demonstrates Simple Texture Mapping
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use OpenGL_GL

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .3,  .3,  .3,  1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/   .7,  .7,  .7,  1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/   1.,  1.,  1.,  1. /)
   real(kind=GLfloat), dimension(4) :: specref      = (/   1.,  1.,  1.,  1. /)

   real(kind=GLfloat), dimension(4) :: whitelight   = (/ 0.05,0.05,0.05,  1. /)
   real(kind=GLfloat), dimension(4) :: sourcelight  = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/ -10.,  5.,  5.,  1. /)

   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real :: xRot = 0.0
   real :: yRot = 0.0
   
end module spheremod
program pyramid

   use opengl_gl
   use opengl_glut

   use spheremod
   
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

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface
     
   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Textured Pyramid'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)

   call setuprc
   
   call glutMainLoop()

end program
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
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(40.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
subroutine SetupRC
	
   use OpenGL_GL

   use spheremod

   interface
     function gltLoadTGA(FileName,iw,ih,ic,eform) bind(c,name='gltLoadTGA')
       use, intrinsic :: iso_c_binding
       use opengl_gl
       type(C_PTR), target :: gltLoadTGA
       character, dimension(*), intent(IN) :: FileName
       integer(kind=GLint), intent(OUT)  :: iw,ih 
       integer(kind=GLenum), intent(OUT) :: ic,eform
     end function
   end interface
   
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

   type(C_PTR)  :: img
   integer :: iw,ih
   integer :: ic 
   integer :: eform, ierr
   integer(C_CHAR),dimension(:), allocatable :: image
   integer :: i0    = 0
   
   
   call glEnable(GL_DEPTH_TEST)    ! Hidden surface removal
   call glFrontFace(GL_CCW)        ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)     ! Do not calculate inside of jet

   ! Enable lighting
   call glEnable(GL_LIGHTING)

    ! Setup and enable light 0
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT,whiteLight)
   call glLightfv(GL_LIGHT0,GL_AMBIENT,sourceLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE,sourceLight)
   call glLightfv(GL_LIGHT0,GL_POSITION,lightPos)
   call glEnable(GL_LIGHT0)

    ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)

    ! Set Material properties to followcall glColor values
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)

    ! Black blue background
   call glClearColor(0.0, 0.0, 0.0, 1.0 )

    ! Load texture
   call glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
   
   img = fgltLoadTGA('data/stone.tga',         iW, iH, iC, eForm, image)
  !img = gltLoadTGA('data/stone.tga'//char(0), iW, iH, iC, eForm)
   
   !              GL:    enum      int   int sizei sizei    int    enum     enum            void
   !                     target   level form width height  border format    type           *pizels
   call glTexImage2D(GL_TEXTURE_2D, i0,  iC,  iW,   iH,     i0,   eForm, GL_UNSIGNED_BYTE, img)
   
   ! free(pBytes)
   if( allocated(image) )then
     deallocate(image)
     img = C_NULL_PTR
     write(*,*)'image deallocated'
   endif
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE)

   call glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)
   call glEnable(GL_TEXTURE_2D)

   
end subroutine SetupRC
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)    xrot = xrot - 5.0
   if( key == GLUT_KEY_DOWN)  xrot = xrot + 5.0
   if( key == GLUT_KEY_LEFT)  yrot = yrot - 5.0
   if( key == GLUT_KEY_RIGHT) yrot = yrot + 5.0

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
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod

   real, dimension(5,3) :: Corners
   real, dimension(3)   :: Normal

   Corners(1,1:3) = (/  0.0, 0.8,  0.0 /) ! Top           1
   Corners(2,1:3) = (/ -0.5, 0.0, -0.5 /) ! Back left     2
   Corners(3,1:3) = (/  0.5, 0.0, -0.5 /) ! Back right    3
   Corners(4,1:3) = (/  0.5, 0.0,  0.5 /) ! Front right   4
   Corners(5,1:3) = (/ -0.5, 0.0,  0.5 /) ! Front left    5
                              
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the matrix state and do the rotations
   call glPushMatrix
   
   ! Move object back and do in place rotation
   call glTranslatef(0.0, -0.25, -4.0)
   call glRotatef(xRot, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   ! Draw the Pyramid
   call glColor3f(1.0, 1.0, 1.0)
   call glBegin(GL_TRIANGLES)
   
     ! Bottom section - two triangles
     call glNormal3f(0.0, -1.0, 0.0)
     call glTexCoord2f(1.0, 1.0)
     call glVertex3fv(Corners(3,:))

     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(5,:))

     call glTexCoord2f(0.0, 1.0)
     call glVertex3fv(Corners(2,:))


     call glTexCoord2f(1.0, 1.0)
     call glVertex3fv(Corners(3,:))

     call glTexCoord2f(1.0, 0.0)
     call glVertex3fv(Corners(4,:))

     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(5,:))

     ! Front Face
     call m3dFindNormal(Normal, Corners(1,:), Corners(5,:), Corners(4,:))
     call glNormal3fv(Normal)
     call glTexCoord2f(0.5, 1.0)
     call glVertex3fv(Corners(1,:))
     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(5,:))
     call glTexCoord2f(1.0, 0.0)
     call glVertex3fv(Corners(4,:))

     ! Left Face
     call m3dFindNormal(Normal, Corners(1,:), Corners(2,:), Corners(5,:))
     call glNormal3fv(Normal)
     call glTexCoord2f(0.5, 1.0)
     call glVertex3fv(Corners(1,:))
     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(2,:))
     call glTexCoord2f(1.0, 0.0)
     call glVertex3fv(Corners(5,:))

     ! Back Face
     call m3dFindNormal(Normal, Corners(1,:), Corners(3,:), Corners(2,:))
     call glNormal3fv(Normal)
     call glTexCoord2f(0.5, 1.0)
     call glVertex3fv(Corners(1,:))

     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(3,:))

     call glTexCoord2f(1.0, 0.0)
     call glVertex3fv(Corners(2,:))

     ! Right Face
     call m3dFindNormal(Normal, Corners(1,:), Corners(4,:), Corners(3,:))
     call glNormal3fv(Normal)
     call glTexCoord2f(0.5, 1.0)
     call glVertex3fv(Corners(1,:))
     call glTexCoord2f(0.0, 0.0)
     call glVertex3fv(Corners(4,:))
     call glTexCoord2f(1.0, 0.0)
     call glVertex3fv(Corners(3,:))

   call glEnd()
   
   ! Restore the matrix state
   call glPopMatrix

   ! Buffer swap
   call glutSwapBuffers
    
end subroutine RenderScene
