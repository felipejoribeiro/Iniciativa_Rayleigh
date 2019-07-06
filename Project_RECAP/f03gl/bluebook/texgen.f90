!
! TexGen.cpp
! OpenGL SuperBible
! Demonstrates OpenGL Texture Coordinate Generation
! Original program by Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   real :: xRot = 0.0
   real :: yRot = 0.0

   integer :: iRenderMode = 3   ! Sphere Mapped is default

   integer, parameter :: TEXTURE_COUNT = 2
   integer(kind=GLuint) :: toTextures(TEXTURE_COUNT)

end module spheremod
program sphereworld4

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
          ('Texture Coordinate Generation'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)

   call setuprc

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Object Linear'//char(0),1)
   call glutAddMenuEntry('Eye Linear'//char(0),2)
   call glutAddMenuEntry('Sphere Map'//char(0),3)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)
   
   call glutMainLoop

end program
subroutine ProcessMenu(in) bind(C)
	
   use opengl_glut

   use spheremod
   
   integer, intent(in), value :: in

   real, dimension(4) :: zPlane= (/ 0.0, 0.0, 1.0, 0.0 /)
   
   iRenderMode = in
   
   select case(in)
     case(1)
       ! Object Linear
       call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR)
       call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR)
       call glTexGenfv(GL_S, GL_OBJECT_PLANE, zPlane)
       call glTexGenfv(GL_T, GL_OBJECT_PLANE, zPlane)

     case(2)
       ! Eye Linear
       call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR)
       call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR)
       call glTexGenfv(GL_S, GL_EYE_PLANE, zPlane)
       call glTexGenfv(GL_T, GL_EYE_PLANE, zPlane)

     case(3)
       ! Sphere Map
       call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP)
       call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP)
    
   end select
   
   call glutPostRedisplay
	
end subroutine ProcessMenu
subroutine RenderScene() bind(C)

   use opengl_glu
   use OpenGL_GLut

   use spheremod
      
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Switch to orthographic view for background drawing
   call glMatrixMode(GL_PROJECTION)
   call glPushMatrix 
     call glLoadIdentity 
     call gluOrtho2D(dble(0.0), dble(1.0), dble(0.0), dble(1.0))
   
     call glMatrixMode(GL_MODELVIEW)
     call glBindTexture(GL_TEXTURE_2D, toTextures(2))  ! Background texture

     ! We will specify texture coordinates
     call glDisable(GL_TEXTURE_GEN_S)
     call glDisable(GL_TEXTURE_GEN_T)
   
     ! No depth buffer writes for background
     call glDepthMask(GL_FALSE)

     ! Background image
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0, 0.0)
       call glVertex2f  (0.0, 0.0)
   
       call glTexCoord2f(1.0, 0.0)
       call glVertex2f  (1.0, 0.0)
   
       call glTexCoord2f(1.0, 1.0)
       call glVertex2f  (1.0, 1.0)
       
       call glTexCoord2f(0.0, 1.0)
       call glVertex2f  (0.0, 1.0)
     call glEnd()

     ! Back to 3D land
     call glMatrixMode(GL_PROJECTION)
   call glPopMatrix
   call glMatrixMode(GL_MODELVIEW)

   ! Turn texgen and depth writing back on
   call glEnable(GL_TEXTURE_GEN_S)
   call glEnable(GL_TEXTURE_GEN_T)
   call glDepthMask(GL_TRUE)

   ! May need to swtich to stripe texture
   if(iRenderMode /= 3) &
       call glBindTexture(GL_TEXTURE_2D, toTextures(1))

       ! Save the matrix state and do the rotations
       call glPushMatrix
         call glTranslatef(0.0, 0.0, -2.0)
         call glRotatef(xRot, 1.0, 0.0, 0.0)
         call glRotatef(yRot, 0.0, 1.0, 0.0)

      ! Draw the tours
      call gltDrawTorus(0.35, 0.15, 61, 37)
               
   ! Restore the matrix state
   call glPopMatrix
   
   ! Display the results
   call glutSwapBuffers

end subroutine renderscene
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLu

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

   type(C_PTR), dimension(TEXTURE_COUNT) :: img 
   integer(kind=GLsizei) :: nW, nH
   integer(kind=GLenum)  :: nC 
   integer(kind=GLenum)  :: eform
   integer(C_CHAR),dimension(:), allocatable :: image
   
   call glEnable(GL_DEPTH_TEST)       ! Hidden surface removal
   call glFrontFace(GL_CCW)           ! Counter clock-wise polygons face out
   call glEnable(GL_CULL_FACE)        ! Do not calculate inside of jet

   ! White background
   call glClearColor(1.0, 1.0, 1.0, 1.0)
   
   ! Decal texture environment
   call glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL)
   
   ! Two textures
   call glGenTextures(2, toTextures)

   !!!!!!!!!!!!!!!!!!!!!
   ! Load the main texture
   call glBindTexture(GL_TEXTURE_2D, toTextures(1))

   img(1) = fgltLoadTGA('data/stripes.tga', &
                        nW, nH, nC, eForm,image)    
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm, &
                     GL_UNSIGNED_BYTE, img(1))
   !free(pBytes)
   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif
   
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glEnable(GL_TEXTURE_2D)

   !!!!!!!!!!!!!!!!!!!!!
   ! Load environment map
   call glBindTexture(GL_TEXTURE_2D, toTextures(2))
   img(2) = fgltLoadTGA('data/Environment.tga', &
                         nW, nH, nC, eForm,image)    
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm, &
                     GL_UNSIGNED_BYTE, img(2))
   !free(pBytes)
   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif
   
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
   call glEnable(GL_TEXTURE_2D)

   ! Turn on texture coordiante generation
   call glEnable(GL_TEXTURE_GEN_S)
   call glEnable(GL_TEXTURE_GEN_T)
   
   ! Sphere Map will be the default
   call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP)
   call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP)

end subroutine SetupRC
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
   real(kind=GLdouble)  :: Zero, One, Aspect

   w = win
   h = hin
   
   if( h == 0 ) h = 1
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluPerspective(dble(45.), Aspect, dble(1.), dble(255.0))

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
     call glDeleteTextures(TEXTURE_COUNT, totextures)
     stop
   endif
   
end subroutine KeyPressFunc
