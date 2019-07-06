!
! CubeMap.cpp
! OpenGL SuperBible
! Demonstrates a applying a cube map to an object (sphere) using
! texgen, and using the same map for the skybox applying the coordinates
! manually.
! Original program by Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   use opengl_glee

   use GLframes

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

   integer, parameter :: NUM_TEXTURES   = 6

   character(len=14), dimension(6) :: TexFiles = &
      (/ 'data/pos_x.tga', 'data/neg_x.tga', 'data/pos_y.tga', &
         'data/neg_y.tga', 'data/pos_z.tga', 'data/neg_z.tga'  /) 

   integer(kind=GLenum), dimension(6) :: cube = &
                      (/ GL_TEXTURE_CUBE_MAP_POSITIVE_X, &
                         GL_TEXTURE_CUBE_MAP_NEGATIVE_X, &
                         GL_TEXTURE_CUBE_MAP_POSITIVE_Y, &
                         GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, &
                         GL_TEXTURE_CUBE_MAP_POSITIVE_Z, &
                         GL_TEXTURE_CUBE_MAP_NEGATIVE_Z  /)

end module spheremod
program cubemap

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
     
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
        ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow &
          ('OpenGL Cube Maps'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)
   call glutTimerFunc(33, TimerFunction, 1)

   call setuprc
   
   call glutMainLoop

end program
subroutine SetupRC

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

   ! Cull backs of polygons
   call glCullFace(GL_BACK)
   call glFrontFace(GL_CCW)
   call glEnable(GL_CULL_FACE)
   call glEnable(GL_DEPTH_TEST)
       
   ! Set up texture maps        
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)        
  
   ! Load Cube Map images
   do i=1,6
     ! Load this texture map
     call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_GENERATE_MIPMAP, int(GL_TRUE))
       
     img(i) = fgltLoadTGA(TexFiles(i), nW, nH, nC, eform, image)
       
     call glTexImage2D(cube(i), 0, nC, nW, nH, 0, eForm, &
                       GL_UNSIGNED_BYTE, img(i) )
     !free(pBytes)
     if( allocated(image) )then
       deallocate(image)
       img(i) = C_NULL_PTR
       write(*,*)'image deallocated'
     endif
   end do
       
   call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   call glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   
   ! Enable cube mapping, and set texture environment to decal
   call glEnable(GL_TEXTURE_CUBE_MAP)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)

end subroutine setuprc
subroutine DrawSkyBox

   use OpenGL_GL
   
   fExtent = 15.0
   
   call glBegin(GL_QUADS)
     !!!!!!!!!!!!!!!!!!!!!!!
     ! Negative X
     call glTexCoord3f(-1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(-1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glTexCoord3f(-1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)
     
     call glTexCoord3f(-1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)


     !!!!!!!!!!!!!!!!!!!!!!!/
     !  Postive X
     call glTexCoord3f(1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
     
     call glTexCoord3f(1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)
     
     call glTexCoord3f(1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
 

     !!!!!!!!!!!!!!!!!!!!!!!!
     ! Negative Z 
     call glTexCoord3f(-1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glTexCoord3f(1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
     
     call glTexCoord3f(1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
     
     call glTexCoord3f(-1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)


     !!!!!!!!!!!!!!!!!!!!!!!!
     ! Positive Z 
     call glTexCoord3f(1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(-1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(-1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)
     
     call glTexCoord3f(1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)


     !!!!!!!!!!!!!!!!!!!!!!!!!
     ! Positive Y
     call glTexCoord3f(-1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)
     
     call glTexCoord3f(-1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)
     
     call glTexCoord3f(1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
     
     call glTexCoord3f(1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)
  
    
     !!!!!!!!!!!!!!!!!!!!!!!!!/
     ! Negative Y
     call glTexCoord3f(-1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glTexCoord3f(-1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glTexCoord3f(1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
   call glEnd

end subroutine DrawSkyBox
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut
   
   use spheremod
   
   real, dimension(4,4) :: m, invert, c
   
   ! Clear the window
   call glClear(ior(GL_COLOR_BUFFER_BIT , GL_DEPTH_BUFFER_BIT))
       
   call glPushMatrix
     call ApplyCameraTransform(frameCamera,.false.) ! Move the camera about

     ! Sky Box is manually textured
     call glDisable(GL_TEXTURE_GEN_S)
     call glDisable(GL_TEXTURE_GEN_T)
     call glDisable(GL_TEXTURE_GEN_R)        
     call DrawSkyBox

     ! Use texgen to apply cube map
     call glEnable(GL_TEXTURE_GEN_S)
     call glEnable(GL_TEXTURE_GEN_T)
     call glEnable(GL_TEXTURE_GEN_R)

     call glPushMatrix
       call glTranslatef(0.0, 0.0, -3.0)    
       
       call glMatrixMode(GL_TEXTURE)
       call glPushMatrix()
       
         ! Invert camera matrix (rotation only) and apply to 
         ! texture coordinates
         call GetCameraOrientation(frameCamera,m)
         call m3dInvertMatrix44(invert, m)
   
         call glMultMatrixf(invert)       
         call gltDrawSphere(0.75, 41, 41)
       
       call glPopMatrix
       call glMatrixMode(GL_MODELVIEW)
     call glPopMatrix
   call glPopMatrix()
       
   ! Do the buffer Swap
   call glutSwapBuffers
   
end subroutine RenderScene
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
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(2000.0))

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
     call glDeleteTextures(NUM_TEXTURES, cube)
     stop
   endif
   
end subroutine KeyPressFunc

