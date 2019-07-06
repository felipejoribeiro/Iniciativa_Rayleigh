!
! Tunnel.cpp
! Demonstrates mipmapping and using texture objects
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   real :: zPos = -12.0

   integer, parameter :: TEXTURE_BRICK   = 1
   integer, parameter :: TEXTURE_FLOOR   = 2
   integer, parameter :: TEXTURE_CEILING = 3

   integer, parameter :: TEXTURE_COUNT   = 3
   integer(kind=GLuint) :: textures(TEXTURE_COUNT)

   character*16, dimension(TEXTURE_COUNT) :: TexFiles = &
      (/ 'data/brick.tga  ', 'data/floor.tga  ', 'data/ceiling.tga' /)

end module spheremod
program tunnel

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

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface
        
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Tunnel'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('GL_NEAREST'//char(0),0)
   call glutAddMenuEntry('GL_LINEAR'//char(0),1)
   call glutAddMenuEntry('GL_NEAREST_MIPMAP_NEAREST'//char(0),2)
   call glutAddMenuEntry('GL_NEAREST_MIPMAP_LINEAR'//char(0),3)
   call glutAddMenuEntry('GL_LINEAR_MIPMAP_NEAREST'//char(0),4)
   call glutAddMenuEntry('GL_LINEAR_MIPMAP_LINEAR'//char(0),5)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutSpecialFunc(KeySpecialFunc)

   call setuprc
   
   call glutMainLoop

end program
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine ProcessMenu(in) bind(C)
	
   use opengl_glut

   use spheremod
   
   integer, intent(in), value :: in

   do i=1,TEXTURE_COUNT
     call glBindTexture(GL_TEXTURE_2D, textures(i))
   
     select case(in)
       case(0)
	 call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
	
       case(1)
	 call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
	
       case(2)
         call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST)
                
       case(3)
         call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR)
      
       case(4)
         call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST)
       
       case(5)
         call glTexParameteri &
         (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
         
     end select
   end do
   
   call glutPostRedisplay
	
end subroutine ProcessMenu
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

   ! Black background
   call glClearColor(0.0, 0.0, 0.0,1.0)

   ! Textures applied as decals, no lighting or coloring effects
   call glEnable(GL_TEXTURE_2D)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)

   ! Load textures
   call glGenTextures(TEXTURE_COUNT, textures)
   
   do i=1,TEXTURE_COUNT
     ! Bind to next texture object
     call glBindTexture(GL_TEXTURE_2D, textures(i))
     
     ! Load texture, set filter and wrap modes
     img(i) = fgltLoadTGA(TexFiles(i)(1:lens(TexFiles(i))), &
                          nW, nH, nC, eform, image)

     ! Load texture, set filter and wrap modes
     j = gluBuild2DMipmaps(GL_TEXTURE_2D, nC, nw, nh, eform, &
                           GL_UNSIGNED_BYTE, img(i))
     
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
     
     ! Don't need original texture data any more
     !free(pBytes);
     if( allocated(image) )then
       deallocate(image)
       img(i) = C_NULL_PTR
       write(*,*)'image deallocated'
     endif

   end do
      
end subroutine SetupRC
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     call glDeleteTextures(TEXTURE_COUNT, textures)
     stop
   endif
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)    zPos = zpos + 1.0
   if( key == GLUT_KEY_DOWN)  zPos = zpos - 1.0
   
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

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluPerspective(dble(90.), Aspect, dble(1.), dble(120.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
  
   call glClear(GL_COLOR_BUFFER_BIT)

   ! Save the matrix state and do the rotations
   call glPushMatrix()
   
   ! Move object back and do in place rotation
   call glTranslatef(0.0, 0.0, zPos)
    
   ! Floor
   do i=60,0,-10
     call glBindTexture(GL_TEXTURE_2D, textures(TEXTURE_FLOOR))
     call glBegin(GL_QUADS)     
       call glTexCoord2f(0.0, 0.0)
       call glVertex3f(-10.0, -10.0, z)

       call glTexCoord2f(1.0, 0.0)
       call glVertex3f(10.0, -10.0, z)

       call glTexCoord2f(1.0, 1.0)
       call glVertex3f(10.0, -10.0, z - 10.0)

       call glTexCoord2f(0.0, 1.0)
       call glVertex3f(-10.0, -10.0, z - 10.0)
     call glEnd

     ! Ceiling
     call glBindTexture(GL_TEXTURE_2D, textures(TEXTURE_CEILING))
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0, 1.0)
       call glVertex3f(-10.0, 10.0, z - 10.0)

       call glTexCoord2f(1.0, 1.0)
       call glVertex3f(10.0, 10.0, z - 10.0)

       call glTexCoord2f(1.0, 0.0)
       call glVertex3f(10.0, 10.0, z)

       call glTexCoord2f(0.0, 0.0)
       call glVertex3f(-10.0, 10.0, z)
     call glEnd
             
     ! Left Wall
     call glBindTexture(GL_TEXTURE_2D, textures(TEXTURE_BRICK))
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0, 0.0)
       call glVertex3f(-10.0, -10.0, z)

       call glTexCoord2f(1.0, 0.0)
       call glVertex3f(-10.0, -10.0, z - 10.0)

       call glTexCoord2f(1.0, 1.0)
       call glVertex3f(-10.0, 10.0, z - 10.0)

       call glTexCoord2f(0.0, 1.0)
       call glVertex3f(-10.0, 10.0, z)
     call glEnd

     ! Right Wall
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0, 1.0)
       call glVertex3f(10.0, 10.0, z)

       call glTexCoord2f(1.0, 1.0)
       call glVertex3f(10.0, 10.0, z - 10.0)

       call glTexCoord2f(1.0, 0.0)
       call glVertex3f(10.0, -10.0, z - 10.0)

       call glTexCoord2f(0.0, 0.0)
       call glVertex3f(10.0, -10.0, z)
     call glEnd
   end do
   
   call glPopMatrix 
   call glutSwapBuffers

end subroutine RenderScene
