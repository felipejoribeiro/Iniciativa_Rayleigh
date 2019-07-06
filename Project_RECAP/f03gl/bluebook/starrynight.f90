!
! Smoother.cpp
! OpenGL SuperBible
! Demonstrates vertex arrays (with point sprites!)
! Original program by Richard S. Wright Jr.
!
module spheremod

   use opengl_gl

   integer, parameter :: SMALL_STARS  =  40
   integer, parameter :: MEDIUM_STARS =  20
   integer, parameter :: LARGE_STARS  =  10

   integer, parameter :: SCREEN_X =  800
   integer, parameter :: SCREEN_Y =  600

   real, dimension(SMALL_STARS,2), target  :: vSmallStars
   real, dimension(MEDIUM_STARS,2), target :: vMediumStars
   real, dimension(LARGE_STARS,2), target  :: vLargeStars

   integer :: drawMode = 1

   integer, parameter :: NUM_TEXTURES   = 2
   integer(kind=GLuint) :: textureObjects(NUM_TEXTURES)

   !
   ! from: /usr/include/GL/glext.h
   !
   integer(kind=GLenum) :: GL_POINT_SPRITE    = z'8861'
   integer(kind=GLenum) :: GL_COORD_REPLACE   = z'8862'
   integer(kind=GLenum) :: GL_GENERATE_MIPMAP = z'8191'

end module spheremod
program starrynight

   use opengl_gl
   use opengl_glut

   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ProcessMenu_(i) bind(C)
       integer, intent(in), value :: i
     end subroutine ProcessMenu_

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
   iwin = glutCreateWindow('Starry Starry Night'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu_)
   call glutAddMenuEntry('Normal Points'//char(0),1)
   call glutAddMenuEntry('Antialiased Points'//char(0),2)
   call glutAddMenuEntry('Point Sprites'//char(0),3)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)
   
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call SetupRC

   call glutMainLoop

end program
subroutine ProcessMenu_(i) bind(C)

   use opengl_gl 
   use opengl_glu
   use opengl_glut

   use spheremod

   integer, intent(in), value :: i

   drawMode = i

   select case(i)
     case(1)
       ! Turn off blending and all smoothing
       call glDisable(GL_BLEND)
       call glDisable(GL_LINE_SMOOTH)
       call glDisable(GL_POINT_SMOOTH)
       call glDisable(GL_TEXTURE_2D)
       call glDisable(GL_POINT_SPRITE)

     case(2)
       ! Turn on antialiasing, and give hint to do the best
       ! job possible.
       call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
       call glEnable(GL_BLEND)
       call glEnable(GL_POINT_SMOOTH)
       call glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
       call glEnable(GL_LINE_SMOOTH)
       call glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
       call glDisable(GL_TEXTURE_2D)
       call glDisable(GL_POINT_SPRITE)

     case(3)
       ! Point Sprites
       call glEnable(GL_BLEND)
       call glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR)
       call glDisable(GL_LINE_SMOOTH)
       call glDisable(GL_POINT_SMOOTH)
       call glDisable(GL_POLYGON_SMOOTH)

   end select

   ! Trigger a redraw
   call glutPostRedisplay
    
end subroutine ProcessMenu_
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   use spheremod
   
   real(kind=GLfloat) :: x =   700.0   ! Location and radius of moon
   real(kind=GLfloat) :: y =   500.0
   real(kind=GLfloat) :: r =    50.0
   real(kind=GLfloat) :: angle = 0.0   ! Another looping variable
                       
   ! Clear the window
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
        
   ! Everything is white
   call glColor3f(1.0, 1.0, 1.0)
   
   if(drawMode == 3 )then
     call glEnable(GL_POINT_SPRITE)
     call glEnable(GL_TEXTURE_2D)
     call glBindTexture(GL_TEXTURE_2D, textureObjects(1))
     call glEnable(GL_BLEND)
   endif

   call glEnableClientState(GL_VERTEX_ARRAY)

   ! Draw small stars
   call glPointSize(7.0)
  !call glBegin(GL_POINTS)
  !do i=1,SMALL_STARS
  !  call glVertex2fv(vSmallStars(i,1:2))
  !end do
  !call glEnd
   call glVertexPointer(2, GL_FLOAT, 0, c_loc(vSmallStars))
   call glDrawArrays(GL_POINTS, 0, SMALL_STARS)
           
   ! Draw medium sized stars
   call glPointSize(12.0)
  !call glBegin(GL_POINTS)
  !do i=1,MEDIUM_STARS 
  !  call glVertex2fv(vMediumStars(i,1:2))
  !end do
  !call glEnd
   call glVertexPointer(2, GL_FLOAT, 0, c_loc(vMediumStars))
   call glDrawArrays(GL_POINTS, 0, MEDIUM_STARS)
        
   ! Draw largest stars
   call glPointSize(20.0)
  !call glBegin(GL_POINTS)
  !do i=1,LARGE_STARS
  !  call glVertex2fv(vLargeStars(i,1:2))
  !end do
  !call glEnd
        
   call glVertexPointer(2, GL_FLOAT, 0, c_loc(vLargeStars))
   call glDrawArrays(GL_POINTS, 0, LARGE_STARS)	   
   	       
   call glDisableClientState(GL_VERTEX_ARRAY)
       
   call glPointSize(120.0)
   if( drawMode == 3 )then
     call glDisable(GL_BLEND)
     call glBindTexture(GL_TEXTURE_2D, textureObjects(2))
   endif

   call glBegin(GL_POINTS)
     call glVertex2f(x, y)
   call glEnd

   call glDisable(GL_TEXTURE_2D)
   call glDisable(GL_POINT_SPRITE)

   ! Draw distant horizon
   call glLineWidth(3.5)
   call glBegin(GL_LINE_STRIP)
     call glVertex2f(  0.0,  25.0)
     call glVertex2f( 50.0, 100.0)
     call glVertex2f(100.0,  25.0)
     call glVertex2f(225.0, 115.0)
     call glVertex2f(300.0,  50.0)
     call glVertex2f(375.0, 100.0)
     call glVertex2f(460.0,  25.0)
     call glVertex2f(525.0, 100.0)
     call glVertex2f(600.0,  20.0)
     call glVertex2f(675.0,  70.0)
     call glVertex2f(750.0,  25.0)
     call glVertex2f(800.0,  90.0)
   call glEnd
   
   ! Swap buffers
   call glutSwapBuffers
   
end subroutine RenderScene
subroutine SetupRC

   use OpenGL_GL
   use opengl_glut

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

   ! Populate star list
   do i=1,SMALL_STARS 
     call random_number(r)
     vSmallStars(i,1) = r * float(SCREEN_X)
     call random_number(r)
     vSmallStars(i,2) = r * float((SCREEN_Y - 125)) + 125.0 
   end do
           
   ! Populate star list
   do i=1,MEDIUM_STARS 
     call random_number(r)
     vMediumStars(i,1) = r * float(SCREEN_X)
     call random_number(r)
     vMediumStars(i,2) = r * float((SCREEN_Y - 125)) + 125.0 
   end do

   ! Populate star list
   do i=1,LARGE_STARS 
     call random_number(r)
     vLargeStars(i,1)  = r * float(SCREEN_X)
     call random_number(r)
     vLargeStars(i,2)  = r * float((SCREEN_Y - 125)) + 125.0 
   end do
                      
   ! Black background
   call glClearColor(0.0, 0.0, 0.0, 1.0)

   ! Set drawing color to white
   call glColor3f(1.0, 1.0, 1.0)

   ! Load our textures
   call glGenTextures(2, textureObjects)
    
   call glBindTexture(GL_TEXTURE_2D, textureObjects(1)) 
    
   ! Load this texture map
   call glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, int(GL_TRUE))

   img(1) = fgltLoadTGA('data/star.tga', nW, nH, nC, eForm, image)
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm, &
                     GL_UNSIGNED_BYTE, img(1))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif
   
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

   call glBindTexture(GL_TEXTURE_2D, textureObjects(2))
   call glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, int(GL_TRUE))

   img(2) = fgltLoadTGA('data/moon.tga', nW, nH, nC, eForm, image)
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm, &
                     GL_UNSIGNED_BYTE, img(2))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

   call glTexEnvi(GL_POINT_SPRITE, GL_COORD_REPLACE, int(GL_TRUE))
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)

   ! Point Sprites enabled by default
   call ProcessMenu(3)

end subroutine SetupRC
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu

   use spheremod
    
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
   call gluOrtho2D(dble(0.0), dble(SCREEN_X), dble(0.0), dble(SCREEN_Y))

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

 
