!
! occquery.cpp
! OpenGL SuperBible, Chapter 13
! Demonstrates occlusion queries
! Original program by Benjamin Lipchak
!
module spheremod

   use OpenGL_GL

   integer(kind=GLint) :: windowWidth = 1024		   ! window size
   integer(kind=GLint) :: windowHeight = 768

   integer(kind=GLint) :: mainMenu, bboxMenu		   ! menu handles

   integer(kind=GLboolean) ::  showMenu = GL_TRUE
   integer(kind=GLboolean) ::  occlusionDetection = GL_TRUE
   integer(kind=GLboolean) ::  showBoundingVolume = GL_FALSE
   integer(kind=GLint)     ::  boundingVolume = 0
   
   integer(kind=GLuint), dimension(27) ::  queryIDs

   real(kind=GLfloat), dimension(4) :: ambientLight = (/ 0.4, 0.4, 0.4, 1.0 /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/ 0.9, 0.9, 0.9, 1.0 /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/ 100.0, 300.0, 100.0, 1.0 /)
						        
   real(kind=GLfloat), dimension(4) :: cameraPos    = (/ 200.0, 300.0, 400.0, 1.0 /)
						      
   real :: cameraZoom = 0.6

   integer :: iFrames = 0 
   real    :: TimeElapsed = 0.0

end module spheremod
program occ

   use OpenGL_GL
   use OpenGL_GLut
   use opengl_glext
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

   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
                            ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(windowWidth, windowHeight)
   iwin = glutCreateWindow('Occlusion Query Demo'//char(0))
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   ! not in original glut:
  !call glutMouseWheelFunc(MouseWheel)
  !call glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_GLUTMAINLOOP_RETURNS)

   ! Create the Menu
   bboxMenu = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('BOX (CUBE)'//char(0), 3)
   call glutAddMenuEntry('TETRAHEDRON'//char(0), 4)
   call glutAddMenuEntry('OCTAHEDRON'//char(0), 5)
   call glutAddMenuEntry('DODECAHEDRON'//char(0), 6)
   call glutAddMenuEntry('ICOSAHEDRON'//char(0), 7)

   mainMenu = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Toggle occlusion query (currently ON)'//char(0), 1)
   call glutAddMenuEntry('Show bounding volume (currently OFF)'//char(0), 2)
   call glutAddSubMenu('Choose bounding volume (currently BOX)'//char(0), bboxMenu)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call SetupRC

   call glutMainLoop()

  !if( glDeleteQueries ) call glDeleteQueries(27, queryIDs)

end program
subroutine DrawOccluder

   use opengl_gl 
   use opengl_glut

   call glColor3f(0.5, 0.25, 0.0)

   call glPushMatrix
   call glScalef(30.0, 30.0, 1.0)
   call glTranslatef(0.0, 0.0, 50.0)
   call glutSolidCube(dble(10.0))
   call glTranslatef(0.0, 0.0, -100.0)
   call glutSolidCube(dble(10.0))
   call glPopMatrix

   call glPushMatrix
   call glScalef(1.0, 30.0, 30.0)
   call glTranslatef(50.0, 0.0, 0.0)
   call glutSolidCube(dble(10.0))
   call glTranslatef(-100.0, 0.0, 0.0)
   call glutSolidCube(dble(10.0))
   call glPopMatrix

   call glPushMatrix
   call glScalef(30.0, 1.0, 30.0)
   call glTranslatef(0.0, 50.0, 0.0)
   call glutSolidCube(dble(10.0))
   call glTranslatef(0.0, -100.0, 0.0)
   call glutSolidCube(dble(10.0))
   call glPopMatrix

end subroutine DrawOccluder
subroutine DrawSphere(i)

   use opengl_gl
   use opengl_glut
   use opengl_glext
   use spheremod

   integer(kind=GLboolean) :: occluded  
   integer(kind=GLint), dimension(27) :: passingSamples

   occluded = GL_FALSE
   
   if( occlusionDetection == GL_TRUE )then
      ! Check if this sphere would be occluded
      call glGetQueryObjectiv(queryIDs(i), GL_QUERY_RESULT, & 
        		      passingSamples(i))
      if( passingSamples(i) == 0 ) occluded = GL_TRUE
   endif

  !if( occluded == GL_FALSE ) call glutSolidSphere(dble(50.0), 100, 100)
   if( occluded == GL_FALSE ) call gltDrawSphere(50.0, 100, 100)
   
end subroutine DrawSphere
subroutine DrawModels

   use opengl_gl
   use opengl_glut
   use opengl_glext

   use spheremod
   
   integer :: r, g, b

   ! Draw main occluder first
   call DrawOccluder

   if( occlusionDetection == GL_TRUE .or. &
       showBoundingVolume == GL_TRUE )then
     ! All we care about for bounding box is resulting depth values
     call glShadeModel(GL_FLAT)
     ! Texturing is already disabled
     if( showBoundingVolume == GL_TRUE )then
       call glEnable(GL_POLYGON_STIPPLE)
     else
       call glDisable(GL_LIGHTING)
       call glDisable(GL_COLOR_MATERIAL)
       call glDisable(GL_NORMALIZE)
       call glDepthMask(GL_FALSE)
       call glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE)
     endif

     ! Draw 27 spheres in a color cube
     do r=1,3
       do g=1,3
         do b=1,3	      
     	   if( showBoundingVolume == GL_TRUE ) &
             call glColor3f((r-1)*0.5,(g-1)*0.5,(b-1)*0.5)

     	   call glPushMatrix
     	   call glTranslatef(100.0 *(r-1) - 100.0, & 
     			     100.0 *(g-1) - 100.0, &
     			     100.0 *(b-1) - 100.0)
     	   call glBeginQuery(GL_SAMPLES_PASSED, &
        		     queryIDs(1+((r-1)*9)+((g-1)*3)+(b-1)))
     	   
           select case(boundingVolume)
      
     	     case(0)
     	       call glutSolidCube(dble(100.0))

     	     case(1)
     	       call glScalef(150.0, 150.0, 150.0)
     	       call glutSolidTetrahedron()

     	     case(2)
     	       call glScalef(90.0, 90.0, 90.0)
     	       call glutSolidOctahedron()

     	     case(3)
     	       call glScalef(40.0, 40.0, 40.0)
     	       call glutSolidDodecahedron()

     	     case(4)
     	       call glScalef(65.0, 65.0, 65.0)
     	       call glutSolidIcosahedron()

     	   end select	     
     	   call glEndQuery(GL_SAMPLES_PASSED)
     	   call glPopMatrix
     	 end do
       end do
     end do
     ! Restore normal drawing state
     call glDisable(GL_POLYGON_STIPPLE)
     call glShadeModel(GL_SMOOTH)
     call glEnable(GL_LIGHTING)
     call glEnable(GL_COLOR_MATERIAL)
     call glEnable(GL_NORMALIZE)
     call glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE)
     call glDepthMask(GL_TRUE)
   endif
   ! Turn on texturing just for spheres
   call glEnable(GL_TEXTURE_2D)
   call glEnable(GL_TEXTURE_GEN_S)
   call glEnable(GL_TEXTURE_GEN_T)

   ! Draw 27 spheres in a color cube
   do r=1,3
     do g=1,3
       do b=1,3 	    
   	 call glColor3f((r-1)*0.5,(g-1)*0.5,(b-1)*0.5)

   	 call glPushMatrix
   	 call glTranslatef(100.0 *(r-1) - 100.0, & 
   	  	           100.0 *(g-1) - 100.0, &
   	  	           100.0 *(b-1) - 100.0  )
   	 call DrawSphere(1+((r-1)*9)+((g-1)*3)+(b-1))
   	 call glPopMatrix
       end do
     end do
   end do 

   call glDisable(GL_TEXTURE_2D)
   call glDisable(GL_TEXTURE_GEN_S)
   call glDisable(GL_TEXTURE_GEN_T)

end subroutine DrawModels
subroutine gltPrintf(x,y,s)

  use opengl_gl
  use opengl_glut

  real :: x,y
  character :: s*(*)
  character :: c
  integer :: i,lenc
  
  call glrasterpos2f(x,y)
  lenc = len(s)
  do i=1,lenc
    c = s(i:i)
    call glutbitmapcharacter(GLUT_BITMAP_9_BY_15,ichar(c))
  end do

end subroutine gltPrintf
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine RenderScene() bind(C)

   use OpenGL_GL
   use OpenGL_GLu
   use OpenGL_GLut
   use spheremod

   character(len=64) :: string   
   
   ! Track camera angle
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity()
   if( windowWidth > windowHeight )then
     ar = windowWidth / windowHeight
     call glFrustum(dble(-ar*cameraZoom),dble(ar*cameraZoom), &
                    dble(-cameraZoom),dble(cameraZoom),dble(1.0),dble(1000.0))
   else
     ar = windowHeight / windowWidth
     call glFrustum(dble(-cameraZoom),dble(cameraZoom), &
                    dble(-ar*cameraZoom),dble(ar*cameraZoom),dble(1.0),dble(1000.0))
   endif   

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity
   call gluLookAt(dble(cameraPos(1)),dble(cameraPos(2)),dble(cameraPos(3)), &
   	          dble(0.0),dble(0.0),dble(0.0), &
		  dble(0.0),dble(1.0),dble(0.0)  )

   call glViewport(0, 0, windowWidth, windowHeight)

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Draw objects in the scene
   call DrawModels
   
   if( showMenu == GL_TRUE )then
     call glColor4f(0.0, 0.0, 1.0, 1.0)
     call glDisable(GL_LIGHTING)
     call glDisable(GL_DEPTH_TEST)
     call gltPrintf(60.,100.,'Controls:')
     call gltPrintf(60., 80.,'   Right-click for menu')
     call gltPrintf(60., 60.,'   x/X  Move +/- in x direction')
     call gltPrintf(60., 40.,'   y/Y  Move +/- in y direction')
     call gltPrintf(60., 20.,'   z/Z  Move +/- in z direction')
     call gltPrintf(60.,  0.,'    b   Switch bounding volume')
     call gltPrintf(60.,-20.,'    s   Toggle show bounding volume')
     call gltPrintf(60.,-40.,'    o   Toggle occlusion culling')
     call gltPrintf(60.,-60.,'    m   Show menu')
     call gltPrintf(60.,-80.,'    q   Exit demo')
     call glEnable(GL_LIGHTING)
     call glEnable(GL_DEPTH_TEST)
   endif
   ierr = glGetError()
   if( ierr /= GL_NO_ERROR )then
     call glColor4f(1.0, 0.0, 0.0, 1.0)
     call glDisable(GL_LIGHTING)
     call glDisable(GL_DEPTH_TEST)
     write(string,'(''GL Error! '',i8)') ierr
     call gltPrintf(0.,120.,string(1:lens(string)))
     call glEnable(GL_LIGHTING)
     call glEnable(GL_DEPTH_TEST)
   endif

   ! Flush drawing commands
   call glutSwapBuffers()

   ! Increment the frame count
   iFrames =  iFrames + 1

   ! Do periodic frame rate calculation
   if( iFrames == 101 )then

     T0 = TimeElapsed
     TimeElapsed = secnds(0.0)
     dT = TimeElapsed - T0
     
     fps = 100.0 /(dT+1.e-6)

     if( occlusionDetection == GL_TRUE )then
       write(string,'(''Draw scene with occlusion detection '',f4.1,'' fps '',i2)') fps, boundingvolume
     else
       write(string,'(''Draw scene without occlusion detection '',f4.1,'' fps'')') fps
     endif   

     call glutSetWindowTitle(string(1:lens(string))//char(0))
    !frameTimer.Reset()
     iFrames = 1
   endif    
   ! Do it again
   call glutPostRedisplay()

end subroutine RenderScene 
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLut
   use OpenGL_GLext
   
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

   type(C_PTR) :: img 
   integer(kind=GLsizei) :: nW, nH
   integer(kind=GLenum)  :: nC 
   integer(kind=GLenum)  :: eform
   integer(C_CHAR),dimension(:), allocatable :: image

   integer(kind=GLint), dimension(1) :: queryCounterBits
   integer(kind=GLubyte), dimension(1024) :: mask

   real(kind=GLfloat), dimension(4) :: p = (/ 0.02, 0.0, 0.0, -0.5 /)

   ! Make sure required functionality is available!
   !if( .not.GLEE_VERSION_1_5 .and. .not.GLEE_ARB_occlusion_query)
   !
   !   fprintf(stderr, 'Neither OpenGL 1.5 nor GL_ARB_occlusion_query'
   !		       ' extension is available!\n')
   !    Sleep(2000)
   !    exit(0)
   
   ! Make sure query counter bits is non-zero
   call glGetQueryiv(GL_SAMPLES_PASSED, GL_QUERY_COUNTER_BITS, queryCounterBits)
   if( queryCounterBits(1) == 0 )then  
     write(*,*) 'Occlusion queries not really supported!'
     write(*,*) 'Available query counter bits: 0'
     stop
   else
     write(*,*) 'Available query counter bits: ',queryCounterBits   
   endif
   
   ! Generate occlusion query names
   call glGenQueries(27, queryIDs)
   
   ! Black background
   call glClearColor(0.3, 0.3, 0.3, 1.0 )

   ! Hidden surface removal
   call glEnable(GL_DEPTH_TEST)
   call glDepthFunc(GL_LESS)

   ! Screen door transparency
   ! the stipple pattern is represented as a 32x32 array
   ! of 1-bit color indices packed in unsigned bytes
   do i=1,1024,2    
     mask( i ) = z'aa'
     mask(i+1) = z'aa'
   end do
   
   call glPolygonStipple(mask)

   write(*,*)'mask'
   call glutReportErrors
   
   ! Set up some lighting state that never changes
   call glShadeModel(GL_SMOOTH)
   call glEnable(GL_LIGHTING)
   call glEnable(GL_COLOR_MATERIAL)
   call glEnable(GL_NORMALIZE)
   call glEnable(GL_LIGHT0)
   call glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight)
   call glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight)
   call glLightfv(GL_LIGHT0, GL_POSITION, lightPos)

   ! Set up texturing for spheres
   call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR)
   call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   
   img = fgltLoadTGA('data/logo.tga', nW, nH, nC, eForm, image)
   call glTexImage2D(GL_TEXTURE_2D, 0, Nc, nW, nH, 0, eForm, GL_UNSIGNED_BYTE, img)

   if( allocated(image) )then
     deallocate(image)
     img = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glTexGenfv(GL_S, GL_OBJECT_PLANE, p)
   p(1) = 0.00
   p(2) = 0.02
   call glTexGenfv(GL_T, GL_OBJECT_PLANE, p)
   
   write(*,*)'end setup'
   call glutReportErrors
   
end subroutine SetupRC
subroutine ProcessMenu(i) bind(C)

   use OpenGL_GL
   use OpenGL_GLut
   use spheremod

   integer(glcint), intent(in), value :: i

   select case(i)
   
   case(1)
     occlusionDetection = abs( occlusionDetection - 1 )
     if( occlusionDetection == GL_TRUE )then	 
       call glutChangeToMenuEntry(1, 'Toggle occlusion culling (currently ON)'//char(0), 1)
     else
       call glutChangeToMenuEntry(1, 'Toggle occlusion culling (currently OFF)'//char(0), 1)
     endif

   case(2)
     showBoundingVolume = abs(showBoundingVolume - 1 )
     if( showBoundingVolume == GL_TRUE )then
       call glutChangeToMenuEntry(2, 'Toggle bounding volume (currently ON)'//char(0), 2)
     else
       call glutChangeToMenuEntry(2, 'Toggle bounding volume (currently OFF)'//char(0), 2)
     endif
     
   case default
     boundingVolume = i - 3
     call glutSetMenu(mainMenu)
     select case (boundingVolume)      
       case(0)
   	 call glutChangeToSubMenu(3, 'Choose bounding volume (currently BOX)'//char(0), bboxMenu)
       case(1)
   	 call glutChangeToSubMenu(3, 'Choose bounding volume (currently TETRAHEDRON)'//char(0), bboxMenu)
       case(2)
   	 call glutChangeToSubMenu(3, 'Choose bounding volume (currently OCTAHEDRON)'//char(0), bboxMenu)
       case(3)
   	 call glutChangeToSubMenu(3, 'Choose bounding volume (currently DODECAHEDRON)'//char(0), bboxMenu)
       case(4)
   	 call glutChangeToSubMenu(3, 'Choose bounding volume (currently ICOSAHEDRON)'//char(0), bboxMenu)
     end select
   end select
   ! Refresh the Window
   call glutPostRedisplay()

end subroutine ProcessMenu
subroutine lowercase(string)

  character(len=*),intent(inout) :: string
  
  ial = ichar('a')
  izl = ichar('z')
  iau = ichar('A')
  izu = ichar('Z')
  
  ioffset = ial - iau

  do i=1,len(string)
    if( ichar(string(i:i)) >= iau .and. ichar(string(i:i)) <= izu )then
      string(i:i) = char(ichar(string(i:i))+ioffset)
    endif      
  end do

end subroutine lowercase
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLut
   use spheremod

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   character(len=1) :: str

   if( key == 27 )then
     stop
   endif

   str = char(key)  
   select case(str)
   
   case('b','B')
     boundingVolume = mod(boundingVolume+1,5)
     call glutSetMenu(mainMenu)
     select case(boundingVolume)
       case(0)
         call glutChangeToSubMenu(3, 'Choose bounding volume (currently BOX)', bboxMenu)
       case(1)
         call glutChangeToSubMenu(3, 'Choose bounding volume (currently TETRAHEDRON)', bboxMenu)
       case(2)
         call glutChangeToSubMenu(3, 'Choose bounding volume (currently OCTAHEDRON)', bboxMenu)
       case(3)
         call glutChangeToSubMenu(3, 'Choose bounding volume (currently DODECAHEDRON)', bboxMenu)
       case(4)
         call glutChangeToSubMenu(3, 'Choose bounding volume (currently ICOSAHEDRON)', bboxMenu)
     end select
   case('s','S')
     showBoundingVolume = abs(showBoundingVolume-1)
     if( showBoundingVolume == GL_TRUE )then
       call glutChangeToMenuEntry(2, 'Toggle bounding volume (currently ON)', 2)
     else
       call glutChangeToMenuEntry(2, 'Toggle bounding volume (currently OFF)', 2)
     endif
   case('o','O')
     occlusionDetection = abs(occlusionDetection-1)
     if( occlusionDetection == GL_TRUE )then
       call glutChangeToMenuEntry(1, 'Toggle occlusion culling (currently ON)', 1)
     else
       call glutChangeToMenuEntry(1, 'Toggle occlusion culling (currently OFF)', 1)
     endif
   case('m','M')
     showMenu = abs(showMenu-1)
   case('x')
     cameraPos(1) = camerapos(1) + 5.0
   case('X')
     cameraPos(1) = camerapos(1) - 5.0
   case('y')
     cameraPos(2) = camerapos(2) + 5.0
   case('Y')
     cameraPos(2) = camerapos(2) - 5.0
   case('z')
     cameraPos(3) = camerapos(3) + 5.0
   case('Z')
     cameraPos(3) = camerapos(3) - 5.0
   case('q','Q')
     stop           !  ESC 
   end select 

   ! Refresh the Window
   call glutPostRedisplay()

end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   select case (key)
     case(GLUT_KEY_LEFT)
       cameraPos(1) = cameraPos(1) + 5.0

     case(GLUT_KEY_RIGHT)
       cameraPos(1) = cameraPos(1) - 5.0

     case(GLUT_KEY_UP)
       cameraPos(2) = cameraPos(2) + 5.0

     case(GLUT_KEY_DOWN)
       cameraPos(2) = cameraPos(2) - 5.0

   end select   

   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
 
   use spheremod

   integer(kind=GLcint), intent(IN), value :: win, hin

   windowWidth = win
   windowHeight = hin

end subroutine ChangeSize
subroutine MouseWheel(iwheel, idir, ix, iy ) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT 
   use spheremod

  !cameraZoom = cameraZoom * ((direction > 0) ? 1.1 : 0.9)

   if( idir > 0 )then
     cameraZoom = cameraZoom * 1.1
   else
     cameraZoom = cameraZoom * 0.9
   endif

   call glutPostRedisplay

end subroutine MouseWheel

