!
! shadowmap.cpp
! OpenGL SuperBible, Chapter 14
! Demonstrates shadow mapping
! Original Program by Benjamin Lipchak
!
module spheremod

   use OpenGL_GL

   logical :: ambientShadowAvailable = .FALSE.
   logical :: npotTexturesAvailable = .FALSE.
   logical :: controlCamera = .TRUE.            ! xyz keys will control lightpos
   logical :: noShadows = .FALSE.               ! normal lighting
   logical :: showShadowMap = .FALSE.           ! show the shadowmap texture
   logical :: showMenu = .TRUE.                 ! show menu

   real :: factor = 4.0                         ! for polygon offset

   integer(kind=GLint)  :: windowWidth = 1024   ! window size
   integer(kind=GLint)  :: windowHeight = 512

   integer(kind=GLint)  :: shadowWidth = 1024   ! set based on window size
   integer(kind=GLint)  :: shadowHeight = 512

   integer(kind=GLuint), dimension(1) :: shadowTextureID
   integer(kind=GLint), dimension(1)  :: maxTexSize  ! maximum allowed size for 1D/2D texture

   real, dimension(4)   :: ambientLight = (/   0.2 ,   0.2 ,   0.2 , 1.0 /)
   real, dimension(4)   :: diffuseLight = (/   0.7 ,   0.7 ,   0.7 , 1.0 /) 
   real, dimension(4)   :: noLight      = (/   0.0 ,   0.0 ,   0.0 , 1.0 /)
   real, dimension(4)   :: lightPos     = (/ 100.0 , 300.0 , 100.0 , 1.0 /)
   real, dimension(4)   :: lowAmbient   = (/   0.1 ,   0.1 ,   0.1 , 1.0 /)
   real, dimension(4)   :: lowDiffuse   = (/   0.35,   0.35,   0.35, 1.0 /)

   real, dimension(4)   :: cameraPos    = (/ 100.0 , 150.0 , 200.0 , 1.0 /)

   double precision     :: cameraZoom = 0.3

   real, dimension(4,4) :: textureMatrix

end module spheremod
program shadowmap

   use OpenGL_GL
   use OpenGL_GLUT
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

     subroutine TimerFunction(i) bind(C)
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
   call glutInitWindowSize(windowWidth, windowHeight)
   iwin = glutCreateWindow('Shadow Mapping Demo'//char(0))
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Toggle shadows (currently ON)'//char(0), 1)
   call glutAddMenuEntry('Toggle show shadowmap (currently OFF)'//char(0), 2)
   call glutAddMenuEntry('Switch to LIGHT control (currently CAMERA)'//char(0), 3)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call SetupRC

   call glutMainLoop
   
end program
subroutine DrawModels(drawBasePlane)

   use OpenGL_GL
   use OpenGL_GLUT
   use spheremod

   logical :: drawBasePlane
   
   if( drawBasePlane )then
     ! Draw plane that the objects rest on
     call glColor3f(0.0, 0.0, 0.9)                  ! Blue
     call glNormal3f(0.0, 1.0, 0.0)
     call glBegin(GL_QUADS)
       call glVertex3f(-100.0, -25.0, -100.0)
       call glVertex3f(-100.0, -25.0,  100.0)
       call glVertex3f( 100.0, -25.0,  100.0)
       call glVertex3f( 100.0, -25.0, -100.0)
     call glEnd
   endif

   ! Draw red cube
   call glColor3f(1.0 , 0.0 , 0.0 )
   call glutSolidCube(dble(48.0))

   ! Draw green sphere
   call glColor3f(0.0 , 1.0 , 0.0 )
   call glPushMatrix
   call glTranslatef(-60.0 , 0.0 , 0.0 )
   call glutSolidSphere(dble(25.0), 50, 50)
   call glPopMatrix

   ! Draw yellow cone
   call glColor3f(1.0 , 1.0 , 0.0 )
   call glPushMatrix
   call glRotatef(-90.0 , 1.0 , 0.0 , 0.0 )
   call glTranslatef(60.0 , 0.0 , -24.0 )
   call glutSolidCone(dble(25.0),dble(50.0), 50, 50)
   call glPopMatrix

   ! Draw magenta torus
   call glColor3f(1.0 , 0.0 , 1.0 )
   call glPushMatrix
   call glTranslatef(0.0 , 0.0 , 60.0 )
   call glutSolidTorus(dble(8.0),dble(16.0), 50, 50)
   call glPopMatrix

   ! Draw cyan octahedron
   call glColor3f(0.0 , 1.0 , 1.0 )
   call glPushMatrix
   call glTranslatef(0.0 , 0.0 , -60.0 )
   call glScalef(25.0 , 25.0 , 25.0 )
   call glutSolidOctahedron()
   call glPopMatrix

end subroutine DrawModels
subroutine RegenerateShadowMap
! Called to regenerate the shadow map

   use OpenGL_GL
   use OpenGL_GLU
   use OpenGL_GLUT
   use spheremod

   real :: lightToSceneDistance, nearPlane, fieldOfView
   real, dimension(16)  :: lightModelview, lightProjection
   real, dimension(4,4) :: tempMatrix 
   
   real :: sceneBoundingRadius = 95.0   ! based on objects in scene

   double precision :: Zero = 0.0, One = 1.0
   real :: m3dRadToDeg

   ! Save the depth precision for where it's useful
   lightToSceneDistance = sqrt( lightPos(1) * lightPos(1) + &
   			        lightPos(2) * lightPos(2) + &
   			        lightPos(3) * lightPos(3) )
   nearPlane = lightToSceneDistance - sceneBoundingRadius
   ! Keep the scene filling the depth texture
   fieldOfView = m3dRadToDeg(2.0  * atan(sceneBoundingRadius / lightToSceneDistance))

   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity
   call gluPerspective(dble(fieldOfView), dble(1.0), &
                       dble(nearPlane), dble(nearPlane + 2.0*sceneBoundingRadius))
   call glGetFloatv(GL_PROJECTION_MATRIX, lightProjection)
   ! Switch to light's point of view
   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity
   call gluLookAt(dble(lightPos(1)), dble(lightPos(2)), dble(lightPos(3)), & 
   	          Zero, Zero, Zero, Zero, One, Zero )
   call glGetFloatv(GL_MODELVIEW_MATRIX, lightModelview)
   call glViewport(0, 0, shadowWidth, shadowHeight)

   ! Clear the depth buffer only
   call glClear(GL_DEPTH_BUFFER_BIT)

   ! All we care about here is resulting depth values
   call glShadeModel(GL_FLAT)
   call glDisable(GL_LIGHTING)
   call glDisable(GL_COLOR_MATERIAL)
   call glDisable(GL_NORMALIZE)
   call glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE)

   ! Overcome imprecision
   call glEnable(GL_POLYGON_OFFSET_FILL)

   ! Draw objects in the scene except base plane
   ! which never shadows anything
   call DrawModels(.FALSE.)

   ! Copy depth values into depth texture
   call glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, &
   		         0, 0, shadowWidth, shadowHeight, 0)

   ! Restore normal drawing state
   call glShadeModel(GL_SMOOTH)
   call glEnable(GL_LIGHTING)
   call glEnable(GL_COLOR_MATERIAL)
   call glEnable(GL_NORMALIZE)
   call glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE)
   call glDisable(GL_POLYGON_OFFSET_FILL)

   ! Set up texture matrix for shadow map projection,
   ! which will be rolled into the eye linear
   ! texture coordinate generation plane equations

   call m3dLoadIdentity44(tempMatrix)
   call m3dTranslateMatrix44(tempMatrix, 0.5, 0.5, 0.5)
   call m3dScaleMatrix44(tempMatrix, 0.5, 0.5, 0.5)
   call m3dMatrixMultiply44(textureMatrix, tempMatrix, lightProjection)
   call m3dMatrixMultiply44(tempMatrix, textureMatrix, lightModelview)
   ! transpose to get the s, t, r, and q rows for plane equations
   call m3dTransposeMatrix44(textureMatrix, tempMatrix)

end subroutine RegenerateShadowMap
subroutine RenderScene() bind(C)

   use OpenGL_GL
   use OpenGL_GLU
   use OpenGL_GLUT
   use OpenGL_GLext
   
   use spheremod

   double precision :: Zero = 0.0, One = 1.0

   ! Track camera angle
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity
   if( bwindowWidth > windowHeightb )then
     ar = dble( float(windowWidth) / float(windowHeight) )
     call glFrustum(-ar * cameraZoom, ar * cameraZoom, &
                    -cameraZoom, cameraZoom, One, dble(1000.0))
   else
     ar = dble( float(windowHeight) / float(windowWidth) )
     call glFrustum(-cameraZoom, cameraZoom, -ar * cameraZoom, &
                    ar * cameraZoom, One, dble(1000.0))
   endif 

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity
   call gluLookAt( dble(cameraPos(1)), dble(cameraPos(2)), dble(cameraPos(3)), &
   	           Zero, Zero, Zero,  Zero, One, Zero)

   call glViewport(0, 0, windowWidth, windowHeight)
   
   ! Track light position
   call glLightfv(GL_LIGHT0, GL_POSITION, lightPos)

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   if( showShadowMap )then
   
     ! Display shadow map for educational purposes
     call glMatrixMode(GL_PROJECTION)
     call glLoadIdentity
     call glMatrixMode(GL_MODELVIEW)
     call glLoadIdentity
     call glMatrixMode(GL_TEXTURE)
     call glPushMatrix
     call glLoadIdentity
     call glEnable(GL_TEXTURE_2D)
     call glDisable(GL_LIGHTING)
     call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)
     call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_NONE)
     ! Show the shadowMap at its actual size relative to window
     call glBegin(GL_QUADS)
       call glTexCoord2f(0.0 , 0.0 )
       call glVertex2f(-1.0 , -1.0 )
       call glTexCoord2f(1.0 , 0.0 )
       call glVertex2f(float(shadowWidth)/float(windowWidth)*2.0 - 1.0 , -1.0 )
       call glTexCoord2f(1.0 , 1.0 )
       call glVertex2f(float(shadowWidth)/float(windowWidth)*2.0 - 1.0 , & 
                       float(shadowHeight)/float(windowHeight)*2.0 - 1.0 )
       call glTexCoord2f(0.0 , 1.0 )
       call glVertex2f(-1.0 , float(shadowHeight)/float(windowHeight)*2.0 - 1.0 )
     call glEnd
     call glDisable(GL_TEXTURE_2D)
     call glEnable(GL_LIGHTING)
     call glPopMatrix
     call glMatrixMode(GL_PROJECTION)
     call gluPerspective(dble(45.0),dble(1.0),dble(1.0),dble(1000.0))
     call glMatrixMode(GL_MODELVIEW)
   
   else if( noShadows )then
   
     ! Set up some simple lighting
     call glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight)
     call glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight)
     ! Draw objects in the scene including base plane
     call DrawModels(.TRUE.)
   
   else
     if( .not. ambientShadowAvailable )then
       
       ! Because there is no support for an 'ambient'
       ! shadow compare fail value, we'll have to
       ! draw an ambient pass first...
       call glLightfv(GL_LIGHT0, GL_AMBIENT, lowAmbient)
       call glLightfv(GL_LIGHT0, GL_DIFFUSE, lowDiffuse)

       ! Draw objects in the scene, including base plane
       call DrawModels(.TRUE.)

       ! Enable alpha test so that shadowed fragments are discarded
       call glAlphaFunc(GL_GREATER, 0.9)
       call glEnable(GL_ALPHA_TEST)
     
     endif

     call glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight)
     call glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight)

     ! Set up shadow comparison
     call glEnable(GL_TEXTURE_2D)
     call glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)
     call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_COMPARE_MODE, &
        	          GL_COMPARE_R_TO_TEXTURE)
    
     ! Set up the eye plane for projecting the shadow map on the scene
     call glEnable(GL_TEXTURE_GEN_S)
     call glEnable(GL_TEXTURE_GEN_T)
     call glEnable(GL_TEXTURE_GEN_R)
     call glEnable(GL_TEXTURE_GEN_Q)
     call glTexGenfv(GL_S,GL_EYE_PLANE, textureMatrix(1:4,1))
     call glTexGenfv(GL_T,GL_EYE_PLANE, textureMatrix(1:4,2))
     call glTexGenfv(GL_R,GL_EYE_PLANE, textureMatrix(1:4,3))
     call glTexGenfv(GL_Q,GL_EYE_PLANE, textureMatrix(1:4,4))

     ! Draw objects in the scene, including base plane
     call DrawModels(.TRUE.)

     call glDisable(GL_ALPHA_TEST)
     call glDisable(GL_TEXTURE_2D)
     call glDisable(GL_TEXTURE_GEN_S)
     call glDisable(GL_TEXTURE_GEN_T)
     call glDisable(GL_TEXTURE_GEN_R)
     call glDisable(GL_TEXTURE_GEN_Q)
   endif
   
   ierr = glGetError()
   if( ierr /= GL_NO_ERROR ) write(*,*)'GL Error!',ierr

   if( showMenu )then
     call glColor4f(1.0, 1.0, 1.0, 1.0)
     call glDisable(GL_LIGHTING)
     call glDisable(GL_DEPTH_TEST)

     call gltPrintf( 0., 40., 'Controls:')
     call gltPrintf( 0., 30., 'Right-click for menu')
     call gltPrintf( 0., 20., 'x/X  Move +/- in x direction')
     call gltPrintf( 0., 10., 'y/Y  Move +/- in y direction')
     call gltPrintf( 0.,  0., 'z/Z  Move +/- in z direction')
     call gltPrintf( 0.,-10., 'f/F  Change polygon offset factor +/-')
     call gltPrintf( 0.,-20., 'm    Toggle menu')
     call gltPrintf( 0.,-30., 'q    Exit demo')

     call glEnable(GL_LIGHTING)
     call glEnable(GL_DEPTH_TEST)
   endif

   ! Flush drawing commands
   call glutSwapBuffers

end subroutine RenderScene
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLU
   use OpenGL_GLUT
   use OpenGL_GLext

   use spheremod

   write(*,*)'Shadow Mapping Demo'

   ! Make sure required functionality is available!
   !if (!GLEE_VERSION_1_4 && !GLEE_ARB_shadow)
   !  fprintf(stderr, 'Neither OpenGL 1.4 nor call gl_ARB_shadow'
   !     	     ' extension is available!\n')
   !  Sleep(2000)
   !  exit(0)
   
   ! Check for optional extensions
   ! in: /usr/include/GL/glext.h set to 1
  !if( GLEE_ARB_shadow_ambient )then   
     ambientShadowAvailable = .TRUE.  
  !else   
  !  write(*,*)'GL_ARB_shadow_ambient extension not available!'
  !  write(*,*)'Extra ambient rendering pass will be required.'
  !  ! sleep(2000)
  !endif

  ! in: /usr/include/GL/glext.h set to 1
  !if( GLEE_VERSION_2_0 || glEE_ARB_texture_non_power_of_two)
     npotTexturesAvailable = .TRUE.
  !else
  !  write(*,*)'Neither OpenGL 2.0 nor call gl_ARB_texture_non_power_of_two extension'
  !  write(*,*)'is available!  Shadow map will be lower resolution (lower quality).'
  !  ! sleep(2000)
  !endif

   call glGetIntegerv(GL_MAX_TEXTURE_SIZE, maxTexSize)
   
   ! Black background
   call glClearColor(0.0 , 0.0 , 0.0 , 1.0  )

   ! Hidden surface removal
   call glEnable(GL_DEPTH_TEST)
   call glDepthFunc(GL_LEQUAL)
   call glPolygonOffset(factor, 0.0 )

   ! Set up some lighting state that never changes
   call glShadeModel(GL_SMOOTH)
   call glEnable(GL_LIGHTING)
   call glEnable(GL_COLOR_MATERIAL)
   call glEnable(GL_NORMALIZE)
   call glEnable(GL_LIGHT0)

   ! Set up some texture state that never changes
   call glGenTextures(1, shadowTextureID)
   call glBindTexture(GL_TEXTURE_2D, shadowTextureID(1))
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_DEPTH_TEXTURE_MODE,GL_INTENSITY)
   if( ambientShadowAvailable ) &
       call glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_COMPARE_FAIL_VALUE_ARB,0.5)
   call glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR)
   call glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR)
   call glTexGeni(GL_R,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR)
   call glTexGeni(GL_Q,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR)

   call RegenerateShadowMap

end subroutine SetupRC
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
subroutine ProcessMenu(value) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT
   use spheremod

   integer(glcint), intent(in), value :: value

   select case( value ) 
     case(1)
       noShadows = .not. noShadows
       showShadowMap = .FALSE.
       if( noShadows )then
         call glutChangeToMenuEntry(1, 'Toggle shadows (currently OFF)'//char(0), 1)
       else
         call glutChangeToMenuEntry(1, 'Toggle shadows (currently ON)'//char(0), 1)
       endif
       call glutChangeToMenuEntry(2, 'Toggle show shadowmap (currently OFF)'//char(0), 2)

     case(2)
       showShadowMap = .not. showShadowMap
       noShadows = .FALSE.
       if (showShadowMap)then
   	 call glutChangeToMenuEntry(2, 'Toggle show shadowmap (currently ON)'//char(0), 2)
       else
         call glutChangeToMenuEntry(2, 'Toggle show shadowmap (currently OFF)'//char(0), 2)
       endif
       call glutChangeToMenuEntry(1, 'Toggle shadows (currently ON)'//char(0), 1)

     case(3)
       controlCamera = .not. controlCamera
       if( controlCamera )then
   	 call glutChangeToMenuEntry(3, 'Switch to LIGHT control (currently CAMERA)'//char(0), 3)
       else
         call glutChangeToMenuEntry(3, 'Switch to CAMERA control (currently LIGHT)'//char(0), 3)
       endif
       
   end select
   ! Refresh the Window
   call glutPostRedisplay

end subroutine ProcessMenu
subroutine KeyPressFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLU
   use OpenGL_GLUT
   use spheremod

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   character(len=1) :: str

   if( key == 27 )then
     stop
   endif

   str = char(key)  

   select case(str)   
     case('m','M')
       showmenu = .not. showmenu

     case('f')
       factor = factor + 1
       call glPolygonOffset(factor, 0.0 )
       call RegenerateShadowMap

     case('F')
       factor = factor - 1
       call glPolygonOffset(factor, 0.0 )
       call RegenerateShadowMap

     case('x')
       if( controlCamera )then
	 cameraPos(1) = cameraPos(1) +  5.0 
       else
	 lightPos(1) = lightPos(1) +  5.0 
       endif

     case('X')
       if( controlCamera )then
	 cameraPos(1) = cameraPos(1) -  5.0 
       else
	 lightPos(1) = lightPos(1) -  5.0 
       endif

     case('y')
       if( controlCamera )then
	 cameraPos(2) = cameraPos(2) +  5.0 
       else
	 lightPos(2) = lightPos(2) +  5.0 
       endif

     case('Y')
       if( controlCamera )then
	 cameraPos(2) = cameraPos(2) -  5.0 
       else
	 lightPos(2) = lightPos(2) -  5.0 
       endif

     case('z')
       if( controlCamera )then
	 cameraPos(3) = cameraPos(3) +  5.0 
       else
	 lightPos(3) = lightPos(3) +  5.0 
       endif

     case('Z')
       if( controlCamera )then
	 cameraPos(3) = cameraPos(3) -  5.0 
       else
	 lightPos(3) = lightPos(3) -  5.0 
       endif

     case('q','Q')
       stop

   end select
   
   ! We don't need to regenerate the shadow map
   ! if only the camera angle changes
   if( .not. controlCamera ) call RegenerateShadowMap

   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)

   use OpenGL_GL
   use OpenGL_GLUT
   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   select case(key)
     case (glUT_KEY_LEFT)
       if(controlCamera )then
	 cameraPos(1) = cameraPos(1) - 5.0 
       else
	 lightPos(1) =  lightPos(1) - 5.0 
       endif
     case (glUT_KEY_RIGHT)
       if( controlCamera )then
	 cameraPos(1) = cameraPos(1) + 5.0 
       else
	 lightPos(1) = lightPos(1) + 5.0 
       endif
     case (glUT_KEY_UP)
       if( controlCamera )then
	 cameraPos(3) = cameraPos(3) - 5.0 
       else
	 lightPos(3) = lightPos(3) - 5.0 
       endif
     case (glUT_KEY_DOWN)
       if( controlCamera )then
	 cameraPos(3) = cameraPos(3) + 5.0 
       else
	 lightPos(3) = lightPos(3) + 5.0 
       endif
   end select

   ! We don't need to regenerate the shadow map
   ! if only the camera angle changes
   if( .not. controlCamera ) call RegenerateShadowMap  

   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
subroutine ChangeSize(w,h) bind(C)

   use OpenGL_GL
   use spheremod

   integer(kind=GLcint), intent(IN), value :: w, h
   
   windowWidth  = w
   shadowWidth  = w
   
   windowHeight = h 
   shadowHeight = h
   
   if( .not. npotTexturesAvailable )then
       ! Find the largest power of two that will fit in window.
       ! Try each width until we get one that's too big
       i = 1
       do while( i <= shadowWidth)
   	 i = i*2
       end do
       shadowWidth = i / 2

       ! Now for height
       i = 1
       do while(i <= shadowHeight)
   	 i = i*2
       end do
       shadowHeight = i / 2
       
       write(*,*)'w,h:',shadowWidth,shadowHeight
   endif

   if( shadowWidth > maxTexSize(1) ) shadowWidth = maxTexSize(1)
   
   if(shadowHeight > maxTexSize(1) ) shadowHeight = maxTexSize(1)
   
   call RegenerateShadowMap

end subroutine ChangeSize
