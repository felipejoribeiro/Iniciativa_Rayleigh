!
! shaders.cpp
! OpenGL SuperBible, Chapter 15
! Demonstrates GLSL shaders
! Original program by Benjamin Lipchak
!
module shadermod

   use OpenGL_GL

   integer(GLint) :: windowWidth = 1024		   ! window size
   integer(GLint) :: windowHeight = 768

   integer(GLboolean) :: useVertexShader = GL_TRUE
   integer(GLboolean) :: useFragmentShader = GL_TRUE
   integer(GLboolean) :: doBlink = GL_FALSE
   integer(GLboolean) :: needsValidation = GL_TRUE

   integer(GLuint)    :: vShader, fShader, progObj 
   integer(GLint)     :: flickerLocation = -1
						        
   real(GLfloat), dimension(4) :: cameraPos    = (/ 100.0, 150.0, 200.0, 1.0 /)		      
   real(GLdouble) :: cameraZoom = 0.6

   integer, parameter :: MAX_INFO_LOG_SIZE = 2048

end module shadermod
program shadrs

   use OpenGL_GL
   use OpenGL_GLut
   use Opengl_GLext

   use shadermod
   
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

   call glutInit()
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
                            ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(windowWidth, windowHeight)
   iwin = glutCreateWindow('GLSL Shaders Demo'//char(0))
   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)

   ! not in original glut:
   call glutMouseWheelFunc(MouseWheel)
   call glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, &
                      GLUT_ACTION_GLUTMAINLOOP_RETURNS)

   ! Create the Menu
   menu = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Toggle vertex shader (currently ON)'//char(0), 1)
   call glutAddMenuEntry('Toggle fragment shader (currently ON)'//char(0), 2)
   call glutAddMenuEntry('Toggle flicker (currently OFF)'//char(0), 3)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call SetupRC()

   call glutMainLoop()

  !if( glDeleteQueries ) call glDeleteQueries(27, queryIDs)

end program
subroutine Link(firstTime)

   use OpenGL_GL
   use OpenGL_GLUT

   use shadermod

   integer(GLboolean) :: firstTime
   integer(GLint) :: success

   character, dimension(MAX_INFO_LOG_SIZE) :: infoLog

   call glLinkProgram(progObj)
   call glGetProgramiv(progObj, GL_LINK_STATUS, success)
    
   if( success /= 1 )then
     call glGetProgramInfoLog(progObj, MAX_INFO_LOG_SIZE, NULL, infoLog)
     call glColor4f(1.0, 0.0, 0.0, 1.0)
     
     !gltPrintf(GLUT_BITMAP_9_BY_15, 0, 0, "Error in program linkage!  Info log:");
     !gltPrintf(GLUT_BITMAP_HELVETICA_10, 2, 0, "%s", infoLog);
     
     call glutSwapBuffers()
     call sleep(5)
      
     stop '1'
     
   endif

   if( firstTime == GL_TRUE) call glUseProgram(progObj)

   ! Find out where the flicker constant lives
   flickerLocation = glGetUniformLocation(progObj,'flickerFactor'//char(0))

   ! Initially set the blink parameter to 1 (no flicker)
   if( flickerLocation /= -1) call glUniform1f(flickerLocation, 1.0)

   ! Program object has changed, so we should revalidate
   needsValidation = GL_TRUE

end subroutine Link
subroutine ReLink()

   use OpenGL_GL

   call Link(GL_FALSE)
   
end subroutine ReLink()
subroutine DrawModels()

   use OpenGL_GL
   use OpenGL_GLUT

   ! Draw plane that the objects rest on
   call glColor3f(0.0, 0.0, 0.90) ! Blue
   call glNormal3f(0.0, 1.0, 0.0)
   call glBegin(GL_QUADS)
     call glVertex3f(-100.0, -25.0, -100.0)
     call glVertex3f(-100.0, -25.0,  100.0)		
     call glVertex3f( 100.0, -25.0,  100.0)
     call glVertex3f( 100.0, -25.0, -100.0)
   call glEnd()

   ! Draw red cube
   call glColor3f(1.0, 0.0, 0.0)
   call glutSolidCube(48.0)

   ! Draw green sphere
   call glColor3f(0.0, 1.0, 0.0)
   call glPushMatrix()
   call glTranslatef(-60.0, 0.0, 0.0)
   call glutSolidSphere(25.0, 50, 50)
   call glPopMatrix()

   ! Draw yellow cone
   call glColor3f(1.0, 1.0, 0.0)
   call glPushMatrix()
   call glRotatef(-90.0, 1.0, 0.0, 0.0)
   call glTranslatef(60.0, 0.0, -24.0)
   call glutSolidCone(25.0, 50.0, 50, 50)
   call glPopMatrix()

   ! Draw magenta torus
   call glColor3f(1.0, 0.0, 1.0)
   call glPushMatrix()
   call glTranslatef(0.0, 0.0, 60.0)
   call glutSolidTorus(8.0, 16.0, 50, 50)
   call glPopMatrix()

   ! Draw cyan octahedron
   call glColor3f(0.0, 1.0, 1.0)
   call glPushMatrix()
   call glTranslatef(0.0, 0.0, -60.0)
   call glScalef(25.0, 25.0, 25.0)
   call glutSolidOctahedron()
   call glPopMatrix()

end subroutine DrawModels()
subroutine RenderScene() bind(C)

   use OpenGL_GL
   use OpenGL_GLU
   use OpenGL_GLUT

   use shadermod

   integer(GLint) :: success

   real(GLfloat), save :: flickerFactor = 1.0
   real(GLdouble) :: ar 

   ! Track camera angle
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity()
   
   if( windowWidth > windowHeight )then
     ar = dble(float(windowWidth)/float(windowHeight))
     call glFrustum(-ar*cameraZoom, ar*cameraZoom, &
                       -cameraZoom,    cameraZoom, 1.0, 1000.0)
   else
     ar = dble(float(windowHeight)/float(windowWidth))
     call glFrustum(   -cameraZoom,    cameraZoom, &
                    -ar*cameraZoom, ar*cameraZoom, 1.0, 1000.0)
   endif
   
   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity()
   call gluLookAt(cameraPos(1), cameraPos(2), cameraPos(3), & 
                  0.0, 0.0, 0.0, 0.0, 1.0, 0.0)

   call glViewport(0, 0, windowWidth, windowHeight)
    
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   if( doBlink == GL_TRUE .and. flickerLocation /= -1 )then
     ! Pick a random flicker factor
     call random_number(r)
     flickerFactor = flickerFactor + (r - 0.5)*0.1
     if( flickerFactor > 1.0 ) flickerFactor = 1.0
     if( flickerFactor < 0.0 ) flickerFactor = 0.0
     
     call glUniform1f(flickerLocation, flickerFactor)
   endif

   ! Validate our shader before first use
   if( needsValidation == GL_TRUE )then
   
     call glValidateProgram(progObj)
     call glGetProgramiv(progObj, GL_VALIDATE_STATUS, success)
     
     if( success == 0 )then
       
       call glGetProgramInfoLog(progObj, MAX_INFO_LOG_SIZE, NULL, infoLog)
       call glColor4f(1.0f, 0.0f, 0.0f, 1.0f)
       !gltPrintf(GLUT_BITMAP_9_BY_15, 0, 0, "Error in program validation!  Info log:")
       !gltPrintf(GLUT_BITMAP_HELVETICA_10, 2, 0, "%s", infoLog)
       
       call glutSwapBuffers()
       call sleep(5)
       
       stop '2'
       
     endif

     needsValidation = GL_FALSE

   endif
    
   ! Draw objects in the scene
   call DrawModels()
    
   call glColor4f(1.0f, 1.0f, 1.0f, 1.0f)
   call glDisable(GL_DEPTH_TEST)
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 0, 3, "Controls:")
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 2, 3, "    Right-click for menu")
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 4, 3, "    x/X      Move +/- in x direction")
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 5, 3, "    y/Y      Move +/- in y direction")
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 6, 3, "    z/Z      Move +/- in z direction")
   ! gltPrintf(GLUT_BITMAP_9_BY_15, 8, 3, "    q        Exit demo")
   call glEnable(GL_DEPTH_TEST)

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

   if( doBlink == GL_TRUE .and. flickerLocation /= -1 ) &
     call glutPostRedisplay()

end subroutine RenderScene
subroutine SetupRC()

    integer(GLint) :: success
    
    call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
    call glColor4f(1.0, 0.0, 0.0, 1.0)

    ! Black background
    call glClearColor(0.0, 0.0, 0.0, 1.0 )
    call glSecondaryColor3f(1.0, 1.0, 1.0)

    ! Hidden surface removal
    call glEnable(GL_DEPTH_TEST)
    call glDepthFunc(GL_LEQUAL)

    call glShadeModel(GL_SMOOTH)

    ! Create shader objects and specify shader text
    !vShader = glCreateShader(GL_VERTEX_SHADER)
    !fShader = glCreateShader(GL_FRAGMENT_SHADER)
    !vsStringPtr[0] = vsString;
    !glShaderSource(vShader, 1, vsStringPtr, NULL);
    !fsStringPtr[0] = fsString;
    !glShaderSource(fShader, 1, fsStringPtr, NULL);

    ! Compile shaders and check for any errors
    call glCompileShader(vShader)
    call glGetShaderiv(vShader, GL_COMPILE_STATUS, success)
    
    if( success /= 0 )then
        GLchar infoLog[MAX_INFO_LOG_SIZE];
        glGetShaderInfoLog(vShader, MAX_INFO_LOG_SIZE, NULL, infoLog);
        //gltPrintf(GLUT_BITMAP_9_BY_15, 0, 0, "Error in vertex shader compilation!  Info log:");
        //gltPrintf(GLUT_BITMAP_HELVETICA_10, 2, 0, "%s", infoLog);
        glutSwapBuffers();
        Sleep(5);
        exit(0);
    }
    glCompileShader(fShader);
    glGetShaderiv(fShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        GLchar infoLog[MAX_INFO_LOG_SIZE];
        glGetShaderInfoLog(fShader, MAX_INFO_LOG_SIZE, NULL, infoLog);
        //gltPrintf(GLUT_BITMAP_9_BY_15, 0, 0, "Error in fragment shader compilation!  Info log:");
        //gltPrintf(GLUT_BITMAP_HELVETICA_10, 2, 0, "%s", infoLog);
        glutSwapBuffers();
        Sleep(5000);
        exit(0);
    }

    // Create program object, attach shaders, then link
    progObj = glCreateProgram();
    if (useVertexShader)
        glAttachShader(progObj, vShader);
    if (useFragmentShader)
        glAttachShader(progObj, fShader);

    Link(GL_TRUE);
}




