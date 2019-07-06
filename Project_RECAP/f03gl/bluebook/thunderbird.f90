!
! ThunderBird.cpp
! OpenGL SuperBible
! Demonstrates rendering a sample model with indexed vertex arrays
! Original Program by Richard S. Wright Jr.
!
module spheremod

   use Opengl_gl

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .1,   .1,  .1, 0. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/  1.0,  1.0, 1.0, 0. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  0.5,  0.5, 0.5, 0. /)

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-100.,100.,100.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real :: yRot = 0.0

   integer, parameter :: BODY_TEXTURE = 1
   integer, parameter :: GLASS_TEXTURE  = 2

   integer, parameter :: NUM_TEXTURES   = 2
   integer(kind=GLuint) :: textureObjects(NUM_TEXTURES)

   ! Display list identifiers
   integer(kind=GLint) :: bodyList, glassList 

   integer, dimension(3704,9) :: face_indicies
   real, dimension(1898,3) :: vertices  
   real, dimension(2716,3) :: normals 
   real, dimension(2925,2) :: textures

   ! Glass cock-pit
   integer, dimension(352,9) :: face_indiciesGlass
   real, dimension(197,3) :: verticesGlass
   real, dimension(227,3) :: normalsGlass 
   real, dimension(227,2) :: texturesGlass

end module spheremod
program thunder

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
     
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('F-16 Thunderbird'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call glutTimerFunc(5, TimerFunction, 1)

   call setuprc

   call glutMainLoop

end program
subroutine DrawBody

   use spheremod

   write(*,*)'drawbody'
   call glBegin(GL_TRIANGLES)
   
   do i=1,3704 ! Each new triangle starts here
     do j=1,3  ! Each vertex specified here
       i1 = face_indicies(i,(j-1)+1)
       i2 = face_indicies(i,(j-1)+4)
       i3 = face_indicies(i,(j-1)+7)
       ! Lookup the texture value
       call glTexCoord2fv(textures(i3,:))
       ! Lookup the normal value
       call glNormal3fv(normals(i2,:))
       ! Lookup the vertex value
       call glVertex3fv(vertices(i1,:))
     end do
   end do
   call glEnd

end subroutine DrawBody
subroutine DrawGlass
   
   use spheremod

   write(*,*)'drawglass'
   call glBegin(GL_TRIANGLES)

   do i=1,352    ! Each new triangle starts here
     do j=1,3    ! Each vertex specified here
       i1 = face_indiciesGlass(i,(j-1)+1)
       i2 = face_indiciesGlass(i,(j-1)+4)
       i3 = face_indiciesGlass(i,(j-1)+7)
       ! Lookup the texture value
       call glTexCoord2fv(texturesGlass(i3,1:2)) 
       ! Lookup the normal value
       call glNormal3fv(normalsGlass(i2,1:3))
       ! Lookup the vertex value
       call glVertex3fv(verticesGlass(i1,1:3)) 
     end do
   end do
   call glEnd
   
end subroutine DrawGlass
subroutine setuprc

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

   type(C_PTR), dimension(NUM_TEXTURES) :: img 
   integer(kind=GLsizei) :: nW, nH
   integer(kind=GLenum)  :: nC 
   integer(kind=GLenum)  :: eform
   integer(C_CHAR),dimension(:), allocatable :: image

   !
   ! from glext.h:
   !
   integer(kind=GLenum), parameter :: GL_TEXTURE_MAX_ANISOTROPY_EXT      = z'84fe'
   integer(kind=GLenum), parameter :: GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT  = z'84ff' 

   real, dimension(4) :: flargest

   open(11,file='data/body.dat')
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(face_indicies(i,j),j=1,9)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(vertices(i,j),j=1,3)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(normals(i,j),j=1,3)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(textures(i,j),j=1,2)
   end do
   close(11)

   open(11,file='data/glass.dat')
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(face_indiciesGlass(i,j),j=1,9)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(verticesGlass(i,j),j=1,3)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(normalsGlass(i,j),j=1,3)
   end do
   read(11,*) N1
   do ii=1,N1
     read(11,*) i,(texturesGlass(i,j),j=1,2)
   end do
   close(11)
   
   write(*,*)'data files read'
   ! Bluish background
   call glClearColor(0.0, 0.0, 0.5, 1.0 )
   call glEnable(GL_DEPTH_TEST)
   call glEnable(GL_CULL_FACE)

   ! Lit texture environment
   call glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)

   call glGenTextures(2, textureObjects)

   ! Load the body texture
   call glBindTexture(GL_TEXTURE_2D, textureObjects(BODY_TEXTURE))
   
   img(1) = fgltLoadTGA('data/body.tga', nW, nH, nC, eForm, image)
  !img(1) = gltLoadTGA('data/body.tga'//char(0), nW, nH, nC, eform)

   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm,GL_UNSIGNED_BYTE,img(1))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, fLargest)
   call glTexParameterf(GL_TEXTURE_2D,gl_TEXTURE_MAX_ANISOTROPY_EXT, fLargest(1))
   
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP)

   call glBindTexture(GL_TEXTURE_2D, textureObjects(GLASS_TEXTURE))

   img(2) = fgltLoadTGA('data/glass.tga', nW, nH, nC, eForm, image)
  !img(2) = gltLoadTGA('data/glass.tga'//char(0), nW, nH, nC, eform)

   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm,GL_UNSIGNED_BYTE,img(2))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,float(GL_LINEAR))
   CALL GLTEXPARAMETERF(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,float(GL_LINEAR))
   CALL GLTEXPARAMETERF(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,float(GL_CLAMP))
   CALL GLTEXPARAMETERF(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,float(GL_CLAMP))

   call glEnable(GL_TEXTURE_2D)

   write(*,*)'textures done'
   
   ! Set up lighting
   call glEnable(GL_LIGHTING)
   call glEnable(GL_LIGHT0)
   call glEnable(GL_COLOR_MATERIAL)
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)
   call glMaterialfv(GL_FRONT,GL_SPECULAR, DiffuseLight)
   call glMateriali(GL_FRONT,GL_SHININESS, 128)

   call glLightfv(GL_LIGHT0,GL_AMBIENT, AmbientLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE, DiffuseLight)
   call glLightfv(GL_LIGHT0,GL_SPECULAR, specular)
   call glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL,GL_SEPARATE_SPECULAR_COLOR)

    ! Light never changes, put it here
   call glLightfv(GL_LIGHT0,GL_POSITION, lightPos)

   call glEnable(GL_RESCALE_NORMAL)

   bodyList = glGenLists(2)
   glassList = bodyList + 1

   call glNewList(bodyList,GL_COMPILE)
   call DrawBody
   call glEndList

   call glNewList(glassList,GL_COMPILE)
   call DrawGlass
   call glEndList
 
end subroutine setuprc
subroutine RenderScene() bind(C)

   use OpenGL_GL
   use OpenGL_GLu
   use OpenGL_GLut

   use spheremod

   yRot = yrot + 0.5
       
   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
   call glPushMatrix
       
   call glTranslatef(0.0, 0.0, -4.0)
   call glRotatef(10.0, 1.0, 0.0, 0.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)

   call glPushMatrix
     call glRotatef(-90.0, 1.0, 0.0, 0.0);
     call glBindTexture(GL_TEXTURE_2D, textureObjects(BODY_TEXTURE))
     call glScalef(.01, .01, .01)
     call glCallList(bodyList)   ! DrawBody
   call glPopMatrix
   
   call glEnable(GL_BLEND)
   call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
   call glTranslatef(0.0, 0.132, 0.555)
   call glBindTexture(GL_TEXTURE_2D, textureObjects(GLASS_TEXTURE))
   
   call glScalef(0.01, 0.01, 0.01)
   call glFrontFace(GL_CW)
   call glCallList(glassList)    ! DrawGlass
   call glFrontFace(GL_CCW)
   call glCallList(glassList)    ! DrawGlass
   call glDisable(GL_BLEND)
      
   call glPopMatrix
   
   ! Do the buffer Swap
   call glutSwapBuffers 
   
end subroutine RenderScene
recursive subroutine TimerFunction( ivalue ) bind(C)
!
! Called by GLUT library when idle (window not being
! resized or moved)
!
   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(5,TimerFunction, 1)

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
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(1000.0))

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
     call glDeleteLists(bodyList, 2)
     call glDeleteTextures(NUM_TEXTURES, textureObjects)
     stop
   endif
   
end subroutine KeyPressFunc
   

        

