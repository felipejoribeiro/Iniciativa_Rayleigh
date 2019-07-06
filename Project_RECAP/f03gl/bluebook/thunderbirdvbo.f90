!
! ThunderBird.cpp
! OpenGL SuperBible
! Demonstrates rendering a sample model with indexed vertex arrays
! Original Program by Richard S. Wright Jr.
!
!module opengl_glee
!
!   use opengl_gl
!
!   integer(glenum), parameter :: GL_GENERATE_MIPMAP             = z'8191' ! 0x8191
!  
!   integer(glenum), parameter :: GL_REFLECTION_MAP              = z'8512' ! 0x8512
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP            = z'8513' ! 0x8513
!   integer(glenum), parameter :: GL_TEXTURE_BINDING_CUBE_MAP    = z'8514' ! 0x8514
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_X = z'8515' ! 0x8515
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_X = z'8516' ! 0x8516
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_Y = z'8517' ! 0x8517
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = z'8518' ! 0x8518
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_Z = z'8519' ! 0x8519
!   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = z'851A' ! 0x851A
!
!end module opengl_glee
module spheremod

   use GLframes
   use VBOMeshes
   
   use Opengl_gl
   use opengl_glee

   ! Light values and coordinates
   real(kind=GLfloat), dimension(4) :: ambientLight = (/   .1,   .1,  .1, 0. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/  1.0,  1.0, 1.0, 0. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/  0.5,  0.5, 0.5, 0. /)

   real(kind=GLfloat), dimension(4) :: lightPos     = (/-100.,100.,100.,  1. /)
   real(kind=GLfloat), dimension(4) :: nolight      = (/   0.,  0.,  0.,  0. /)
   real(kind=GLfloat), dimension(4) :: lowlight     = (/ 0.25,0.25,0.25,  1. /)
   real(kind=GLfloat), dimension(4) :: brightlight  = (/   1.,  1.,  1.,  1. /)

   real :: yRot  = 0.0
   real :: Scale = 0.01

   type(GLframe), save  :: frameCamera

   integer, parameter   :: CUBE_MAP       = 1
   integer, parameter   :: BODY_TEXTURE   = 2
   integer, parameter   :: GLASS_TEXTURE  = 3

   integer, parameter   :: NUM_TEXTURES   = 3
   integer(kind=GLuint) :: textureObjects(NUM_TEXTURES)

   integer, dimension(3704,9) :: face_indicies
   real, dimension(1898,3) :: vertices  
   real, dimension(2716,3) :: normals 
   real, dimension(2925,2) :: textures

   ! Glass cock-pit
   integer, dimension(352,9) :: face_indiciesGlass
   real, dimension(197,3) :: verticesGlass
   real, dimension(227,3) :: normalsGlass 
   real, dimension(227,2) :: texturesGlass

   type(VBOMesh), save :: thunderBirdBody
   type(VBOMesh), save :: thunderBirdGlass

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
   call glutInitDisplayMode(ior(GLUT_DOUBLE, &
        ior(GLUT_RGB,ior(GLUT_DEPTH,GLUT_MULTISAMPLE))))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('OpenGL Sweet Sixteen w/VBO''s'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutSpecialFunc(KeySpecialFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1)

   call setuprc

   call glutMainLoop

end program
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
 
   type(C_PTR), dimension(NUM_TEXTURES) :: img 
   type(C_PTR) :: imgc 
   
   integer(kind=GLsizei) :: nW, nH
   integer(kind=GLenum)  :: nC 
   integer(kind=GLenum)  :: eform
   integer(C_CHAR),dimension(:), allocatable :: image

   real, dimension(3,3) :: v, n, t
   
   !
   ! from glext.h:
   !
   integer(kind=GLenum), parameter :: GL_TEXTURE_MAX_ANISOTROPY_EXT      = z'84fe'
   integer(kind=GLenum), parameter :: GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT  = z'84ff' 

   real, dimension(4) :: flargest

   call glCullFace(GL_BACK)
   call glFrontFace(GL_CCW)
   call glEnable(GL_CULL_FACE)
   call glEnable(GL_DEPTH_TEST)
        
   call glGenTextures(3, textureObjects)
   !
   ! Set up texture maps   
   !
   ! Cube Map
   call glBindTexture(GL_TEXTURE_CUBE_MAP, textureObjects(CUBE_MAP))
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)      
  
   ! Load Cube Map images
   do i=1,6
     ! Load this texture map
     call glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_GENERATE_MIPMAP, int(GL_TRUE))

     imgc = fgltLoadTGA(TexFiles(i),nW, nH, nC, eform, image)
     call glTexImage2D(cube(i), 0, nC, nW, nH, 0, eForm, GL_UNSIGNED_BYTE, imgc )

     if( allocated(image) )then
       deallocate(image)
       imgc = C_NULL_PTR
       write(*,*)'image deallocated'
     endif
   end do     

   !
   ! Load the body texture
   !
   call glBindTexture(GL_TEXTURE_2D, textureObjects(BODY_TEXTURE))
   call glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, int(GL_TRUE))
   
   img(1) = fgltLoadTGA('data/body.tga', nW, nH, nC, eForm, image)
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm,GL_UNSIGNED_BYTE,img(1))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE)
   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE)

   call glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, fLargest)
   call glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAX_ANISOTROPY_EXT, fLargest(1))
   
   ! 
   ! glass
   !
   call glBindTexture(GL_TEXTURE_2D, textureObjects(GLASS_TEXTURE))
   call glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, int(GL_TRUE))

   img(2) = fgltLoadTGA('data/glass.tga', nW, nH, nC, eForm, image)
   call glTexImage2D(GL_TEXTURE_2D, 0, nC, nW, nH, 0, eForm,GL_UNSIGNED_BYTE,img(2))

   if( allocated(image) )then
     deallocate(image)
     img(i) = C_NULL_PTR
     write(*,*)'image deallocated'
   endif

   call glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
   call gltexparameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)
   call gltexparameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE)
   call gltexparameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE)

   !////////////////////////////////////////////////////////////////////
   ! 
   ! Set up the texture units
   !
   ! First texture unit contains the color map
   !
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   call glEnable(GL_TEXTURE_2D)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE) ! Decal tarnish
    
   ! Second texture unit contains the cube map
   call glActiveTextureARB(GL_TEXTURE1_ARB)
   call glBindTexture(GL_TEXTURE_CUBE_MAP, textureObjects(CUBE_MAP))
   call glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   call glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   call glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP)
   call glEnable(GL_TEXTURE_CUBE_MAP)
    
   ! Multiply this texture by the one underneath
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    
   write(*,*)'textures done'
   !
   ! Load Thunderbird body and canopy
   !
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

   ! Start assembling the body mesh, set maximum size
   call VBO_BeginMesh(thunderbirdbody,3704*3)

   v =  0.0
   n =  0.0
   t = -1.0
   do i=1,3704 ! Loop through all the faces
     do j=1,3  ! Assemble the triangle
       i1 = face_indicies(i,(j-1)+1)
       i2 = face_indicies(i,(j-1)+4)
       i3 = face_indicies(i,(j-1)+7)
       v(j,1:3) = vertices(i1,:)
       n(j,1:3) = normals(i2,:)
       t(j,1:2) = textures(i3,:)
     end do
     call VBO_AddTriangle(thunderbirdbody,v,n,t)
   end do
   nv = thunderbirdbody%nNumVerts*3
   thunderbirdbody%Verts(1:nv) = Scale * thunderbirdbody%Verts(1:nv)
       
   call VBO_EndMesh(thunderbirdbody)
  !call VBO_ScaleMesh(thunderbirdbody,Scale)
   
   ! Now do the same for the canopy
   write(*,*)'glass'
   call VBO_BeginMesh(thunderbirdglass,352*3)

   v =  0.0
   n =  0.0
   t = -1.0
   do i=1,352
     do j=1,3
       i1 = face_indiciesglass(i,(j-1)+1)
       i2 = face_indiciesglass(i,(j-1)+4)
       i3 = face_indiciesglass(i,(j-1)+7)
       v(j,1:3) = verticesglass(i1,:)
       n(j,1:3) = normalsglass(i2,:)
       t(j,1:2) = texturesglass(i3,:)
     end do
     call VBO_AddTriangle(thunderbirdglass,v,n,t)
   end do
   nv = thunderbirdglass%nNumVerts*3
   thunderbirdglass%Verts(1:nv) = Scale * thunderbirdglass%Verts(1:nv)

   call VBO_EndMesh(thunderbirdglass)
  !call VBO_ScaleMesh(thunderbirdglass,Scale)
   
   write(*,*)'mesh assembled'
   !
   ! Set up lighting
   !
   call glEnable(GL_LIGHTING)
   call glEnable(GL_LIGHT0)
   call glEnable(GL_COLOR_MATERIAL)
   call glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE)
   call glMaterialfv(GL_FRONT,GL_SPECULAR, DiffuseLight)
   call glMateriali(GL_FRONT,GL_SHININESS, 128)

   call glLightfv(GL_LIGHT0,GL_AMBIENT, AmbientLight)
   call glLightfv(GL_LIGHT0,GL_DIFFUSE, DiffuseLight)
   call glLightfv(GL_LIGHT0,GL_SPECULAR, specular)
   call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, AmbientLight)
   call glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL,GL_SEPARATE_SPECULAR_COLOR)

    ! Light never changes, put it here
   call glLightfv(GL_LIGHT0,GL_POSITION, lightPos)

  !call glEnable(GL_RESCALE_NORMAL)

   call MoveUp(frameCamera, 20.0)
   
end subroutine setuprc
subroutine RenderScene() bind(C)

   use OpenGL_GL
   use OpenGL_GLu
   use OpenGL_GLut

   use spheremod

   real, dimension(16) :: m, invert, tst 

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
   call glPushMatrix
       
   call ApplyCameraTransform(frameCamera) ! Move the camera about
   
   ! Sky Box is manually textured
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   call glDisable(GL_TEXTURE_2D)
   call glActiveTextureARB(GL_TEXTURE1_ARB)

   call glEnable(GL_TEXTURE_CUBE_MAP)
   call glDisable(GL_TEXTURE_GEN_S)
   call glDisable(GL_TEXTURE_GEN_T)
   call glDisable(GL_TEXTURE_GEN_R)     
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)
   
   call DrawSkyBox
 
   ! Use texgen to apply cube map
   call glEnable(GL_TEXTURE_GEN_S)
   call glEnable(GL_TEXTURE_GEN_T)
   call glEnable(GL_TEXTURE_GEN_R)
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
   call glDisable(GL_TEXTURE_CUBE_MAP)
   
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   call glEnable(GL_TEXTURE_2D)

 
   call glActiveTextureARB(GL_TEXTURE1_ARB)
   call glMatrixMode(GL_TEXTURE)
   call glPushMatrix()
       
   ! Invert camera matrix (rotation only) and apply to 
   ! texture coordinates
   call GetCameraOrientation(frameCamera,m)
   call m3dInvertMatrix44(invert, m)
      
   call glMultMatrixf(invert)
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   call glMatrixMode(GL_MODELVIEW)
       
   call rotatingbird
  !call formation

   call glMatrixMode(GL_TEXTURE)
   call glActiveTextureARB(GL_TEXTURE1_ARB)
   call glPopMatrix
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   call glMatrixMode(GL_MODELVIEW)

   call glPopMatrix
   
   ! Do the buffer Swap
   call glutSwapBuffers 
   
end subroutine RenderScene
subroutine rotatingbird
!
! This displays a rotating ThunderBird Model
!
   use OpenGL_gl
   use OpenGL_glut
   use spheremod

   yRot = yRot + 0.1
   call glTranslatef(0.0, 19.6, -3.0)
   call glRotatef(yRot, 0.0, 1.0, 0.0)
   call DrawThunderBird

end subroutine rotatingbird
subroutine formation

   use OpenGL_gl
   use spheremod

  !yRot = yRot - 0.1
   call glRotatef(yRot, 0.0, 1.0, 0.0)
   call glPushMatrix
     call glTranslatef(0.0, 21.5, -7.0)
     call glRotatef(90.0, 0.0, 1.0, 0.0)
     call glRotatef(45.0, 0.0, 0.0, 1.0)
     call glRotatef(-10.0, 1.0, 0.0, 0.0)
     call DrawThunderBird
   call glPopMatrix

   call glPushMatrix
     call glTranslatef(0.85,20.75, -6.5)
     call glRotatef(90.0, 0.0, 1.0, 0.0)
     call glRotatef(45.0, 0.0, 0.0, 1.0)
     call glRotatef(-10.0, 1.0, 0.0, 0.0)
     call DrawThunderBird
   call glPopMatrix

   call glPushMatrix
     call glTranslatef(-1.0, 19.75, -7.0)
     call glRotatef(90.0, 0.0, 1.0, 0.0)
     call glRotatef(45.0, 0.0, 0.0, 1.0)
     call glRotatef(-10.0, 1.0, 0.0, 0.0)
     call DrawThunderBird
   call glPopMatrix

   call glPushMatrix
     call glTranslatef(-.15, 19., -6.5)
     call glRotatef(90.0, 0.0, 1.0, 0.0)
     call glRotatef(45.0, 0.0, 0.0, 1.0)
     call glRotatef(-10.0, 1.0, 0.0, 0.0)
     call DrawThunderBird
   call glPopMatrix

end subroutine formation
subroutine DrawSkyBox

   use OpenGL_gl
   
   fExtent = 50.0
   
   call glBegin(GL_QUADS)
     !/////////////////////////////////////////////
     ! Negative X
     ! Note, we must now use the multi-texture version of glTexCoord
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)

     !/////////////////////////////////////////////
     !  Postive X
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
 
     !//////////////////////////////////////////////
     ! Negative Z 
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)

     !///////////////////////////////////////////////
     ! Positive Z 
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)

     !////////////////////////////////////////////////
     ! Positive Y
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, 1.0)
     call glVertex3f(-fExtent, fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, 1.0, -1.0)
     call glVertex3f(-fExtent, fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, -1.0)
     call glVertex3f(fExtent, fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, 1.0, 1.0)
     call glVertex3f(fExtent, fExtent, fExtent)
    
     !/////////////////////////////////////////////////
     ! Negative Y
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, -1.0)
     call glVertex3f(-fExtent, -fExtent, -fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, -1.0, -1.0, 1.0)
     call glVertex3f(-fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, 1.0)
     call glVertex3f(fExtent, -fExtent, fExtent)
     
     call glMultiTexCoord3fARB(GL_TEXTURE1_ARB, 1.0, -1.0, -1.0)
     call glVertex3f(fExtent, -fExtent, -fExtent)
   call glEnd
   
end subroutine DrawSkyBox
subroutine DrawThunderBird
   
   use OpenGL_GL
   use Spheremod
   
   call glEnableClientState(GL_VERTEX_ARRAY)
   call glEnableClientState(GL_NORMAL_ARRAY)
   call glEnableClientState(GL_TEXTURE_COORD_ARRAY)

   call glActiveTextureARB(GL_TEXTURE1_ARB)
   call glDisable(GL_TEXTURE_CUBE_MAP)
   call glActiveTextureARB(GL_TEXTURE0_ARB)
   
   call glPushMatrix
     call glRotatef(-90.0, 1.0, 0.0, 0.0)
     call glTexEnvi(GL_TEXTURE_2D, GL_TEXTURE_ENV_MODE, GL_MODULATE)
     call glBindTexture(GL_TEXTURE_2D, textureObjects(BODY_TEXTURE))
     
     call VBO_Draw(thunderBirdBody)
          
   call glPopMatrix
   
   call glActiveTextureARB(GL_TEXTURE1_ARB)
   call glEnable(GL_TEXTURE_CUBE_MAP)
   call glActiveTextureARB(GL_TEXTURE0_ARB)

   call glEnable(GL_BLEND)
   call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
   call glColor4f(1.0, 1.0, 1.0, 0.25)
   call glBindTexture(GL_TEXTURE_2D, textureObjects(GLASS_TEXTURE))
   
   call glTranslatef(0.0, 0.132, 0.555)
   
   call glFrontFace(GL_CW)
   
   call VBO_Draw(thunderBirdGlass)
   
   call glFrontFace(GL_CCW)
   
   call VBO_Draw(thunderBirdGlass)
   
   call glDisable(GL_BLEND)

   call glDisableClientState(GL_VERTEX_ARRAY)
   call glDisableClientState(GL_NORMAL_ARRAY)
   call glDisableClientState(GL_TEXTURE_COORD_ARRAY)

end subroutine DrawThunderBird
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
     call glDeleteTextures(NUM_TEXTURES, textureObjects)
     stop
   endif
   
end subroutine KeyPressFunc
subroutine KeySpecialFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   use spheremod

   integer(kind=GLint), intent(in), value  :: key, x, y

   if( key == GLUT_KEY_UP)        call MoveForward (frameCamera, 0.1)
   if( key == GLUT_KEY_DOWN)      call MoveForward (frameCamera,-0.1)
   if( key == GLUT_KEY_LEFT)      call RotateLocalY(frameCamera, 0.1)
   if( key == GLUT_KEY_RIGHT)     call RotateLocalY(frameCamera,-0.1)
   if( key == GLUT_KEY_PAGE_UP)   call RotateLocalX(frameCamera, 0.1)
   if( key == GLUT_KEY_PAGE_DOWN) call RotateLocalX(frameCamera,-0.1)
   
   ! Refresh the Window
   call glutPostRedisplay

end subroutine KeySpecialFunc
   

        

