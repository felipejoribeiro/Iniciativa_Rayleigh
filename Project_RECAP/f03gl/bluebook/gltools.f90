! 
! glxinfo:
!
! server glx vendor string: NVIDIA Corporation
! server glx version string: 1.4
! server glx extensions: ...
! .
! .
! OpenGL vendor string: NVIDIA Corporation
! OpenGL renderer string: GeForce GT 230/PCI/SSE2
! OpenGL version string: 3.2.0 NVIDIA 195.36.24  ===> OpenGL 3.2
! OpenGL shading language version string: 1.50 NVIDIA via Cg compiler
! OpenGL extensions: ...
! .
! GLU version is 1.3
!
! GL_GLEXT_VERSION 56
! GLee = GL Easy Extension library = 
!        provides support for OpenGL up to version 3.0 (dus < 3.2)
!
module opengl_glee

   use opengl_gl

   public 
   
   integer(glenum), parameter :: GL_GENERATE_MIPMAP             = z'8191' ! 0x8191
  
   integer(glenum), parameter :: GL_REFLECTION_MAP              = z'8512' ! 0x8512
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP            = z'8513' ! 0x8513
   integer(glenum), parameter :: GL_TEXTURE_BINDING_CUBE_MAP    = z'8514' ! 0x8514
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_X = z'8515' ! 0x8515
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_X = z'8516' ! 0x8516
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_Y = z'8517' ! 0x8517
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = z'8518' ! 0x8518
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_POSITIVE_Z = z'8519' ! 0x8519
   integer(glenum), parameter :: GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = z'851A' ! 0x851A

end module opengl_glee
module opengl_glext

   use opengl_gl
   use, intrinsic :: iso_c_binding
   
   integer(glenum), parameter :: GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = z'80bf' 

   integer(glenum), parameter :: GL_DEPTH_TEXTURE_MODE       = z'884b' 
   integer(glenum), parameter :: GL_TEXTURE_COMPARE_MODE     = z'884c' 
   integer(glenum), parameter :: GL_COMPARE_R_TO_TEXTURE     = z'884e' 

   integer(glenum), parameter :: GL_QUERY_COUNTER_BITS       = z'8864' 
   integer(glenum), parameter :: GL_CURRENT_QUERY            = z'8865' 
   integer(glenum), parameter :: GL_QUERY_RESULT             = z'8866' 
   integer(glenum), parameter :: GL_QUERY_RESULT_AVAILABLE   = z'8867' 

   integer(glenum), parameter :: GL_ARRAY_BUFFER             = z'8892' 
   integer(glenum), parameter :: GL_ELEMENT_ARRAY_BUFFER     = z'8893' 
   
   integer(glenum), parameter :: GL_READ_WRITE               = z'88ba' 
   
   integer(glenum), parameter :: GL_PIXEL_PACK_BUFFER        = z'88eb' 
   integer(glenum), parameter :: GL_PIXEL_UNPACK_BUFFER      = z'88ec' 

   integer(glenum), parameter :: GL_STREAM_DRAW              = z'88e0' 
   integer(glenum), parameter :: GL_STREAM_READ              = z'88e1' 
   integer(glenum), parameter :: GL_STREAM_COPY              = z'88e2'
    
   integer(glenum), parameter :: GL_STATIC_DRAW              = z'88e4' 
   integer(glenum), parameter :: GL_STATIC_READ              = z'88e5' 
   integer(glenum), parameter :: GL_STATIC_COPY              = z'88e6'
    
   integer(glenum), parameter :: GL_DYNAMIC_DRAW             = z'88e8' 
   integer(glenum), parameter :: GL_DYNAMIC_READ             = z'88e9' 
   integer(glenum), parameter :: GL_DYNAMIC_COPY             = z'88ea' 

   integer(glenum), parameter :: GL_SAMPLES_PASSED           = z'8914' 

      !
      ! glGenBuffers (GLsizei, GLuint *)
      ! glBindBuffer (GLenum, GLuint)
      ! glBufferData (GLenum, GLsizeiptr, const GLvoid *, GLenum)
      !


   interface  
     ! glGenBuffers etc is available only if the GL version is 1.5 or greater.
     ! glGenBuffers - generate buffer object names
     ! void glGenBuffers( GLsizei n, GLuint* buffers)
     subroutine glGenBuffers(n, buffers) bind(C,name="glGenBuffers")
       use opengl_gl
       integer(GLsizei), value :: n
      !integer(GLuint), dimension(n), intent(out) :: buffers       
       type(C_PTR), dimension(n) :: buffers       
     end subroutine glGenBuffers
     
     ! glBindBuffer - bind a named buffer object
     ! void glBindBuffer( GLenum target, GLuint buffer)
     subroutine glBindBuffer(target, buffer) bind(C,name="glBindBuffer")
       use opengl_gl
       integer(GLenum), value :: target
       integer(GLuint), value :: buffer       
     end subroutine glBindBuffer

     ! glBufferData - creates and initializes a buffer object's data store
     ! void glBufferData( GLenum target, GLsizeiptr size, const GLvoid* data, GLenum usage)
     subroutine glBufferData(target, size, data, usage) bind(C,name="glBufferData")
       use opengl_gl
       integer(GLenum), value :: target
       integer(GLsizei), value :: size    ! size in bytes
       type(C_PTR) :: data                ! pointer to data that will be copied into the data store
       integer(GLenum), value :: usage    ! the expected usage pattern of the data store
     end subroutine glBufferData

     ! glMapBuffer - map a buffer object's data store
     ! void * glMapBuffer( GLenum target, GLenum access)
     function glMapBuffer(target, access) bind(C,name="glMapBuffer")
       use opengl_gl
       type(C_PTR) :: glMapBuffer       
       integer(kind=GLenum), value :: target
       integer(kind=GLenum), value :: access
     end function glMapBuffer
     
     !  
     ! GLboolean glUnmapBuffer(GLenum target)
     subroutine glUnmapBuffer(target) bind(C,name="glUnmapBuffer")
       use opengl_gl
       integer(kind=GLenum), value :: target
     end subroutine glUnmapBuffer
     
     !
     ! === chapter 13
     !
     
     ! glBeginQuery - delimit the boundaries of a query object
     ! void glBeginQuery(GLenum target, GLuint id)
     subroutine glBeginQuery(target, id) bind(C,name="glBeginQuery")
       use opengl_gl
       integer(kind=GLenum), value :: target
       integer(kind=GLuint), value :: id
     end subroutine glBeginQuery
     
     !
     ! void glEndQuery(GLenum target)
     subroutine glEndQuery(target) bind(C,name="glEndQuery")
       use opengl_gl
       integer(kind=GLenum), value :: target
     end subroutine glEndQuery
     
     ! glGenQueries - generate query object names
     ! void glGenQueries(GLsizei n, GLuint * ids)
     subroutine glGenQueries(n, ids) bind(C,name="glGenQueries")
       use opengl_gl
       integer(kind=GLsizei), value :: n
       integer(kind=GLuint), dimension(n) :: ids       
     end subroutine glGenQueries
     
     ! glGetQueryiv - return parameters of a query object target
     ! void glGetQueryiv(GLenum target,GLenum pname,GLint * params)
     subroutine glGetQueryiv(target, pname, params) bind(C,name="glGetQueryiv")
       use opengl_gl
       integer(kind=GLenum), value :: target
       integer(kind=GLenum), value :: pname
       integer(kind=GLint), dimension(*) :: params
     end subroutine glGetQueryiv
     
     ! glGetQueryObject - return parameters of a query object
     ! void glGetQueryObjectiv(GLuint id, GLenum pname, GLint * params)
     subroutine glGetQueryObjectiv(target, pname, params) bind(C,name="glGetQueryObjectiv")
       use opengl_gl
       integer(kind=GLenum), value :: target
       integer(kind=GLenum), value :: pname
       integer(kind=GLint), dimension(*) :: params
     end subroutine glGetQueryObjectiv
    
     
     
     !
     ! 
   end interface 

end module opengl_glext
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
module VBOmeshes

   use OpenGL_gl
   use Opengl_glext

   integer(kind=GLint), parameter :: NbufferObjects = 4
   type :: VBOMesh
     integer(kind=GLuint), dimension(:),  allocatable :: Indexes
     real(kind=GLfloat), dimension(:), allocatable :: Verts
     real(kind=GLfloat), dimension(:), allocatable :: Norms
     real(kind=GLfloat), dimension(:), allocatable :: TexCoords

     integer(kind=GLint) :: nMaxIndexes = 0 ! maximum workspace 
     integer(kind=GLint) :: nNumIndexes = 0 ! Number of indexes currently used
     integer(kind=GLint) :: nNumVerts   = 0 ! Number of vertices currently used

     integer(kind=GLuint), dimension(NbufferObjects) :: bufferObjects
   end type VBOMesh

   public
   
   integer(kind=GLint), parameter, private :: VERTEX_DATA  = 1
   integer(kind=GLint), parameter, private :: NORMAL_DATA  = 2
   integer(kind=GLint), parameter, private :: TEXTURE_DATA = 3
   integer(kind=GLint), parameter, private :: INDEX_DATA   = 4

   contains

   subroutine VBO_ScaleMesh(self,factor)

      type(VBOMesh), target :: self
      real, intent(in) :: factor

      type(C_PTR) :: VertexData
      integer, pointer :: fptr
      real, dimension(1), target :: tmp

      write(*,*)'VBO_ScaleMesh ',factor
      
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(VERTEX_DATA))
      VertexData = glMapBuffer(GL_ARRAY_BUFFER, GL_READ_WRITE)
     
      call c_f_pointer(cptr=VertexData, fptr=fptr)

      !fptr => tmp(1)
      !tmp(1:self%nNumVerts*3) = factor * tmp(1:self%nNumVerts*3)
      
      call glUnmapBuffer(GL_ARRAY_BUFFER)
      
   end subroutine VBO_ScaleMesh
   subroutine VBO_Draw(self)

      use opengl_glut
      
      type(VBOMesh) :: self

     ! Here's where the data is now
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(VERTEX_DATA))
      call glVertexPointer(3, GL_FLOAT,0, c_loc(self%Verts))
    
      ! Normal data
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(NORMAL_DATA))
      call glNormalPointer(GL_FLOAT, 0, c_loc(self%Norms))
           
      ! Texture coordinates
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(TEXTURE_DATA))
      call glTexCoordPointer(2, GL_FLOAT, 0, c_loc(self%TexCoords))

      ! Indexes
      call glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, self%bufferObjects(INDEX_DATA))
      !
      ! glDrawElements( GLenum mode, GLsizei count,
      !                 GLenum type, const GLvoid *indices )
      !
      ! type Specifies the type of the values in indices. Must
      ! be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or GL_UNSIGNED_INT
      !
      call glDrawElements(GL_TRIANGLES, self%nNumIndexes, &
           GL_UNSIGNED_INT, c_loc(self%Indexes))
 
   end subroutine VBO_Draw
   subroutine VBO_BeginMesh(self, n )
   ! Start assembling a mesh. You need to specify a maximum amount
   ! of indexes that you expect. The EndMesh will clean up any uneeded
   ! memory. This is far better than shreading your heap with STL containers...
   ! At least that's my humble opinion.

      type(VBOMesh) :: self
      integer, intent(in) :: n
   
      ! Just in case this gets called more than once...
      if( allocated( self%Indexes   ) ) deallocate( self%Indexes   )
      if( allocated( self%Verts     ) ) deallocate( self%Verts     )
      if( allocated( self%Norms     ) ) deallocate( self%Norms     )
      if( allocated( self%TexCoords ) ) deallocate( self%TexCoords )

      self%nMaxIndexes = n
      self%nNumIndexes = 0
      self%nNumVerts   = 0

      ! Allocate new blocks
      allocate( self%Indexes(nMaxIndexes), stat=i1 )
      allocate( self%Verts(nMaxIndexes*3), stat=i2 )
      allocate( self%Norms(nMaxIndexes*3), stat=i3 )
      allocate( self%TexCoords(nMaxIndexes*3), stat=i4 )

      write(*,*)'New VBO mesh allocated ',n
      if( i1 /= 0 ) write(*,*)'Status i1 ',i1
      if( i2 /= 0 ) write(*,*)'Status i2 ',i2
      if( i3 /= 0 ) write(*,*)'Status i3 ',i3
      if( i4 /= 0 ) write(*,*)'Status i4 ',i4

   end subroutine VBO_BeginMesh
   subroutine VBO_AddTriangle(self, v, n, t)
   ! Add a triangle to the mesh. This searches the current list for identical
   ! (well, almost identical - these are floats you know...) verts. If one is found, it
   ! is added to the index array. If not, it is added to both the index 
   ! array and the vertex array grows by one as well.

      type(VBOMesh) :: self

      real, dimension(3,3), intent(in)    :: v
      real, dimension(3,3), intent(inout) :: n
      real, dimension(3,3), intent(in)    :: t
      
      real :: e = 1.e-6    ! How small a difference to equate

      logical :: m3dCloseEnough, new
      
      integer, save :: icount = 0
      
      icount = icount + 1
      ! First thing we do is make sure the normals are unit length!
      ! It's almost always a good idea to work with pre-normalized normals
      !
      do i=1,3
        call m3dNormalizeVector(n(i,:))
      end do
      ! Search for match - triangle consists of three verts
      !
      new = .true.
      do i=1,3 
        do j=1,self%nNumVerts 
          k   = (j-1)*3+1 
          new = .true.
          ! If the vertex positions are the same
          if( m3dCloseEnough(self%Verts( k ),v(i,1),e) .and.     &
              m3dCloseEnough(self%Verts(k+1),v(i,2),e) .and.     &
              m3dCloseEnough(self%Verts(k+2),v(i,3),e) .and.     &
              ! AND the Normal is the same...
              m3dCloseEnough(self%Norms( k ),n(i,1),e) .and.     &
              m3dCloseEnough(self%Norms(k+1),n(i,2),e) .and.     &
              m3dCloseEnough(self%Norms(k+2),n(i,3),e) .and.     &
              ! AND Texture is the same...
              m3dCloseEnough(self%TexCoords( k ),t(i,1),e) .and. &
              m3dCloseEnough(self%TexCoords(k+1),t(i,2),e) )then
          
            ! Then add the index only
            self%Indexes(self%nNumIndexes+1) = j
            self%nNumIndexes = self%nNumIndexes + 1
            new = .false.
	    exit
          endif
        end do
      
        !  No match for this vertex, add to end of list
        if( new )then
          k = self%nNumVerts*3 + 1
          self%Verts(k:k+2) = v(i,:)
          self%Norms(k:k+2) = n(i,:)
          self%TexCoords(k:k+2) = t(i,:)

          self%Indexes(self%nNumIndexes+1) = self%nNumVerts+1
          
          self%nNumIndexes = self%nNumIndexes + 1
          self%nNumVerts   = self%nNumVerts + 1
        endif
      end do

   end subroutine VBO_AddTriangle
   subroutine VBO_EndMesh(self)
   ! Compact the data. This is a nice utility, but you should really
   ! save the results of the indexing for future use if the model data
   ! is static (doesn't change).

      use OpenGL_gl
      use OpenGL_glut
      use Opengl_glext
      
      type(VBOMesh) :: self
      integer(kind=GLint)   :: sizeof
      real, dimension(:) , allocatable :: tmp
 
      write(*,*)'Buffers',self%nNumIndexes,self%nNumVerts
 
      nv = self%nNumVerts*3
      allocate(tmp(nv), stat=i1)
      if( i1 /= 0 ) write(*,*)'Allocation of tmp array failed ',i1
      
      ! vertices
      tmp = self%Verts(1:nv)      
      deallocate( self%Verts )
      allocate( self%Verts(nv), stat=i2 )
      if( i2 /= 0 ) write(*,*)'Allocation of self%verts array failed ',i2
      
      self%Verts(1:nv) = tmp(1:nv)
      
      ! normals
      tmp = self%Norms(1:nv)      
      deallocate( self%Norms )
      allocate( self%Norms(nv), stat=i3 )
      if( i3 /= 0 ) write(*,*)'Allocation of self%norms array failed ',i3
      
      self%Norms(1:nv) = tmp(1:nv)

      ! textures
      tmp = self%TexCoords(1:nv)
      deallocate( self%TexCoords )
      allocate( self%TexCoords(self%nNumVerts*2), stat=i4 )
      if( i4 /= 0 ) write(*,*)'Allocation of self%TexCoords array failed ',i4

      j = 1
      do i=1,self%nNumVerts*3,3
        self%TexCoords(j:j+1) = tmp(i:i+1)
        j= j + 2
      end do
      deallocate(tmp)
      
      !
      ! glGenBuffers (GLsizei, GLuint *)
      ! glBindBuffer (GLenum, GLuint)
      ! glBufferData (GLenum, GLsizeiptr, const GLvoid *, GLenum)
      !
      
      ! Create the buffer objects
     !call glGenBuffers(NbufferObjects, self%bufferObjects)
      call glGenBuffers(NbufferObjects,c_loc(self%bufferObjects))
      
      ! Copy data to video memory
      ! Vertex data
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(VERTEX_DATA))
      call glBufferData(GL_ARRAY_BUFFER, int(sizeof(GLfloat)*self%nNumVerts*3,C_INT), & 
                        c_loc(self%Verts), GL_STATIC_DRAW)

      ! Normal data
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(NORMAL_DATA))
      call glBufferData(GL_ARRAY_BUFFER, int(sizeof(GLfloat)*self%nNumVerts*3,C_INT), &
                        c_loc(self%Norms), GL_STATIC_DRAW)

      ! Texture coordinates
      call glBindBuffer(GL_ARRAY_BUFFER, self%bufferObjects(TEXTURE_DATA))
      call glBufferData(GL_ARRAY_BUFFER, int(sizeof(GLfloat)*self%nNumVerts*2,C_INT), &
                        c_loc(self%TexCoords), GL_STATIC_DRAW)

      ! Indexes (shift array first to C-style indexing)
      self%Indexes(1:self%nNumIndexes) = self%Indexes(1:self%nNumIndexes)-1
      call glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, self%bufferObjects(INDEX_DATA))
      call glBufferData(GL_ELEMENT_ARRAY_BUFFER, int(sizeof(GLuint)*self%nNumIndexes,C_INT), &
                        c_loc(self%Indexes), GL_STATIC_DRAW)

      write(*,*)'VBO buffers ready'
      write(*,*)'NumIndexes:',self%nNumIndexes
      write(*,*)'NumVerts  :',self%nNumVerts
      
   end  subroutine VBO_EndMesh
    
end module VBOmeshes
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
module CTriangleMeshes

   use OpenGL_gl

   type :: CTriangleMesh
     integer(kind=GLuint), dimension(:), allocatable :: Indexes
     real(kind=GLfloat),   dimension(:), allocatable :: Verts
     real(kind=GLfloat),   dimension(:), allocatable :: Norms
     real(kind=GLfloat),   dimension(:), allocatable :: TexCoords

     integer(kind=GLint) :: nMaxIndexes = 0 ! maximum workspace 
     integer(kind=GLint) :: nNumIndexes = 0 ! Number of indexes currently used
     integer(kind=GLint) :: nNumVerts   = 0 ! Number of vertices currently used
   end type CTriangleMesh

   contains

   subroutine CT_ScaleMesh(self,factor)

      type(CTriangleMesh) :: self
      real, intent(in) :: factor

      self%Verts(1:self%nNumVerts*3) = factor * self%Verts(1:self%nNumVerts*3)
      
   end subroutine CT_ScaleMesh
   subroutine CT_Draw(self)

      type(CTriangleMesh), target :: self

      call glVertexPointer(3, GL_FLOAT,0, c_loc(self%Verts))
      call glNormalPointer(GL_FLOAT, 0, c_loc(self%Norms))
      call glTexCoordPointer(2, GL_FLOAT, 0, c_loc(self%TexCoords))

      ! type Specifies the type of the values in indices. Must
      !      be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or
      !      GL_UNSIGNED_INT     
      !
      ! Draw them
      call glDrawElements(GL_TRIANGLES, self%nNumIndexes, &
                          GL_UNSIGNED_INT, c_loc(self%Indexes))
   
   end subroutine CT_Draw
   subroutine CT_BeginMesh(self, n )
   ! Start assembling a mesh. You need to specify a maximum amount
   ! of indexes that you expect. The EndMesh will clean up any uneeded
   ! memory. This is far better than shreading your heap with STL containers...
   ! At least that's my humble opinion.

      type(CTriangleMesh), intent(inout) :: self
      integer, intent(in) :: n
   
      ! Just in case this gets called more than once...
      if( allocated( self%Indexes   ) ) deallocate( self%Indexes   )
      if( allocated( self%Verts     ) ) deallocate( self%Verts     )
      if( allocated( self%Norms     ) ) deallocate( self%Norms     )
      if( allocated( self%TexCoords ) ) deallocate( self%TexCoords )

      self%nMaxIndexes = n
      self%nNumIndexes = 0
      self%nNumVerts   = 0

      ! Allocate new blocks 
      ! (ifort seems to need the status in order to force allocation)
      allocate( self%Indexes(1:nMaxIndexes), stat=i1 )
      allocate( self%Verts(1:nMaxIndexes*3), stat=i2 )
      allocate( self%Norms(1:nMaxIndexes*3), stat=i3 )
      allocate( self%TexCoords(1:nMaxIndexes*3), stat=i4 )
       
      write(*,*)'New CT mesh allocated ',n
      if( i1 /= 0 ) write(*,*)'Status i1 ',i1
      if( i2 /= 0 ) write(*,*)'Status i2 ',i2
      if( i3 /= 0 ) write(*,*)'Status i3 ',i3
      if( i4 /= 0 ) write(*,*)'Status i4 ',i4

   end subroutine CT_BeginMesh
   subroutine CT_AddTriangle(self, v, n, t)
   ! Add a triangle to the mesh. This searches the current list for identical
   ! (well, almost identical - these are floats you know...) verts. If one is found, it
   ! is added to the index array. If not, it is added to both the index array and the vertex
   ! array grows by one as well.

      type(CTriangleMesh) :: self

      real, dimension(3,3), intent(in)    :: v
      real, dimension(3,3), intent(inout) :: n
      real, dimension(3,3), intent(in)    :: t
      
      real :: e = 1.e-6    ! How small a difference to equate

      logical :: m3dCloseEnough, new
      
      integer, save :: icount = 0
      
      icount = icount + 1
      ! First thing we do is make sure the normals are unit length!
      ! It's almost always a good idea to work with pre-normalized normals
      !
      do i=1,3
        call m3dNormalizeVector(n(i,:))
      end do

      ! Search for match - triangle consists of three verts
      new = .true.
      do i=1,3 
        k   = 1
        new = .true.
        do j=1,self%nNumVerts 
          k   = (j-1)*3+1 
          ! If the vertex positions are the same
          if( m3dCloseEnough(self%Verts( k ),v(i,1),e) .and.     &
              m3dCloseEnough(self%Verts(k+1),v(i,2),e) .and.     &
              m3dCloseEnough(self%Verts(k+2),v(i,3),e) .and.     &
              ! AND the Normal is the same...
              m3dCloseEnough(self%Norms( k ),n(i,1),e) .and.     &
              m3dCloseEnough(self%Norms(k+1),n(i,2),e) .and.     &
              m3dCloseEnough(self%Norms(k+2),n(i,3),e) .and.     &
              ! AND Texture is the same...
              m3dCloseEnough(self%TexCoords( k ),t(i,1),e) .and. &
              m3dCloseEnough(self%TexCoords(k+1),t(i,2),e) )then
          
            ! Then add the index only
            self%Indexes(self%nNumIndexes+1) = j
            self%nNumIndexes = self%nNumIndexes + 1
            new = .false.
            exit
          endif
        end do
      
        !  No match for this vertex, add to end of list
        if( new )then
          k1 = self%nNumVerts*3 + 1
          k2 = k1 + 2
          self%Verts(k1:k2) = v(i,1:3)
          self%Norms(k1:k2) = n(i,1:3)
          self%TexCoords(k1:k2) = t(i,1:3)

          self%Indexes(self%nNumIndexes+1) = self%nNumVerts+1
          
          self%nNumIndexes = self%nNumIndexes + 1
          self%nNumVerts   = self%nNumVerts + 1
        endif
      end do

   end subroutine CT_AddTriangle
   subroutine CT_EndMesh(self)
   ! Compact the data. This is a nice utility, but you should really
   ! save the results of the indexing for future use if the model data
   ! is static (doesn't change).

      type(CTriangleMesh) :: self

      real, dimension(:) , allocatable :: tmp
 
      nv = self%nNumVerts*3
      
      allocate(tmp(nv))

      ! vertices
      tmp = self%Verts(1:nv)      
      deallocate( self%Verts )
      allocate( self%Verts(nv) )
      
      self%Verts(1:nv) = tmp(1:nv)
      
      ! normals
      tmp = self%Norms(1:nv)      
      deallocate( self%Norms )
      allocate( self%Norms(nv) )
      
      self%Norms(1:nv) = tmp(1:nv)
      
      ! textures
      tmp = self%TexCoords(1:nv)
      deallocate( self%TexCoords )
      allocate( self%TexCoords(self%nNumVerts*2) )  ! note: now 2 instead of 3!
            
      j = 1
      do i=1,self%nNumVerts*3,3
        self%TexCoords(j:j+1) = tmp(i:i+1)
        j= j + 2
      end do
      
      deallocate(tmp)

      self%Indexes(1:self%nNumIndexes) = self%Indexes(1:self%nNumIndexes)-1

      if( self%nNumVerts > 4000 )then
        j=1
        k=1
        do i=1,self%nNumVerts
          write(*,*)   
          write(*,*) 'v',i,self%Verts(j:j+2)
          write(*,*) 'n',i,self%norms(j:j+2)
          write(*,*) 't',i,self%TexCoords(k:k+1)
          j=j+3
          k=k+2
        end do
        do i=1,self%nNumVerts,2
          if( self%TexCoords( i ) < 0.0 .or. &
	      self%TexCoords(i+1) < 0.0 )then
          write(*,*) 't',i,self%TexCoords(i:i+1)
        endif
      end do	
      endif
      
      write(*,*)'NumIndexes:',self%nNumIndexes
      write(*,*)'NumVerts  :',self%nNumVerts
      
   end  subroutine CT_EndMesh
    
end module CTriangleMeshes
!====================================================================
module GLframes

   use opengl_gl
   use opengl_glut

   ! vOrigin  = Where am I?
   ! vUp      = Which way is up?
   ! vForward = Where am I going?

   ! Default position and orientation. At the origin, looking
   ! down the positive Z axis (right handed coordinate system).
   type :: GLFrame
     sequence
     real,dimension(3) :: Org = (/0.0,0.0, 0.0/) ! At origin
     real,dimension(3) :: Up  = (/0.0,1.0, 0.0/) ! Up is up (+Y)
     real,dimension(3) :: Frw = (/0.0,0.0,-1.0/) ! Forward is -Z (default OpenGL) 
   end type GLFrame

   contains

   subroutine SetOrigin(self,x,y,z)
     type(GLframe) :: self
     self%Org(1) = x
     self%Org(2) = y
     self%Org(3) = z
   end subroutine SetOrigin

   subroutine MoveForward(self,x)
     type(GLframe) :: self
     self%Org(1) = self%Org(1) + self%Frw(1)*x
     self%Org(2) = self%Org(2) + self%Frw(2)*x
     self%Org(3) = self%Org(3) + self%Frw(3)*x
   end subroutine MoveForward

   subroutine MoveUp(self,x)
     type(GLframe) :: self
     self%Org(1) = self%Org(1) + self%Up(1)*x
     self%Org(2) = self%Org(2) + self%Up(2)*x
     self%Org(3) = self%Org(3) + self%Up(3)*x
   end subroutine MoveUp

   subroutine RotateLocalX(self,angle)
     type(GLframe) :: self
     real, dimension(16) :: mat
     real, dimension(3)  :: vCross, vec

     call m3dCrossProduct(vCross,self%Up,self%Frw)
     
     call m3dRotationMatrix44(mat, angle, vcross(1), vcross(2), vcross(3))
     
     ! Inline 3x3 matrix multiply for rotation only
     vec(1) = mat(1)*self%frw(1) + mat(5)*self%frw(2) + mat( 9)*self%frw(3)     
     vec(2) = mat(2)*self%frw(1) + mat(6)*self%frw(2) + mat(10)*self%frw(3)     
     vec(3) = mat(3)*self%frw(1) + mat(7)*self%frw(2) + mat(11)*self%frw(3)     

     self%frw = vec

     ! Update pointing up vector
     vec(1) = mat(1)*self%up(1) + mat(5)*self%up(2) + mat( 9)*self%up(3)     
     vec(2) = mat(2)*self%up(1) + mat(6)*self%up(2) + mat(10)*self%up(3)     
     vec(3) = mat(3)*self%up(1) + mat(7)*self%up(2) + mat(11)*self%up(3)     

     self%up = vec

   end subroutine RotateLocalX

   subroutine RotateLocalY(self,angle)
     type(GLframe) :: self
     real, dimension(16) :: mat
     real, dimension(3)  :: vec
     ! Just Rotate around the up vector
     ! Create a rotation matrix around my Up (Y) vector
     call m3dRotationMatrix44(mat, angle, &
                              self%Up(1), self%Up(2), self%Up(3))
     ! Rotate forward pointing vector (inlined 3x3 transform)
     vec(1) = mat(1)*self%frw(1) + mat(5)*self%frw(2) + mat( 9)*self%frw(3)     
     vec(2) = mat(2)*self%frw(1) + mat(6)*self%frw(2) + mat(10)*self%frw(3)     
     vec(3) = mat(3)*self%frw(1) + mat(7)*self%frw(2) + mat(11)*self%frw(3)     

     self%Frw(1) = vec(1)
     self%Frw(2) = vec(2)
     self%Frw(3) = vec(3)
   end subroutine RotateLocalY

   subroutine ApplyCameraTransform(self,RotOnly)
     type(GLframe) :: self
     logical, OPTIONAL :: RotOnly
     real, dimension(16)  :: mat
     real, dimension(4,4) :: m
     real, dimension(3)   :: x, z

     call GetCameraOrientation(self,mat)
     call glMultMatrixf(mat)               ! Camera Transform
     ! If Rotation only, then do not do the translation
     if( present(RotOnly) )then
       if( .not. RotOnly ) &
         call glTranslatef(-self%Org(1),-self%Org(2),-self%Org(3))
     else
       call glTranslatef(-self%Org(1),-self%Org(2),-self%Org(3))
     endif
   end subroutine ApplyCameraTransform

   subroutine GetCameraOrientation(self,mat)
     ! Get a 4x4 transformation matrix that 
     ! describes the camera orientation.
     type(GLframe) :: self
     real, dimension(16)  :: mat
     real, dimension(4,4) :: m
     real, dimension(3)   :: x, z
     ! Make rotation matrix, Z vector is reversed
     z(1) = -self%frw(1)
     z(2) = -self%frw(2)
     z(3) = -self%frw(3)
     ! X vector = Y cross Z 
     call m3dCrossProduct(x, self%Up, z)
     ! Matrix has no translation information and is
     ! transposed.... (rows instead of columns)
     !
     ! Xx Yx Zx Tx
     ! Xy Yy Zy Ty
     ! Xz Yz Zz Tz
     ! 0   0  0  1
     !
     m(1,1) = x(1)
     m(2,1) = x(2)
     m(3,1) = x(3)
     m(4,1) = 0.0
     m(1,2) = self%Up(1)
     m(2,2) = self%Up(2)
     m(3,2) = self%Up(3)
     m(4,2) = 0.0
     m(1,3) = z(1)
     m(2,3) = z(2)
     m(3,3) = z(3)
     m(4,3) = 0.0
     m(1,4) = 0.0
     m(2,4) = 0.0
     m(3,4) = 0.0
     m(4,4) = 1.0
     do j=1,4
       do i=1,4
         mat((j-1)*4+i) = m(i,j)
       end do
     end do 
   end subroutine GetCameraOrientation

   subroutine ApplyActorTransform(self,RotationOnly)
     type(GLframe) :: self
     real, dimension(16)  :: mat
     logical, OPTIONAL    :: RotationOnly

     if( present(RotationOnly) )then
       call GetMatrix(self,mat,RotationOnly)
     else
       call GetMatrix(self,mat)
     endif
     ! Apply rotation to the current matrix
     call glMultMatrixf(mat)

   end subroutine ApplyActorTransform

   subroutine GetMatrix(self,matrix,RotationOnly)
     ! Just assemble the matrix
     type(GLframe) :: self
     real, dimension(16)  :: matrix
     real, dimension(3)   :: vXAxis
     logical, OPTIONAL    :: RotationOnly
     ! Calculate the right side (x) vector, 
     ! drop it right into the matrix
     call m3dCrossProduct(vXAxis, self%Up, self%frw)
     ! Set matrix column does not fill in the fourth value...
     call m3dSetMatrixColumn44(matrix, vXAxis, 1)
     matrix(4) = 0.0
     ! Y Column
     call m3dSetMatrixColumn44(matrix, self%up, 2)
     matrix(8) = 0.0     
     ! Z Column
     call m3dSetMatrixColumn44(matrix, self%frw, 3)
     matrix(12) = 0.0
     ! Translation (already done)
     if( present(RotationOnly) )then
       if( RotationOnly )then
         matrix(13) = 0.0
         matrix(14) = 0.0
         matrix(15) = 0.0
       else
         call m3dSetMatrixColumn44(matrix, self%org, 4)
       endif
     else
       call m3dSetMatrixColumn44(matrix, self%org, 4)
     endif
     matrix(16) = 1.0
   end subroutine GetMatrix
  
end module GLframes
!====================================================================
!
!  gltools.f90
!
!  Originally created by Richard Wright on 10/16/06.
!  OpenGL SuperBible, 4th Edition
!
subroutine gltDrawTorus( majorRadius, minorRadius, numMajor, numMinor)
!
! For best results, put this in a display list
! Draw a torus (doughnut)  at z = fZVal... torus is in xy plane
!
   use opengl_gl

   real, intent(in)    :: majorRadius, minorRadius
   integer, intent(in) :: numMajor, numMinor
   
   real, dimension(3) :: vNormal
   real :: majorStep, minorStep
   
   majorStep = 2.0*3.1415926 / numMajor
   minorStep = 2.0*3.1415926 / numMinor

   do i=0,numMajor-1

     a0 = i * majorStep
     a1 = a0 + majorStep
     
     x0 = cos(a0)
     y0 = sin(a0)
     x1 = cos(a1)
     y1 = sin(a1)
               
     call glBegin(GL_TRIANGLE_STRIP)
     do j=0,numMinor
       
       b = j * minorStep
       c = cos(b)
       r = minorRadius * c + majorRadius
       z = minorRadius * sin(b)
       
       ! First point
       call glTexCoord2f( float(i)/float(numMajor), float(j)/float(numMinor))
       vNormal(1) = x0*c
       vNormal(2) = y0*c
       vNormal(3) = z/minorRadius
       call m3dNormalizeVector(vNormal)
       call glNormal3fv(vNormal)
       call glVertex3f(x0*r, y0*r, z)
       
       call glTexCoord2f(float(i+1)/float(numMajor), float(j)/float(numMinor))
       vNormal(1) = x1*c
       vNormal(2) = y1*c
       vNormal(3) = z/minorRadius
       call m3dNormalizeVector(vNormal)
       call glNormal3fv(vNormal)
       call glVertex3f(x1*r, y1*r, z)
     end do  
     call glEnd 
   end do
   
end subroutine gltDrawTorus
subroutine gltDrawSphere(Radius, iSlices, iStacks)
! For best results, put this in a display list
! Draw a sphere at the origin

   use opengl_gl

   drho   = 3.141592653589 / float(iStacks)
   dtheta = 2.0 * 3.141592653589 / float(iSlices)
   ds = 1.0 / float(iSlices)
   dt = 1.0 / float(iStacks)
   t  = 1.0    
   s  = 0.0
   
   do i=0,iStacks-1

     rho  = float(i) * drho
     srho = sin(rho)
     crho = cos(rho)
     srhodrho = sin(rho + drho)
     crhodrho = cos(rho + drho)
               
     ! Many sources of OpenGL sphere drawing code uses a triangle fan
     ! for the caps of the sphere. This however introduces texturing 
     ! artifacts at the poles on some OpenGL implementations

     call glBegin(GL_TRIANGLE_STRIP)
     s = 0.0
     do j=0,iSlices
       if( j == iSlices )then
         theta = 0.0
       else
         theta = float(j) * dtheta
       endif
       stheta = -sin(theta)
       ctheta =  cos(theta)

       x = stheta * srho
       y = ctheta * srho
       z = crho
           
       call glTexCoord2f(s, t)
       call glNormal3f(x, y, z)
       call glVertex3f(x * Radius, y * Radius, z * Radius)

       x = stheta * srhodrho
       y = ctheta * srhodrho
       z = crhodrho
       call glTexCoord2f(s, t - dt)
       
       s = s + ds
       call glNormal3f(x, y, z)
       call glVertex3f(x * Radius, y * Radius, z * Radius)
     end do
     call glEnd

     t = t - dt
   end do

end subroutine gltDrawSphere    
function convert_ushort_to_int( ushort )

   use iso_c_binding

   integer :: convert_ushort_to_int
   integer, parameter :: N = 16

   integer(C_SHORT), intent(in)   :: ushort

   integer, dimension(N) :: bits(0:N-1)

   bits = 0
   do i=0,N-1
     if( btest(ushort,i) ) bits(i) = 1
   end do
   write(*,'('' ushort bits>'',$)') 
   do i=N-1,0,-1
     write(*,'(i1,$)') bits(i)
   end do
   write(*,'(''<'')')

   ! check highest bit
   if( btest(ushort,N-1) )then
     ibig = 32768
   else
     ibig =     0
   endif
   ! clear highest bit 
   iconv = ibclr(ushort,N-1) 
   ! simple type conversion:

   convert_ushort_to_int = iconv + ibig
  
end function convert_ushort_to_int
function convert_byte_to_int( byte )

   use iso_c_binding

   integer :: convert_byte_to_int
   integer, parameter :: N = 8

   integer(C_CHAR), intent(in) :: byte

   integer, dimension(N) :: bits(0:N-1)

   bits = 0
   do i=0,N-1
     if( btest(byte,i) ) bits(i) = 1
   end do
   write(*,'('' byte>'',$)') 
   do i=N-1,0,-1
     write(*,'(i1,$)') bits(i)
   end do
   write(*,'(''<'')')

   ! check highest bit
   if( btest(byte,N-1) )then
     ibig = 128
   else
     ibig =   0
   endif
   ! clear highest bit 
   iconv = ibclr(byte,N-1) 
   ! simple type conversion:

   convert_byte_to_int = iconv + ibig
  
end function convert_byte_to_int
function show_byte(byte)

   use iso_c_binding

   integer :: show_byte
   integer, parameter :: N = 8

   integer(C_CHAR), intent(in) :: byte
   integer, dimension(N) :: bits(0:N-1)

   bits = 0
   do i=0,N-1
     if( btest(byte,i) ) bits(i) = 1
   end do
   write(*,'('' byte>'',$)') 
   do i=N-1,0,-1
     write(*,'(i1,$)') bits(i)
   end do
   write(*,'(''=>'',i,$)') byte
   write(*,'(''<'')')

   show_byte = 1

end function show_byte
function show_cint(ci)

   use iso_c_binding

   integer :: show_cint
   integer, parameter :: N = 16

   integer(C_INT), intent(in) :: ci
   integer, dimension(N) :: bits(0:N-1)

   bits = 0
   do i=0,N-1
     if( btest(ci,i) ) bits(i) = 1
   end do
   write(*,'('' cint>'',$)') 
   do i=N-1,0,-1
     write(*,'(i1,$)') bits(i)
   end do
   write(*,'(''=>'',i,$)') ci
   write(*,'(''<'')')

   show_cint = 1

end function show_cint
function convert_uint_to_real( uint )

   use iso_c_binding

   integer, parameter :: N = 32

   integer(C_INT), intent(in) :: uint

   integer, dimension(N) :: bits(0:N-1)
   real                  :: big
  
  !bits = 0
  !do i=0,N-1
  !  if( btest(uint,i) ) bits(i) = 1
  !end do
  !write(*,'('' bits>'',$)') 
  !do i=N-1,0,-1
  !  write(*,'(i1,$)') bits(i)
  !end do
  !write(*,'(''<'')')

   ! check highest bit
   if( btest(uint,N-1) )then
     big = float(4294967295)
   else
     big = 0.0
   endif
   ! clear highest bit 
   iconv = ibclr(uint,N-1) 
   ! simple type conversion:

   convert_uint_to_real = float(iconv) + big
  
end function convert_uint_to_real
function fgltLoadTGA(FileName, iWidth, iHeight, iComponents, eFormat, image)

   use, intrinsic :: iso_c_binding

   type(C_PTR), pointer :: fgltLoadTGA

   character(len=*), intent(in) :: filename
   integer(C_INT), intent(out)  :: iWidth, iHeight, iComponents
   integer(C_INT), intent(out)  :: eFormat
   integer :: convert_ushort_to_int, convert_byte_to_int
       
   integer(C_CHAR), dimension(:), allocatable, intent(out), target :: image
       
   type :: TGAheader                            !   bit kind
     character(C_SIGNED_CHAR) :: identsize	    !  1  8  1  Size of ID field that follows header (0)
     character(C_SIGNED_CHAR) :: colorMapType	!  2  8  1  0 = None, 1 = paletted
     character(C_SIGNED_CHAR) :: imageType	    !  3  8  1  0 = none, 1 = indexed, 2 = rgb, 3 = grey, +8=rle
     integer(C_SHORT)         :: colorMapStart  !  4 16  2  First colour map entry  USHORT
     integer(C_SHORT)         :: colorMapLength !  5 16  2  Number of colors        USHORT
     character(C_SIGNED_CHAR) :: colorMapBits   !  6  8  1  bits per palette entry   
     integer(C_SHORT)         :: xstart         !  7 16  2  image x origin          USHORT
    !integer(C_SHORT)         :: ystart         !  8 16  2  image y origin          USHORT
     integer(C_SHORT)         :: width          !  9 16  2  width in pixels         USHORT
     integer(C_SHORT)         :: height         ! 10 16  2  height in pixels        USHORT
     character(C_SIGNED_CHAR) :: bits           ! 11  8  1  bits per pixel (8, 16, 24, 32)
     character(C_SIGNED_CHAR) :: descriptor     ! 12  8  1  image descriptor
   end type

   type(TGAheader) :: header
     
   ! Default/Failed values
   fgltLoadTGA => null()
   iWidth      = 0
   iHeight     = 0
   eFormat     = 0
   iComponents = 0

   write(*,*) 'open ',FileName
   ! Attempt to open the file
   open(11,file=FileName,access='STREAM',form='UNFORMATTED',status='OLD')

   ! Read in header (binary)
   read(11) header

   iWidth  = convert_ushort_to_int(header%width)
   iHeight = convert_ushort_to_int(header%height)
   iDepth  = convert_byte_to_int(header%bits) / 8

   iSize   = iWidth * iHeight * iDepth
   allocate(image(1:iSize))
   read(11) image
   
   fgltLoadTGA => image(1)
   
   close(11)

   select case(iDepth)
     case(3)
       write(*,*) 'image RGB8'
       iComponents = z'8051'
       eformat     = z'80E0'
     case(4)
       write(*,*) 'image RGBa8'
       iComponents = z'8058'
       eformat     = z'80E1'    
     case(1)
       write(*,*) 'image LUMINANCE'
       iComponents = z'8040'
       eformat     = z'1909'
   end select
   
   write(*,*)'image ',iWidth,iHeight,iDepth

end function fgltLoadTGA




