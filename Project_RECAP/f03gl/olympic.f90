module olympic

  use opengl_gl
  use opengl_glu
  use opengl_glut

  integer, parameter :: &
     XSIZE      = 100, &
     YSIZE      = 75, &
     RINGS      = 5, &
     BLUERING   = 0, &
     BLACKRING  = 1, &
     REDRING    = 2, &
     YELLOWRING = 3, &
     GREENRING  = 4, &
     BLACK      = 0, &
     RED        = 1, &
     GREEN      = 2, &
     YELLOW     = 3, &
     BLUE       = 4, &
     MAGENTA    = 5, &
     CYAN       = 6, &
     WHITE      = 7

  real(glfloat), parameter :: BACKGROUND = 8.

  integer, parameter :: double = kind(0.0d0)

  real, parameter :: M_PI = 3.141592654

  integer(glenum) rgb, doubleBuffer, directRender

  integer(glubyte) rgb_colors(0:RINGS-1,0:2)
  integer(glint) mapped_colors(0:RINGS-1)
  real(glfloat) dests(0:RINGS-1,0:2)
  real(glfloat) offsets(0:RINGS-1,0:2)
  real(glfloat) angs(0:RINGS-1)
  real(glfloat) rotAxis(0:RINGS-1,0:2)
  integer iters(0:RINGS-1)
  integer(gluint) theTorus

  contains

  subroutine FillTorus(rc, numc, rt, numt)
    real, intent(in) :: rc, rt
    integer, intent(in) :: numc, numt

    integer :: i, j, k
    real :: s, t
    real(glfloat) x, y, z
    real pi, twopi

    pi = M_PI
    twopi = 2 * pi

    do i = 0, numc-1
      call glBegin(GL_QUAD_STRIP)
      do j = 0, numt
        do k = 1, 0, -1
          s = mod((i + k), numc) + 0.5
          t = mod(j, numt)

          x = cos(t * twopi / numt) * cos(s * twopi / numc)
          y = sin(t * twopi / numt) * cos(s * twopi / numc)
          z = sin(s * twopi / numc)
          call glNormal3f(x, y, z)

          x = (rt + rc * cos(s * twopi / numc)) * cos(t * twopi / numt)
          y = (rt + rc * cos(s * twopi / numc)) * sin(t * twopi / numt)
          z = rc * sin(s * twopi / numc)
          call glVertex3f(x, y, z)
        end do
      end do
      call glEnd()
    end do
  
    return
  
  end subroutine filltorus
  function Clamp(iters_left,t)
    real :: clamp
    integer, intent(in) :: iters_left
    real, intent(in) :: t

    if (iters_left < 3) then
      clamp = 0.0
    else
      clamp = (iters_left - 2) * t / iters_left
    endif
  
    return
  
  end function clamp
  function MyRand()
    real :: myrand, rval

    call random_number(rval)
    MyRand = 10.0 * (rval - 0.5)

    return
  
  end function MyRand
  subroutine ReInit()

    integer :: i
    real :: deviation

    deviation = MyRand() / 2
    deviation = deviation * deviation
    do i=0,RINGS-1
      offsets(i,0) = MyRand()
      offsets(i,1) = MyRand()
      offsets(i,2) = MyRand()
      angs(i) = 260.0 * MyRand()
      rotAxis(i,0) = MyRand()
      rotAxis(i,1) = MyRand()
      rotAxis(i,2) = MyRand()
      iters(i) = (deviation * MyRand() + 60.0)
    enddo

    return
  
  end subroutine reinit
  subroutine Init()

    real(glfloat) :: top_y = 1.0
    real(glfloat) :: bottom_y = 0.0
    real(glfloat) :: top_z = 0.15
    real(glfloat) :: bottom_z = 0.69
    real(glfloat) :: spacing = 2.5
    real(glfloat), save :: lmodel_ambient(4) = (/0.0, 0.0, 0.0, 0.0/)
    real(glfloat), save :: lmodel_twoside(1) = (/GL_FALSE/)
    real(glfloat), save :: lmodel_local(1) = (/GL_FALSE/)
    real(glfloat), save :: light0_ambient(4) = (/0.1, 0.1, 0.1, 1.0/)
    real(glfloat), save :: light0_diffuse(4) = (/1.0, 1.0, 1.0, 0.0/)
    real(glfloat), save :: light0_position(4) = (/0.8660254, 0.5, 1.0, 0.0/)
    real(glfloat), save :: light0_specular(4) = (/1.0, 1.0, 1.0, 0.0/)
    real(glfloat), save :: bevel_mat_ambient(4) = (/0.0, 0.0, 0.0, 1.0/)
    real(glfloat), save :: bevel_mat_shininess(1) = (/40.0/)
    real(glfloat), save :: bevel_mat_specular(4) = (/1.0, 1.0, 1.0, 0.0/)
    real(glfloat), save :: bevel_mat_diffuse(4) = (/1.0, 0.0, 0.0, 0.0/)

    integer(glubyte), parameter :: boz_zff = z'ff'

    call random_seed()
    call ReInit()
    rgb_colors = 0
   !rgb_colors(BLUERING,2)   = z'ff'   <= ifort does not like this
    rgb_colors(BLUERING,2)   = boz_zff
    rgb_colors(REDRING,0)    = boz_zff
    rgb_colors(GREENRING,1)  = boz_zff
    rgb_colors(YELLOWRING,0) = boz_zff
    rgb_colors(YELLOWRING,1) = boz_zff
    
    mapped_colors(BLUERING)   = BLUE
    mapped_colors(REDRING)    = RED
    mapped_colors(GREENRING)  = GREEN
    mapped_colors(YELLOWRING) = YELLOW
    mapped_colors(BLACKRING)  = BLACK
    
    dests(BLUERING,:) = (/-spacing, top_y, top_z/)
    dests(BLACKRING,:) = (/0.0, top_y, top_z/)
    dests(REDRING,:) = (/spacing, top_y, top_z/)
    dests(YELLOWRING,:) = (/-spacing / 2.0, bottom_y, bottom_z/)
    dests(GREENRING,:) = (/spacing / 2.0, bottom_y, bottom_z/)

    theTorus = glGenLists(1)
    call glNewList(theTorus, GL_COMPILE)
    call FillTorus(0.1, 6, 1.0, 25)
    call glEndList()
    call glEnable(GL_CULL_FACE)
    call glCullFace(GL_BACK)
    call glEnable(GL_DEPTH_TEST)
    call glClearDepth(1.0_glclampd)
    if( rgb == GL_TRUE )then
      call glClearColor(0.5_glclampf, 0.5_glclampf, 0.5_glclampf, 0.0_glclampf)
      call glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient)
      call glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse)
      call glLightfv(GL_LIGHT0, GL_SPECULAR, light0_specular)
      call glLightfv(GL_LIGHT0, GL_POSITION, light0_position)
      call glEnable(GL_LIGHT0)

      call glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lmodel_local)
      call glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside)
      call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient)
      call glEnable(GL_LIGHTING)

      call glMaterialfv(GL_FRONT, GL_AMBIENT, bevel_mat_ambient)
      call glMaterialfv(GL_FRONT, GL_SHININESS, bevel_mat_shininess)
      call glMaterialfv(GL_FRONT, GL_SPECULAR, bevel_mat_specular)
      call glMaterialfv(GL_FRONT, GL_DIFFUSE, bevel_mat_diffuse)

      call glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE)
      call glEnable(GL_COLOR_MATERIAL)
      call glShadeModel(GL_SMOOTH)
    else
      call glClearIndex(BACKGROUND)
      call glShadeModel(GL_FLAT)
    endif
    call glMatrixMode(GL_PROJECTION)
    call gluPerspective(45._gldouble, 1.33_gldouble, 0.1_gldouble, 100.0_gldouble)
    call glMatrixMode(GL_MODELVIEW)
  
    return
  
  end subroutine init
  
  subroutine glutnullfunc() bind(C)
  end subroutine glutnullfunc

end module olympic
subroutine Idle() bind(C)

  use olympic 

  integer :: i, j
  integer(glenum) :: more = GL_FALSE

  do i=0,RINGS-1
    if( iters(i) /= 0 )then
      do j = 0, 2
        offsets(i,j) = Clamp(iters(i), offsets(i,j))
      enddo
      angs(i) = Clamp(iters(i), angs(i))
      iters(i) = iters(i) - 1
      more = GL_TRUE
    end if
  enddo
  if( more == GL_TRUE )then
    call glutPostRedisplay()
  else
    call glutIdleFunc(glutnullfunc)
  endif

  return

end subroutine idle
subroutine Reshape(width,height) bind(C)

  use olympic
  
  integer(glInt), intent(in), value :: width, height

! if glInt is not the same as glsizei, width and height will
! need to be copied to variables of the later kind

  call glViewport(0_glint, 0_glint, width, height)

  return

end subroutine reshape
subroutine Key(ikey,x,y) bind(C)

  use olympic

  integer(GLubyte), intent(in), value :: ikey
  integer(GLint), intent(in), value ::  x, y
  integer :: i
  
  interface
    subroutine idle()  bind(C)
    end subroutine idle
  end interface

  i = ikey
  select case(i)
  case(iachar(achar(27))) ! esc
    stop
  case(iachar(' '))
    call ReInit()
    call glutIdleFunc(Idle)
  end select

  return

end subroutine key

! fortran handling of command line arguments is nonstandard, so
! this feature is omitted.  Here is the original C code.

!GLenum
!Args(int argc, char **argv)
!{
!  GLint i;
!
!  rgb = GL_TRUE;
!  doubleBuffer = GL_TRUE;
!
!  for (i = 1; i < argc; i++) {
!    if (strcmp(argv[i], "-ci") == 0) {
!      rgb = GL_FALSE;
!    } else if (strcmp(argv[i], "-rgb") == 0) {
!      rgb = GL_TRUE;
!    } else if (strcmp(argv[i], "-sb") == 0) {
!      doubleBuffer = GL_FALSE;
!    } else if (strcmp(argv[i], "-db") == 0) {
!      doubleBuffer = GL_TRUE;
!    } else {
!      printf("%s (Bad option).\n", argv[i]);
!      return GL_FALSE;
!    }
!  }
!  return GL_TRUE;
!}

subroutine visible(vis) bind(C)

  use olympic
  
  integer(glInt), intent(in), value:: vis

  interface
    subroutine idle() bind(C)
    end subroutine idle
  end interface

  if( vis == GLUT_VISIBLE )then
    call glutIdleFunc(Idle)
  else
    call glutIdleFunc(glutnullfunc)
  endif

  return

end subroutine visible
subroutine DrawScene() bind(C)

  use olympic

  integer :: i
  real(4) :: x,y,z

  call glPushMatrix()
  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call gluLookAt(0._gldouble, 0._gldouble, 10._gldouble, &
                 0._gldouble, 0._gldouble, 0._gldouble, &
                 0._gldouble, 1._gldouble, 0._gldouble)

  do i=0,RINGS-1
    if( rgb == GL_TRUE )then
      call glColor3ub(rgb_colors(i,0),rgb_colors(i,1),rgb_colors(i,2))
    else
      call glIndexi(mapped_colors(i))
    endif
    call glPushMatrix()
    x=dests(i,0) + offsets(i,0)
    y=dests(i,1) + offsets(i,1)
    z=dests(i,2) + offsets(i,2)
    call glTranslatef(x, y, z)
    call glRotatef(angs(i), rotAxis(i,0), rotAxis(i,1), rotAxis(i,2))
    call glCallList(theTorus)
    call glPopMatrix()
  enddo
  
  call glPopMatrix()

  if( doubleBuffer == GL_TRUE )then
    call glutSwapBuffers()
  else
    call glFlush()
  endif

  return

end subroutine drawscene
program main

  use olympic

  integer(glenum) :: type
  integer :: i
  interface
    subroutine Reshape(width,height) bind(C)
      use olympic
      integer(GLint), intent(in), value :: width, height
    end subroutine reshape

    subroutine Key(ikey,x,y) bind(C)
      use olympic
      integer(GLubyte), intent(in), value :: ikey
      integer(glInt), intent(in), value :: x, y
    end subroutine key

    subroutine visible(vis) bind(C)
      use olympic
      integer(glInt), intent(in), value :: vis
    end subroutine visible

    subroutine drawscene() bind(C)
    end subroutine drawscene
  end interface

  call glutInitWindowSize(400_glInt, 300_glInt)

! not checking command line arguments
!  glutInit(&argc, argv);
!  if (Args(argc, argv) == GL_FALSE) {
!    exit(1);
!  }
  call glutinit()
  rgb = GL_TRUE           ! default values which could have been
  doubleBuffer = GL_TRUE  ! overwritten by command line arguments

  if( rgb == GL_TRUE )then
    type = GLUT_RGB
  else
    type = GLUT_INDEX
  endif

  if( doubleBuffer == GL_TRUE )then
    type = ior(type,GLUT_DOUBLE)
  else
    type = ior(type,GLUT_SINGLE)
  endif
  call glutInitDisplayMode(type)

  i = glutCreateWindow("Olympic Rings"//char(0))

  call Init()

  call glutReshapeFunc(Reshape)
  call glutKeyboardFunc(Key)
  call glutDisplayFunc(DrawScene)

  call glutVisibilityFunc(visible)

  call glutMainLoop()

end program main

