!
! This is a simple, introductory OpenGL program.  It originally comes from
! page 6 of the "red book".  This is a Fortran translation and slight
! modification of the program hello.c from the GLUT distribution.
!
module array

  implicit none
  public :: display

  contains

  subroutine display() bind(C)

    use opengl_gl
    use opengl_glu
    use opengl_glut

    real(GLfloat), target :: P(8)=[.25,.25, .75,.25, .75,.75, .25,.75]

    logical, save :: Done = .false.

    !
    ! clear all pixels
    !
    call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)

    !
    ! draw white polygon (rectangle) with corners at
    ! ((0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
    !
    call glcolor3f(1.0_glfloat, 0.0_glfloat, 0.0_glfloat)

    call glVertexPointer( 2, GL_FLOAT, 0, c_loc(P) )
    call glDrawArrays(GL_POLYGON, 0, 4)
    !
    ! don't wait!
    !
    ! start processing buffered OpenGL routines
    !
    call glflush()

  end subroutine display

end module array
program main

  use, intrinsic :: ISO_C_BINDING

  use opengl_gl
  use opengl_glu
  use opengl_glut

  use array

  implicit none

  real(kind=glfloat),  parameter :: zero  = 0.0_glfloat
  real(kind=gldouble), parameter :: dzero = 0.0_gldouble, &
                                    one   = 1.0_gldouble

  integer :: i
  integer(kind=glint) :: iwin, l
  character(len=512), pointer :: str

  integer(kind=glint),dimension(4) :: glgeti

  type(C_PTR), target :: cstr

  !
  ! Declare initial window size, position, and display mode
  ! (single buffer and RGBA).  Open window with "trivial"
  ! in its title bar.
  !
  call glutinit()
  call glutinitdisplaymode(GLUT_SINGLE+GLUT_RGB+GLUT_DEPTH)
  iwin = glutcreatewindow("trivial"//char(0))

  !
  ! select clearing color
  !
  call glclearcolor(zero, zero, zero, zero)

  !
  ! initialize viewing values
  !
  call glmatrixmode(GL_PROJECTION)
  call glloadidentity()
  call glortho(dzero, one, dzero, one, -one, one)
  call glEnableClientState(GL_VERTEX_ARRAY)

  !
  ! Register callback function to display graphics.
  ! Enter main loop and process events.
  !
  call glutdisplayfunc(display)

  !
  ! intel ifort compiler seg. faults on this ...
  !

    cstr = glGetString(GL_VENDOR)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=min(len(str),index(str,char(0)))
    write(*,*) 'OpenGL vendor is ',str(1:l)

    cstr=glGetString(GL_RENDERER)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=min(len(str),index(str,char(0)))
    write(*,*) 'OpenGL renderer is ',str(1:l)

    cstr=glGetString(GL_VERSION)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=min(len(str),index(str,char(0)))
    write(*,*) 'OpenGL version is ',str(1:l)

    cstr=glGetString(GL_EXTENSIONS)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=min(len(str),index(str,char(0)))
    write(*,*) 'OpenGL extensions are: ',str(1:l)

    cstr=gluGetString(GLU_VERSION)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=min(len(str),index(str,char(0)))
    write(*,*) 'GLU version is ',str(1:l)

    cstr=gluGetString(GLU_EXTENSIONS)
    call c_f_pointer(cptr=cstr, fptr=str)
    l=index(str,char(0))
    write(*,*) 'GLU extensions are: ',str(1:l)

    call glGetIntegerv(GL_MAX_NAME_STACK_DEPTH,glgeti)
    write(*,*) 'Name stack=',glgeti(1)
 
  call glutmainloop()

end program





