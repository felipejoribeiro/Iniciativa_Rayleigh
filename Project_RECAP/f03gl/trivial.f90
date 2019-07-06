!
! This is a simple, introductory OpenGL program.  It originally comes from
! page 6 of the "red book".  This is a Fortran translation and slight
! modification of the program hello.c from the GLUT distribution.
!
module trivial

  implicit none

  public :: display

  contains

  subroutine display() bind(C)
    
    use opengl_gl
    use opengl_glu
    use opengl_glut

    ! clear all pixels
    call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)

    ! draw white polygon (rectangle) with corners at
    ! ((0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
    call glcolor3f(1.0_glfloat, 1.0_glfloat, 1.0_glfloat)
    call glbegin(GL_POLYGON)
    call glvertex3f(0.25_glfloat, 0.25_glfloat, 0.0_glfloat)
    call glvertex3f(0.75_glfloat, 0.25_glfloat, 0.0_glfloat)
    call glvertex3f(0.75_glfloat, 0.75_glfloat, 0.0_glfloat)
    call glvertex3f(0.25_glfloat, 0.75_glfloat, 0.0_glfloat)
    call glend()

    ! don't wait!
    ! start processing buffered OpenGL routines
    call glflush()

  end subroutine display

end module trivial
program main

  use opengl_gl
  use opengl_glu
  use opengl_glut

  use trivial

  implicit none

  real(kind=glfloat),  parameter :: zero  = 0.0_glfloat
  real(kind=gldouble), parameter :: dzero = 0.0_gldouble, &
                                    one   = 1.0_gldouble
  integer(kind=glint) :: iwin

  ! Declare initial window size, position, and display mode
  ! (single buffer and RGBA).  Open window with "trivial"
  ! in its title bar.

  call glutinit()
  call glutinitdisplaymode(GLUT_SINGLE+GLUT_RGB+GLUT_DEPTH)
  iwin = glutcreatewindow("trivial"//char(0))

  ! select clearing color
  call glclearcolor(zero, zero, zero, zero)

  ! initialize viewing values
  call glmatrixmode(GL_PROJECTION)
  call glloadidentity()
  call glortho(dzero, one, dzero, one, -one, one)

  ! Register callback function to display graphics.
  ! Enter main loop and process events.
  call glutdisplayfunc(display)
  call glutmainloop()

end program main

