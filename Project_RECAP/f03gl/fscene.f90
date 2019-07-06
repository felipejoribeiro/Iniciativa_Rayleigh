!  GLUT Fortran program to render simple red scene.
!
! This is a fortran 90 program in fixed source form.
! In a risky move, this program assumes that the default kind of
! integer is the same as glint and glsizei and also that the
! default real is the same kind as glfloat
!
module fscene 

  use opengl_gl
  use opengl_glu
  use opengl_glut

  private

  public :: display, freshape, submenu, mainmenu, myinit

  contains

  subroutine display() bind(C)
    call glclear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
    call glpushmatrix()
    call glscalef(1.3, 1.3, 1.3)
    call glrotatef(20.0, 1.0, 0.0, 0.0)

    call glpushmatrix()
    call gltranslatef(-0.75, 0.5, 0.0)
    call glrotatef(90.0, 1.0, 0.0, 0.0)

    call glutsolidtorus(0.275_gldouble, 0.55_gldouble, 20, 15)
    call glpopmatrix()

    call glpushmatrix()
    call gltranslatef(-0.75, -0.5, 0.0)
    call glrotatef(270.0, 1.0, 0.0, 0.0)
    call glutsolidtetrahedron()
    call glpopmatrix()

    call glpushmatrix()
    call gltranslatef(0.75, 0.0, -1.0)
    call glutsolidicosahedron()
    call glpopmatrix()

    call glpopmatrix()
    call glFlush()
  end subroutine display

  subroutine freshape(w,h) bind(C)
    integer(glint), intent(in), value :: w,h
    real(kind=gldouble) :: wr,hr,d
    
    call glviewport(0, 0, w, h)
    call glmatrixmode(GL_PROJECTION)
    call glloadidentity()
    wr = w
    hr = h
    d = 1.0_gldouble
    if( w <= h )then
      call glortho( -2.5_gldouble, 2.5_gldouble, &
                    -2.5_gldouble * hr/wr, 2.5_gldouble * hr/wr, &
                   -10.0_gldouble, 10.0_gldouble)
    else
      call glortho( -2.5_gldouble * hr/wr, &
                     2.5_gldouble * hr/wr, &
                    -2.5_gldouble, 2.5_gldouble, -10.0_gldouble, &
                    10.0_gldouble)
    endif
    call glmatrixmode(GL_MODELVIEW)
  end subroutine freshape
  
  subroutine submenu(value) bind(C)
    integer(glint), intent(in), value :: value
    if( value == 1 )then
      call glenable(GL_DEPTH_TEST)
      call glenable(GL_LIGHTING)
      call gldisable(GL_BLEND)
      call glpolygonmode(GL_FRONT_AND_BACK, GL_FILL)
    elseif( value == 2 )then
      call gldisable(GL_DEPTH_TEST)
      call gldisable(GL_LIGHTING)
      call glcolor3f(0.0, 1.0, 1.0)
      call glpolygonmode(GL_FRONT_AND_BACK, GL_LINE)
      call glenable(GL_LINE_SMOOTH)
      call glenable(GL_BLEND)
      call glblendfunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    end if
    call glutpostredisplay()
   end subroutine submenu

  subroutine mainmenu(value) bind(C)
    integer(glint), intent(in), value :: value
    if(value == 666) stop
  end subroutine mainmenu

end module fscene
program main
  use fscene
  use opengl_gl
  use opengl_glu
  use opengl_glut
  
  implicit none
  integer(4) :: i,j
  real(kind=glfloat), dimension(4), save :: lambient = (/0.0, 0.0, 0.0, 1.0/)
  real(kind=glfloat), dimension(4), save :: ldiffuse = (/1.0, 0.0, 0.0, 1.0/)
  real(kind=glfloat), dimension(4), save :: lspecular = (/1.0, 1.0, 1.0, 1.0/)
  real(kind=glfloat), dimension(4), save :: lposition = (/1.0, 1.0, 1.0, 0.0/)

  call glutinit()
  call glutinitwindowposition(100,100)
  call glutinitwindowsize(500,500)
  call glutinitdisplaymode(GLUT_SINGLE+GLUT_RGB+GLUT_DEPTH)
  i = glutcreatewindow("Fortran GLUT program"//char(0))
  !----------------------------------------------------------
  call gllightfv(GL_LIGHT0, GL_AMBIENT, lambient)
  call gllightfv(GL_LIGHT0, GL_DIFFUSE, ldiffuse)
  call gllightfv(GL_LIGHT0, GL_SPECULAR, lspecular)
  call gllightfv(GL_LIGHT0, GL_POSITION, lposition)
  call glenable(GL_LIGHT0)
  call gldepthfunc(GL_LESS)
  call glenable(GL_DEPTH_TEST)
  call glenable(GL_LIGHTING)
  call glClearColor(0.6, 0.8, 0.8, 0.5)
  !----------------------------------------------------------
  call glutdisplayfunc(display)
  call glutreshapefunc(freshape)

  i = glutcreatemenu(submenu)
  call glutaddmenuentry("Filled"//char(0), 1)
  call glutaddmenuentry("Outline"//char(0), 2)

  j = glutcreatemenu(mainmenu)
  call glutaddsubmenu("Polygon mode"//char(0),1)
  call glutaddmenuentry("Quit"//char(0), 666)
  call glutattachmenu(GLUT_RIGHT_BUTTON)
  
  call glutmainloop()
  
end program main

