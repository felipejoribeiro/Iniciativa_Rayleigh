!
!  GLUT Fortran 90 program to draw red light sphere.
!
program main

  use opengl_gl
  use opengl_glu
  use opengl_glut
  
  interface
    subroutine display() bind(C)
    end subroutine display
  end interface

  real(glfloat) :: diffuse(4) = (/1.0, 0.0, 0.0, 1.0/), &
                      pos(4)  = (/1.0, 1.0, 1.0, 0.0/)

  call glutinit()
  call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)
  i = glutcreatewindow('Fortran GLUT torus'//char(0))

  call glnewlist(1, GL_COMPILE)
  call glutsolidtorus(0.275_gldouble, 0.85_gldouble, 20, 35)
  call glendlist

  call gllightfv(GL_LIGHT0, GL_DIFFUSE, diffuse)
  call gllightfv(GL_LIGHT0, GL_POSITION, pos)
  call glenable(GL_LIGHTING)
  call glenable(GL_LIGHT0)
  call glenable(GL_DEPTH_TEST)
  
  call glmatrixmode(GL_PROJECTION)
  call gluperspective(40.0_gldouble, 1.0_gldouble, &
                       1.0_gldouble, 10.0_gldouble)
  call glmatrixmode(GL_MODELVIEW)
  call glulookat( 0.0_gldouble, 0.0_gldouble, 5.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                  0.0_gldouble, 1.0_gldouble, 1.0_gldouble)
  call glutdisplayfunc(display)
  call glutidlefunc(display)
  call glutmainloop()

end
subroutine display() bind(C)

  use opengl_gl
  use opengl_glut

  call glclear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
  call glcalllist(1)
  call gltranslatef(0.0, 0.01, -0.001)
  call glrotatef( 1.0, 1.0, 0.0, 0.0)
  call glflush()
  call glutswapbuffers()
  call system('sleep 0.01')
  
end
