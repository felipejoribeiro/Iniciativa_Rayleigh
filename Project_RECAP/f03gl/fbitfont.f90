subroutine output(x,y,s)

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
     call glutbitmapcharacter(GLUT_BITMAP_TIMES_ROMAN_24, &
          ichar(c))
  enddo

end subroutine output
subroutine display() bind(c)

  use opengl_gl

  call glclear(GL_COLOR_BUFFER_BIT)
  call output(0.0,24.0,'This is written in a GLUT bitmap font.')
  call output(100.0,100.0,'ABCDEFGabcdefg')
  call output(50.0,145.0,'(positioned in pixels with upper-left origin)')
  call glflush()
  
end subroutine display
subroutine reshape(w,h) bind(c)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  integer(glcint), intent(in), value :: w, h

  call glviewport(0, 0, w, h)
  call glmatrixmode(GL_PROJECTION)
  call glloadidentity()
  call gluortho2d(dble(0.0), dble(w), dble(0.0), dble(h))
  call glscalef(1.0, -1.0, 1.0)
  call gltranslatef(real(0.0), real(-h), real(0.0))
  call glmatrixmode(GL_MODELVIEW)
  
end subroutine reshape
program main

  use opengl_gl
  use opengl_glu
  use opengl_glut
  implicit none

  integer :: win

  interface
    subroutine display() bind(c)
    end subroutine display
    subroutine reshape(w,h) bind(c)
    use opengl_gl
    integer(glcint), intent(in), value :: w, h
    end subroutine reshape
  end interface
  
  call glutinitdisplaymode(GLUT_RGB + GLUT_SINGLE)
  call glutinitwindowsize(500, 150)
  call glutinit()
  win = glutcreatewindow('Fortran GLUT bitmap A')
  call glclearcolor(0.0, 0.0, 0.0, 1.0)
  call glutdisplayfunc(display)
  call glutreshapefunc(reshape)
  call glutmainloop()
  
end program main

