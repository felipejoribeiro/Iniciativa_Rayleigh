program fgl10

  use opengl_gl
  use opengl_glu
  use opengl_glut

  interface
    subroutine display() bind(C)
    end subroutine display

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  end interface

  integer(kind=GLint) :: winWidth = 400, winHeight = 150
  integer(kind=GLint) :: iwin

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl10 Example 2-5"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)

  call glShadeModel(GL_FLAT)

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )

  call glutMainLoop()

end program
subroutine display() bind(C)

  use opengl_gl

  integer(GLushort) :: iz
  real(GLfloat)     :: x, y
  
  call glClear(GL_COLOR_BUFFER_BIT)
  call glColor3f(1.0,1.0,1.0)
  
  call glEnable(GL_LINE_STIPPLE)

  !
  ! 1st row
  !
  
  iz = z'0101'                           ! let iz do the casting
  call glLineStipple(1,iz)
  call drawline( 50.,125.,150.,125.)     ! not clean; glfloat == float ...

  iz = z'00FF'
  call glLineStipple(1,iz)               ! dashed in the call
  call drawline(150.,125.,250.,125.)

  iz = z'1C47'
  call glLineStipple(1,iz)               ! dash dot dash
  call drawline(250.,125.,350.,125.)

  !
  ! 2nd row
  !
  call glLineWidth(5.0)                          

  iz = z'0101'
  call glLineStipple(1,iz)               ! dotted
  call drawline( 50.,100.,150.,100.)

  iz = z'00FF'
  call glLineStipple(1,iz)               ! dashed in the call
  call drawline(150.,100.,250.,100.)

  iz = z'1C47'
  call glLineStipple(1,iz)               ! dash dot dash
  call drawline(250.,100.,350.,100.)

  call glLineWidth(1.0)                  ! back to default                      

  !
  ! 3rd row
  !
  iz = z'1C47'
  call glLineStipple(1,iz)  

  call glBegin(GL_Line_Strip)
  do i=0,6
    x = 50.0+float(i)*50.0
    call glVertex2f( x , 75.0_glfloat )  
  end do
  call glend()

  !
  ! 4th row
  !
  iz = z'1C47'
  call glLineStipple(1,iz)  

  call glBegin(GL_Line_Strip)
  do i=0,5
    x = 50.0+float(i)*50.0
    y = 50.0+float(i+1)*50.0
    call drawline(x,50.0_glfloat,y,50.0_glfloat) 
  end do
  call glend()

  !
  ! 5th row
  !
  iz = z'1C47'
  call glLineStipple(5,iz)  
  call drawline( 50.,25.,350.,25.)

  call glDisable(GL_LINE_STIPPLE)
  call glFlush()

end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use opengl_gl
  use opengl_glu

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, Width, Height

  Zero   = 0.0
  Width  = newWidth
  Height = newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call gluOrtho2D( Zero, Width, Zero, Height )

end subroutine reshape
subroutine drawline( x1,y1, x2,y2 )

  use opengl_gl

  real(GLfloat) :: x1,y1, x2,y2

  call glBegin(GL_LINES)
    call glVertex2f(x1,y1)
    call glVertex2f(x2,y2)
  call glEnd()

end subroutine drawline

