module checker 

  use opengl_gl
  use opengl_glu
  use opengl_glut

  implicit none

  integer, parameter :: checkImageWidth = 64
  integer, parameter :: checkImageHeight = 64
  integer(GLubyte), target :: checkImage(0:2,0:checkImageWidth-1,0:checkImageHeight-1)

  contains

  subroutine makeCheckImage()
    integer i, j, c
    
    do i = 0, checkImageWidth-1
      do j = 0, checkImageHeight-1
        if (iand(i,8)==0 .neqv. iand(j,8)==0) then
          c = ibset(127,7)
        else
          c = 0
        endif
        checkImage(0,i,j) = c
        checkImage(1,i,j) = c
        checkImage(2,i,j) = c
     end do
   end do
  end subroutine makeCheckImage

  subroutine myinit()
    call glClearColor (0.0, 0.0, 0.0, 0.0)
    call glEnable(GL_DEPTH_TEST)
    call glDepthFunc(GL_LESS)

    call makeCheckImage()
    call glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
    call glTexImage2D(GL_TEXTURE_2D, 0, 3, checkImageWidth, &
       checkImageHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, c_loc(checkImage) )
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)
    call glEnable(GL_TEXTURE_2D)
    call glShadeModel(GL_FLAT)
  end subroutine myinit

  subroutine display() bind(C)
    call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
    call glBegin(GL_QUADS)
    call glTexCoord2f(0.0, 0.0); call glVertex3f(-2.0, -1.0, 0.0)
    call glTexCoord2f(0.0, 1.0); call glVertex3f(-2.0, 1.0, 0.0)
    call glTexCoord2f(1.0, 1.0); call glVertex3f(0.0, 1.0, 0.0)
    call glTexCoord2f(1.0, 0.0); call glVertex3f(0.0, -1.0, 0.0)

    call glTexCoord2f(0.0, 0.0); call glVertex3f(1.0, -1.0, 0.0)
    call glTexCoord2f(0.0, 1.0); call glVertex3f(1.0, 1.0, 0.0)
    call glTexCoord2f(1.0, 1.0); call glVertex3f(2.41421, 1.0, -1.41421)
    call glTexCoord2f(1.0, 0.0); call glVertex3f(2.41421, -1.0, -1.41421)
    call glEnd()
    call glutSwapBuffers()
  end subroutine display

  subroutine myReshape( w, h ) bind(C)
    integer(GLint), intent(in), value :: w,h
    call glViewport(0, 0, w, h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call gluPerspective(60.0_gldouble, (1.0_gldouble*w)/h, 1.0_gldouble, 30.0_gldouble)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()
    call glTranslatef(0.0, 0.0, -3.6)
  end subroutine myReshape

end module checker

program main
  use checker
  implicit none
  integer w

  call glutInit()
  call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
  w = glutCreateWindow("checker"//char(0))
  
  call myinit()
  call glutReshapeFunc(myReshape)
  call glutDisplayFunc(display)
  call glutMainLoop()

end program main

