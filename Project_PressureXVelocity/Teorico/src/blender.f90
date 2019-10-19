!  blender renders two spinning icosahedrons (red and green).
!  The blending factors for the two icosahedrons vary sinusoidally
!  and slightly out of phase.  blender also renders two lines of
!  text in a stroke font: one line antialiased, the other not.

module blender

! with use here, we don't need them in each routine
use opengl_gl
use opengl_glu
use opengl_glut

contains

subroutine idle1() bind(C)




  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call glPushAttrib(GL_ENABLE_BIT)
  call glcolor3f(0.1, 1.0 , 1.0)
  call glMatrixMode(GL_PROJECTION)
  call glLineWidth(2.0)
  call glLoadIdentity()
  call gluOrtho2D(0.0_gldouble, 1500.0_gldouble, 0.0_gldouble, 1500.0_gldouble * 300/900)
  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()
!  Rotate text slightly to help show jaggies
  call output(0.0, 0.0, "This is aátialíased.")
  call glPopMatrix()
  call glMatrixMode(GL_PROJECTION)
  call glPopMatrix()
  call glPopAttrib()



  call glloadidentity()                                                  !Identity matrix loaded
  call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !Limits (left, right, down, up, close to the camera, far from the camera)
  call glEnableClientState(GL_VERTEX_ARRAY)                              !Funcionalities


  call glutSwapBuffers()






end subroutine idle1



subroutine output(x, y, text)

real(glfloat) x,y
character(len=*) text
integer(glcint) p

  call glPushMatrix()
  call glTranslatef(x, y, 0.0_glfloat)
  do i=1,len(text)
    p = ichar(text(i:i))
    call glutStrokeCharacter(GLUT_STROKE_ROMAN, p)
  end do
  call glPopMatrix()
end subroutine output




end module blender





program main
use opengl_gl
use opengl_glu
use opengl_glut
use blender



  call glutinit()
    call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)              !Exibicion mode
    call glutInitWindowSize(900,300)                                !Determine window width in pixels
    window = glutcreatewindow("ola")                          !A name is given to the window

    call glutIdleFunc(idle1)

  call glclearcolor(1.0, 1.0, 1.0, 0.0)                                  !seta cor no background.
    call glmatrixmode(GL_PROJECTION)                                       !Matrix context of operations
    call glloadidentity()                                                  !Identity matrix loaded
    call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !Limits (left, right, down, up, close to the camera, far from the camera)
    call glEnableClientState(GL_VERTEX_ARRAY)                              !Funcionalities
    call glEnable(GL_BLEND)                                                !Blend
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)                 !Blend function
    call glEnable(GL_LINE_SMOOTH)                                          !Smooth edges



  call glutMainLoop()

end program main
