subroutine display() bind(C)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call glCallList(1)
  call glFlush()

  return

end subroutine display
program main

  use opengl_gl
  use opengl_glu
  use opengl_glut

  implicit none

  real(glfloat) :: &
    mat_red_diffuse(4) = (/ 0.7, 0.0, 0.1, 1.0 /), &
    mat_green_diffuse(4) = (/ 0.0, 0.7, 0.1, 1.0 /), &
    mat_blue_diffuse(4) = (/ 0.0, 0.1, 0.7, 1.0 /), &
    mat_yellow_diffuse(4) = (/ 0.7, 0.8, 0.1, 1.0 /), &
    mat_specular(4) = (/ 1.0, 1.0, 1.0, 1.0 /), &
    mat_shininess(1) = (/ 100.0 /), &
    knots(8) = (/ 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0 /), &
    pts1(4,4,3), pts2(4,4,3), &
    pts3(4,4,3), pts4(4,4,3), &
    pts11d(48), pts21d(48), pts31d(48), pts41d(48)

! integer(4) :: nurb
  type(c_ptr) :: nurb
  
  integer u, v, wind
  
  interface
    subroutine display() bind(C)
    end subroutine display
  end interface
  
  real(glfloat) real_glu_fill

  call glutInit()
  wind = glutCreateWindow("molehill"//char(0))
  
  call glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular)
  call glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess)
  call glEnable(GL_LIGHTING)
  call glEnable(GL_LIGHT0)
  call glEnable(GL_DEPTH_TEST)
  call glEnable(GL_AUTO_NORMAL)
  call glEnable(GL_NORMALIZE)
  nurb = gluNewNurbsRenderer()
  call gluNurbsProperty(nurb, GLU_SAMPLING_TOLERANCE, 25.0)
  real_glu_fill = GLU_FILL
  call gluNurbsProperty(nurb, GLU_DISPLAY_MODE, real_glu_fill)
  !
  ! Build control points for NURBS mole hills.
  !
  do u=1,4
    do v=1,4
      !
      ! red
      !
      pts1(u,v,1) = 2.0*(u-1)
      pts1(u,v,2) = 2.0*(v-1)
      if((u==2 .or. u == 3) .and. (v == 2 .or. v == 3)) then
        ! Stretch up middle.
        pts1(u,v,3) = 6.0
      else
        pts1(u,v,3) = 0.0
      endif
      !
      ! green
      !
      pts2(u,v,1) = 2.0*(u - 4)
      pts2(u,v,2) = 2.0*(v - 4)
      if((u==2 .or. u == 3) .and. (v == 2 .or. v == 3)) then
        if(u == 2 .and. v == 2) then
          ! Pull hard on single middle square.
          pts2(u,v,3) = 15.0
        else
          ! Push down on other middle squares.
          pts2(u,v,3) = -2.0
        endif
      else
        pts2(u,v,3) = 0.0
      endif
      !
      ! blue
      !
      pts3(u,v,1) = 2.0*(u - 4)
      pts3(u,v,2) = 2.0*(v-1)
      if((u==2 .or. u == 3) .and. (v == 2 .or. v == 3)) then
        if(u == 2 .and. v == 3) then
          ! Pull up on single middple square.
          pts3(u,v,3) = 11.0
        else
          ! Pull up slightly on other middle squares.
          pts3(u,v,3) = 2.0
        endif
      else
        pts3(u,v,3) = 0.0
      endif
      !
      ! yellow
      !
      pts4(u,v,1) = 2.0*(u-1)
      pts4(u,v,2) = 2.0*(v - 4)
      if((u==2 .or. u == 3 .or. u == 4) .and. (v == 2 .or. v == 3)) then
        if(v == 2) then
          ! Push down front middle and right squares.
          pts4(u,v,3) = -2.0
        else
          ! Pull up back middle and right squares.
          pts4(u,v,3) = 5.0
        endif
      else
        pts4(u,v,3) = 0.0
      endif
    enddo
  enddo
  !
  ! Stretch up red's far right corner.
  !
  pts1(4,4,3) = 6
  !
  ! Pull down green's near left corner a little.
  !
  pts2(1,1,3) = -2
  !
  ! Turn up meeting of four corners.
  !
  pts1(1,1,3) = 1
  pts2(4,4,3) = 1
  pts3(4,1,3) = 1
  pts4(1,4,3) = 1
  !
  ! gluNurbsSurface expects an array of rank 1, and C will expect
  ! row-major order.  Reshape the pts arrays into 1D arrays in the
  ! opposite order
  !
  pts11d = reshape(reshape(pts1,(/3,4,4/),order=(/3,2,1/)),(/48/))
  pts21d = reshape(reshape(pts2,(/3,4,4/),order=(/3,2,1/)),(/48/))
  pts31d = reshape(reshape(pts3,(/3,4,4/),order=(/3,2,1/)),(/48/))
  pts41d = reshape(reshape(pts4,(/3,4,4/),order=(/3,2,1/)),(/48/))

  call glMatrixMode(GL_PROJECTION)
  call gluPerspective(55.0_gldouble, 1.0_gldouble, 2.0_gldouble, 24.0_gldouble)
  call glMatrixMode(GL_MODELVIEW)
  call glTranslatef(0.0, 0.0, -15.0)
  call glRotatef(330.0, 1.0, 0.0, 0.0)

  call glNewList(1, GL_COMPILE)
    ! Render red hill.
    call glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_red_diffuse)
    call gluBeginSurface(nurb)
      call gluNurbsSurface(nurb, 8, knots, 8, knots, &
        4 * 3, 3, pts11d, &
        4, 4, GL_MAP2_VERTEX_3)
    call gluEndSurface(nurb)

    ! Render green hill.
    call glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_green_diffuse)
    call gluBeginSurface(nurb)
      call gluNurbsSurface(nurb, 8, knots, 8, knots, &
        4 * 3, 3, pts21d, &
        4, 4, GL_MAP2_VERTEX_3)
    call gluEndSurface(nurb)

    ! Render blue hill.
    call glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_blue_diffuse)
    call gluBeginSurface(nurb)
      call gluNurbsSurface(nurb, 8, knots, 8, knots, &
        4 * 3, 3, pts31d, &
        4, 4, GL_MAP2_VERTEX_3)
    call gluEndSurface(nurb)

    ! Render yellow hill.
    call glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_yellow_diffuse)
    call gluBeginSurface(nurb)
      call gluNurbsSurface(nurb, 8, knots, 8, knots, &
        4 * 3, 3, pts41d, &
        4, 4, GL_MAP2_VERTEX_3)
    call gluEndSurface(nurb)
  call glEndList()

  call glutDisplayFunc(display)
  call glutMainLoop()

end program
