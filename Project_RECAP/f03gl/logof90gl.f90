module logof90gl

  use opengl_gl
  use opengl_glu
  use opengl_glut

  implicit none

  type coord
     real(glfloat) :: x,y
  end type coord

  type(coord) :: angle = coord(0.,0.) ! in degrees
  type(coord) :: begin
  logical :: moving
  logical :: newModel = .true.
  logical :: show_colorbars = .false.
  real(glfloat) :: lightZeroPosition(4) = (/10.0, 4.0, 10.0, 1.0/), &
          lightZeroColor(4) = (/1.0, 1.0, 1.0, 1.0/), &
          lightOnePosition(4) = (/-1.0, -2.0, 1.0, 0.0/), &
          lightOneColor(4) = (/0.5, 0.5, 0.5, 1.0/), &
          logoColor(4) = (/1.0, 0.72, 0.0, 1.0/)

  logical :: lightZeroSwitch = .true., lightOneSwitch = .false.
  integer(glint), parameter :: thelist = 20
  real (glfloat), parameter :: PI = 3.1415926
  integer(glint) :: window_width, window_height

  contains

  subroutine make_letter(centers,radii)
    type (coord), intent(in) :: centers(:)
    real (glfloat), intent(in) :: radii(:)
    integer, parameter :: npoints_around_circle = 10
    integer :: npoints
    real (glfloat) :: x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,alpha
    type (coord) :: center1, center2
    real (glfloat) :: orient(size(radii)), radius1, radius2, orient1, orient2
    integer :: point, i

    npoints = size(radii)

    do point=1,npoints
      center1 = centers(max(1,point-1))
      center2 = centers(min(npoints,point+1))
      if( center1%x==center2%x .and. center1%y==center2%y )then
        orient(point) = 0.
      else
        orient(point) = atan2(center2%y-center1%y,center1%x-center2%x)
      endif
    end do

    call glbegin(GL_QUADS)

    do point=1,npoints-1
      center1 = centers(point)
      center2 = centers(point+1)
      radius1 = radii(point)
      radius2 = radii(point+1)
      orient1 = orient(point)
      orient2 = orient(point+1)
      x3 = radius2*sin(orient2)+center2%x
      y3 = radius2*cos(orient2)+center2%y
      z3 = 0._glfloat
      x4 = radius1*sin(orient1)+center1%x
      y4 = radius1*cos(orient1)+center1%y
      z4 = 0._glfloat
      do i=1,npoints_around_circle
        alpha = 2._glfloat * PI * i / real(npoints_around_circle,glfloat)
        x1 = x4; y1 = y4; z1 = z4
        x2 = x3; y2 = y3; z2 = z3
        x3 = radius2*sin(orient2)*cos(alpha)+center2%x
        y3 = radius2*cos(orient2)*cos(alpha)+center2%y
        z3 = radius2*sin(alpha)
        x4 = radius1*sin(orient1)*cos(alpha)+center1%x
        y4 = radius1*cos(orient1)*cos(alpha)+center1%y
        z4 = radius1*sin(alpha)
        call glnormal3f(x1-center1%x,y1-center1%y,z1)
        call glvertex3f(x1,y1,z1)
        call glnormal3f(x2-center2%x,y2-center2%y,z2)
        call glvertex3f(x2,y2,z2)
        call glnormal3f(x3-center2%x,y3-center2%y,z3)
        call glvertex3f(x3,y3,z3)
        call glnormal3f(x4-center1%x,y4-center1%y,z4)
        call glvertex3f(x4,y4,z4)
      end do
    end do

    call glend()

  end subroutine make_letter

  subroutine recalcModelView()

    call glpopmatrix()
    call glpushmatrix()
    call gltranslatef(2.5_glfloat,0._glfloat,0._glfloat)
    call glrotatef(angle%x, 0.0, 0.0, 1.0)
    call glrotatef(angle%y, cos(PI*angle%x/180.), -sin(PI*angle%x/180.),0.0)
    call gltranslatef(-2.5_glfloat,0._glfloat,0._glfloat)
    newModel = .false.

  end subroutine recalcModelView

end module logof90gl
subroutine number(n,x,y)

  use opengl_gl
  use opengl_glu
  use opengl_glut

  integer, intent(in) :: n
  real(gldouble), intent(in) :: x,y

  character(len=4) chn
  integer i

  write(chn,'(i4)') n
  call glmatrixmode(GL_MODELVIEW)
  call glpushmatrix()
  call gltranslated(x,y,0.0_gldouble)
  do i=1,4
    call glutbitmapcharacter(GLUT_BITMAP_8_BY_13,ichar(chn(i:i)))
  end do
  call glpopmatrix()

end subroutine number
subroutine redraw() bind(C)

  use logof90gl

  if( newModel )then
    call recalcModelView()
  endif
  call glclear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  call glcalllist(thelist)
  call glutswapbuffers()

end subroutine redraw
subroutine mouse(button, state, x, y) bind(C)
  
  use logof90gl
  
  implicit none
  integer(glcint), intent(in), value :: button, state, x, y
  real value,xfract,yfract

  if( button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN )then
    moving = .true.
    begin = coord(x,y)
  endif
  if( button == GLUT_LEFT_BUTTON .and. state == GLUT_UP )then
    moving = .false.
  endif
  if( button == GLUT_MIDDLE_BUTTON .and. state == GLUT_DOWN )then
    xfract = real(x)/real(window_width)
    yfract = real(y)/real(window_height)
    value = (xfract-.1)*2.
    if (value >= 0. .and. value <= 1.) then
       if (yfract > .05 .and. yfract < .10) then
          logocolor(1) = value
       elseif (yfract > .15 .and. yfract < .20) then
          logocolor(2) = value
       elseif (yfract > .25 .and. yfract < .30) then
          logocolor(3) = value
       endif
!       print *,logocolor
       call draw_logo
       call glutpostredisplay()
    endif
  endif
          
end subroutine mouse
subroutine motion(x, y) bind(C)

  use logof90gl
  
  integer(glcint), intent(in), value :: x, y

  if( moving )then
    angle%x = angle%x + (x - begin%x)
    angle%y = angle%y + (y - begin%y)
    begin = coord(x,y)
    newModel = .true.
    call glutPostRedisplay()
  endif

  return
  
end subroutine motion
subroutine reshape(w,h) bind(C)

  use logof90gl

  integer(glcint), intent(in), value :: w,h

  window_width  = w
  window_height = h
  call glviewport(0,0,w,h)

  return
  
end subroutine reshape
subroutine controlLights(value) bind(C)

  use logof90gl

  integer(glcint), intent(in), value :: value

  select case (value)
  case(1)
    lightZeroSwitch = .not. lightZeroSwitch
    if (lightZeroSwitch) then
      call glEnable(GL_LIGHT0)
    else
      call glDisable(GL_LIGHT0)
    endif
  case(2)
    lightOneSwitch = .not. lightOneSwitch
    if (lightOneSwitch) then
      call glEnable(GL_LIGHT1)
    else
      call glDisable(GL_LIGHT1)
    endif
  case(3)
    show_colorbars = .not. show_colorbars
  case(4)
    call animate
  end select
  
  call draw_logo
  call glutPostRedisplay()

  return

end subroutine controlLights
subroutine animate

  use logof90gl

  implicit none
  real(glfloat) transx,transy,transz,angx,angy
  integer nstep,i

  nstep = 19
  do i=0,nstep

    transx = -6.*(nstep-i)/float(nstep) + 2.5
    transy = -4.*(nstep-i)/float(nstep)
    transz = -160.*(nstep-i)/float(nstep)
    angx = -10. -80.*(nstep-i)/float(nstep)
    angy = -50. + 320.*(nstep-i)/float(nstep)
    call glPopMatrix()
    call glPushMatrix()
    call gltranslatef(transx,transy,transz)
    call glRotatef(angx, 0.0, 0.0, 1.0)
    call glRotatef(angy, cos(pi*angx/180.), -sin(pi*angx/180.),0.0)
    call gltranslatef(-transx,0.,0.)
    call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
    call glcalllist(thelist)
    call glutSwapBuffers()

    !call sleep(1)
    call system('sleep 0.05')
    
  end do

  return

end subroutine animate
program main

  use logof90gl
  
  implicit none

  interface
    
    subroutine redraw() bind(C)
    end subroutine redraw

    subroutine mouse(button, state, x, y) bind(C)
    use logof90gl
    integer(glcint), intent(in), value :: button, state, x, y
    end subroutine mouse
  
    subroutine motion(x, y) bind(C)
    use logof90gl
    integer(glcint), intent(in), value :: x, y
    end subroutine motion

    subroutine controlLights(value) bind(C)
    use logof90gl
    integer(glcint), intent(in), value :: value
    end subroutine controlLights

    subroutine reshape(w,h) bind(C)
    use logof90gl
    integer(glcint), intent(in), value :: w,h
    end subroutine reshape
  
  end interface

  integer(GLint) :: i

  call glutInit()
  call glutInitDisplayMode(ior(ior(GLUT_RGB,GLUT_DOUBLE),GLUT_DEPTH))
  i = glutCreateWindow('f90gl logo'//char(0))
  call glutreshapewindow(320,320)
  window_width = 320; window_height = 320
  call glutDisplayFunc(redraw)
  call glutMouseFunc(mouse)
  call glutMotionFunc(motion)
  call glutreshapefunc(reshape)

  i = glutCreateMenu(controlLights)
  call glutAddMenuEntry('Toggle right light'//char(0), 1)
  call glutAddMenuEntry('Toggle left light'//char(0), 2)
  call glutAddMenuEntry('Toggle color bars'//char(0), 3)
  call glutAddMenuEntry('Animate'//char(0),4)
  call glutAttachMenu(GLUT_RIGHT_BUTTON)
  
  call glEnable(GL_CULL_FACE)
  call glEnable(GL_DEPTH_TEST)
  call glEnable(GL_LIGHTING)
  call glenable(gl_normalize)
  call glMatrixMode(GL_PROJECTION)
  call gluPerspective(10.0_gldouble,    & ! field of view in degree
                          1.0_gldouble, & ! aspect ratio
                          1.0_gldouble, & ! Z near
                        400.0_gldouble)   ! Z far
  call glMatrixMode(GL_MODELVIEW)
  call gluLookAt( &
   2.0_gldouble, 0.0_gldouble, 40.0_gldouble, & ! eye is at (0,0,30)
   2.0_gldouble, 0.0_gldouble, 0.0_gldouble,  & ! center is at (0,0,0)
   0.0_gldouble, 1.0_gldouble, 0._gldouble)     ! up is in postivie Y direction
  call glPushMatrix()  ! dummy push so we can pop on model recalc 
  call draw_logo

! This call does nothing; it is just to test GLUTNULLFUNC with arguments
! call glutDialsFunc(GLUTNULLFUNC)

  call glutMainLoop()

end program main
subroutine draw_logo

  use logof90gl
  
  type(coord), allocatable   :: centers(:)
  real(glfloat), allocatable :: radii(:)
  real(glfloat) :: x, alpha
  integer int_color

  x = 247./255.

  call glclearcolor(x,x,x,1.0)
  call gldeletelists(thelist,1_glsizei)
  call glnewlist(thelist,gl_compile)
  !
  ! color bars
  !
  if( show_colorbars )then
    call gldisable(GL_LIGHTING)
    call glMatrixMode(GL_MODELVIEW)
    call glpushmatrix()
    call glloadidentity()
    call glMatrixMode(GL_PROJECTION)
    call glpushmatrix()
    call glloadidentity()
    
    call glortho(0._gldouble,1._gldouble,0._gldouble,1._gldouble,-1._gldouble,1._gldouble)
    call glcolor3f(1._glfloat,0._glfloat,0._glfloat)
    call glrectf(.1_glfloat,.9_glfloat,.1_glfloat+.5_glfloat*logocolor(1),.95_glfloat)
    call glcolor3f(0._glfloat,1._glfloat,0._glfloat)
    call glrectf(.1_glfloat,.8_glfloat,.1_glfloat+.5_glfloat*logocolor(2),.85_glfloat)
    call glcolor3f(0._glfloat,0._glfloat,1._glfloat)
    call glrectf(.1_glfloat,.7_glfloat,.1_glfloat+.5_glfloat*logocolor(3),.75_glfloat)
    call glcolor3f(1._glfloat,1._glfloat,1._glfloat)
    call glrectf(.1_glfloat,.9_glfloat,.6_glfloat,.95_glfloat)
    call glrectf(.1_glfloat,.8_glfloat,.6_glfloat,.85_glfloat)
    call glrectf(.1_glfloat,.7_glfloat,.6_glfloat,.75_glfloat)
!    int_color = int(logocolor(1)*256)
!    call number(int_color,.7_gldouble,.9_gldouble)
!    int_color = int(logocolor(2)*256)
!    call number(int_color,.7_gldouble,.8_gldouble)
!    int_color = int(logocolor(3)*256)
!    call number(int_color,.7_gldouble,.7_gldouble)
    call glMatrixMode(GL_PROJECTION)
    call glpopmatrix()
    call glMatrixMode(GL_MODELVIEW)
    call glpopmatrix()
  endif

  call gllightmodelfv(gl_light_model_ambient, (/.25,.25,.25,1./))
  call glLightfv(GL_LIGHT0, GL_POSITION, lightZeroPosition)
  call glLightfv(GL_LIGHT0, GL_DIFFUSE, lightZeroColor)
  call glLightfv(GL_LIGHT0, GL_SPECULAR, lightZeroColor)
  call glLightfv(GL_LIGHT1, GL_POSITION, lightOnePosition)
  call glLightfv(GL_LIGHT1, GL_DIFFUSE, lightOneColor)
  if( lightZeroSwitch )then
    call glEnable(GL_LIGHT0)
  else
    call glDisable(GL_LIGHT0)
  endif
  if( lightOneSwitch )then
    call glEnable(GL_LIGHT1)
  else
    call glDisable(GL_LIGHT1)
  endif
  call glenable(gl_lighting)
  call glpolygonmode(gl_front_and_back, gl_fill)
  !
  ! curve of f
  !
  allocate(centers(41),radii(41))
  call glpushmatrix()
  call glrotatef(25._glfloat,0._glfloat,0._glfloat,1._glfloat)
  do i=1,41
    x = (i-21)/20._glfloat
    centers(i)%x = x
    centers(i)%y = -x*(x-1.3)*(x+1.3)
    if( x <= -0.5 )then
      radii(i) = .2*x+.2
    elseif( x >= 0.5 )then
      radii(i) = .2-.2*x
    else
      radii(i) = .1_glfloat
    endif
  end do
  call glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, logoColor)
  call glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, logoColor)
  call glMaterialf(gl_front_and_back, gl_shininess, 128._glfloat)
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)
  !
  ! crossbar of f
  !
  allocate(centers(4),radii(4))
  centers(1) = coord(-.4,0.)
  centers(2) = coord(-.4,0.)
  centers(3) = coord(.4,0.)
  centers(4) = coord(.4,0.)
  radii(1) = 0.
  radii(2) = .1
  radii(3) = .1
  radii(4) = 0.
  call make_letter(centers,radii)
  deallocate(centers,radii)
  !
  ! left part of 9
  !
  allocate(centers(28),radii(28))
  call glpushmatrix()
  call gltranslatef(1.0_glfloat,0._glfloat,0._glfloat)
  call glrotatef(10._glfloat,0._glfloat,0._glfloat,-1._glfloat)
  do i=1,28
    alpha = (i-6)*pi/20. + pi/2. + pi/10.
    centers(i)%x = .4*cos(alpha)
    centers(i)%y = .4*sin(alpha) + .4
    radii(i) = .1
    if( i > 20 )then
      radii(i) = (28-i)/80.
    endif
  end do
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)
  !
  ! right part of 9
  !
  allocate(centers(25),radii(25))
  call glpushmatrix()
  call gltranslatef(1.0_glfloat,0._glfloat,0._glfloat)
  call glrotatef(10._glfloat,0._glfloat,0._glfloat,-1._glfloat)
  do i=1,25
    alpha = (i-1)*pi/20. - pi/2. - 4.*pi/20.
    centers(i)%x = .5*cos(alpha)
    centers(i)%y = .8*sin(alpha)
    radii(i) = .1
    if( i < 5 )then
      radii(i) = i/50.
    endif
  end do
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)
  !
  ! 0
  !
  allocate(centers(42),radii(42))
  call glpushmatrix()
  call gltranslatef(2.4_glfloat,0._glfloat,0._glfloat)
  do i=1,42
    alpha = (i-1)*pi/20.
    centers(i)%x = .6*cos(alpha)
    centers(i)%y = .8*sin(alpha)
    radii(i) = .1
  end do
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)
  !
  ! right part of g
  !
  allocate(centers(15+56),radii(15+56))
  call glpushmatrix()
  call gltranslatef(3.6_glfloat,-.5_glfloat,0._glfloat)
 !call glrotatef(10._glfloat,0._glfloat,0._glfloat,-1._glfloat)
  do i=1,15
    alpha = (i-1)*pi/20. - pi/2. - 4.*pi/20.
    centers(i)%x = .5*cos(alpha) - .1
    centers(i)%y =1.2*sin(alpha)
    radii(i) = .1
    if( i < 5 )then
      radii(i) = i/50.
    endif
  end do
  !
  ! left part of g
  !
 !call glrotatef(20._glfloat,0._glfloat,0._glfloat,-1._glfloat)
  do i=1+15,55+15
    alpha = 3.*(i-1-15)*pi/80. 
    centers(i)%x = .4*cos(alpha)
    centers(i)%y = .4*sin(alpha)
    radii(i) = .1
  enddo
  centers(56+15) = centers(55+15)
  radii(56+15) = 0.
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)
  !
  ! l
  !
  allocate(centers(41),radii(41))
  call glpushmatrix()
  call gltranslatef(4.6_glfloat,0._glfloat,0._glfloat)
  call glrotatef(25._glfloat,0._glfloat,0._glfloat,1._glfloat)
  do i=1,41
    x = (i-21)/20._glfloat
    centers(i)%x = x
    centers(i)%y = -x*(x-1.4)*(x+1.4)
    if( x <= -0.5 )then
      radii(i) = .2*x+.2
    elseif( x >= 0.5 )then
      radii(i) = .2-.2*x
    else
      radii(i) = .1_glfloat
    endif
  enddo
  
  call make_letter(centers,radii)
  call glpopmatrix()
  deallocate(centers,radii)

  call glendlist()

end subroutine draw_logo

