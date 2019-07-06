!
! This is a variation in which glutriangulatorobj is 
! replaced by glutesselator
!
module gltdino2

  use opengl_gl
  use opengl_glu
  use opengl_glut

  implicit none

  private
  public  :: redraw, mouse, motion, controlLights, makeDinosaur
  private :: extrudeSolidFrompolygon, recalcmodelView

  integer, parameter, private :: &
    RESERVED   =  0, &
    BODY_SIDE  =  1, &
    BODY_EDGE  =  2, &
    BODY_WHOLE =  3, &
    ARM_SIDE   =  4, &
    ARM_EDGE   =  5, &
    ARM_WHOLE  =  6, &
    LEG_SIDE   =  7, &
    LEG_EDGE   =  8, &
    LEG_WHOLE  =  9, &
    EYE_SIDE   = 10, &
    EYE_EDGE   = 11, &
    EYE_WHOLE  = 12, &
    DINOSAUR   = 13

  type, private :: coord
    real(kind=glfloat) :: x,y
  end type coord

  type, private :: tessvertex
    real(kind=glfloat), dimension(2) :: val
  end type tessvertex

  type(coord), private, save :: angle = coord(-150.0,0.0) ! in degrees
  type(coord), private :: begin
  
  logical, private :: moving
  integer, private, save :: W = 300, H = 300
  real(kind=glfloat), private, save :: bodyWidth = 2.0
  logical, private, save :: newmodel = .true.
  real(kind=glfloat), public, dimension(4), save ::     &
          lightZeroPosition = (/10.0, 4.0, 10.0, 1.0/), &
          lightZeroColor = (/0.8, 1.0, 0.8, 1.0/),      & ! green-tinted
          lightOnePosition = (/-1.0, -2.0, 1.0, 0.0/),  &
          lightOneColor = (/0.6, 0.3, 0.2, 1.0/),       & ! red-tinted
          skinColor = (/0.1, 1.0, 0.1, 1.0/),           &
          eyeColor = (/1.0, 0.2, 0.2, 1.0/)

  logical, private, save :: lightZeroSwitch = .true., lightOneSwitch = .true.

  contains

  subroutine extrudeSolidFrompolygon(data, dataSize, &
                                     thickness, side, edge, whole)
    
    type(tessvertex), intent(in out), target, dimension(:) :: data
    integer, intent(in) :: dataSize
    real(kind=glfloat), intent(in) :: thickness
    integer(kind=gluint), intent(in) :: side, edge, whole

    !  integer(GLsizei) :: tobj
    TYPE(C_PTR) :: tobj  
    logical, save :: tobj_null = .true.

    real(kind=gldouble), dimension(3) :: vertex
    real(kind=glfloat) :: dx, dy, leng
    integer :: i, fcount

    fcount = dataSize

    if( tobj_null )then
      tobj = glunewtess()      ! create and initialize a GLU
                               ! polygon * * tesselation object
      call glutesscallback(tobj, GLU_TESS_BEGIN, c_funloc(glbegin))
      call glutesscallback(tobj, GLU_TESS_VERTEX, c_funloc(glvertex2fv))
      call glutesscallback(tobj, GLU_TESS_END, c_funloc(glend))
    endif
    
    call glnewlist(side, GL_COMPILE)
    call glshademodel(GL_SMOOTH)  ! smooth minimizes seeing tessellation
    call glubeginpolygon(tobj)
    do i=1,fcount
      vertex(1) = data(i)%val(1)
      vertex(2) = data(i)%val(2)
      vertex(3) = 0.0
     !call glutessvertex(tobj, vertex, data(i)%val)
     !call glutessvertex(tobj, vertex, c_loc(data(i)%val))
      call glutessvertex(tobj, vertex, c_loc(data(i)))
    end do
    
    call gluendpolygon(tobj)
    call glendlist()
    call glnewlist(edge, GL_COMPILE)
    call glshademodel(GL_FLAT)  ! flat shade keeps angular hands
                              ! from being * * "smoothed"
    call glbegin(GL_QUAD_STRIP)
    do i=0,fcount
      ! mod function handles closing the edge
      call glvertex3f(data(modulo(i,fcount)+1)%val(1), data(modulo(i,fcount)+1)%val(2), 0.0)
      call glvertex3f(data(modulo(i,fcount)+1)%val(1), data(modulo(i,fcount)+1)%val(2), thickness)
      ! Calculate a unit normal by dividing by Euclidean
      ! distance. We * could be lazy and use
      ! glenable(GL_NORMALIZE) so we could pass in * arbitrary
      ! normals for a very slight performance hit.
      dx = data(modulo((i + 1),fcount)+1)%val(2) - data(modulo(i,fcount)+1)%val(2)
      dy = data(modulo(i,fcount)+1)%val(1) - data(modulo((i + 1), fcount)+1)%val(1)
      leng = sqrt(dx * dx + dy * dy)
      call glnormal3f(dx / leng, dy / leng, 0.0)
    enddo
    
    call glend()
    call glendlist()
    call glnewlist(whole, GL_COMPILE)
    call glfrontface(GL_CW)
    call glcalllist(edge)
    call glnormal3f(0.0, 0.0, -1.0)  ! constant normal for side
    call glcalllist(side)
    call glpushmatrix()
    call gltranslatef(0.0, 0.0, thickness)
    call glfrontface(GL_CCW)
    call glnormal3f(0.0, 0.0, 1.0)  ! opposite normal for other side
    call glcalllist(side)
    call glpopmatrix()
    call glendlist()

  end subroutine extrudeSolidFrompolygon

  subroutine makeDinosaur()

    real(kind=glfloat), save :: bodyWidth = 3.0

    type(tessvertex), target, save, dimension(22) :: body = &
      (/ tessvertex((/ 0.0,  3.0/)), tessvertex((/ 1.0,  1.0/)), tessvertex((/ 5.0,  1.0/)), &
         tessvertex((/ 8.0,  4.0/)), tessvertex((/10.0,  4.0/)), tessvertex((/11.0,  5.0/)), &
         tessvertex((/11.0, 11.5/)), tessvertex((/13.0, 12.0/)), tessvertex((/13.0, 13.0/)), &
         tessvertex((/10.0, 13.5/)), tessvertex((/13.0, 14.0/)), tessvertex((/13.0, 15.0/)), &
         tessvertex((/11.0, 16.0/)), tessvertex((/ 8.0, 16.0/)), tessvertex((/ 7.0, 15.0/)), &
         tessvertex((/ 7.0, 13.0/)), tessvertex((/ 8.0, 12.0/)), tessvertex((/ 7.0, 11.0/)), &
         tessvertex((/ 6.0,  6.0/)), tessvertex((/ 4.0,  3.0/)), tessvertex((/ 3.0,  2.0/)), &
         tessvertex((/ 1.0,  2.0/)) /)

    type(tessvertex), target, save, dimension(16) :: arm = &
      (/ tessvertex((/ 8.0, 10.0/)), tessvertex((/ 9.0,  9.0/)), tessvertex((/10.0,  9.0/)), &
         tessvertex((/13.0,  8.0/)), tessvertex((/14.0,  9.0/)), tessvertex((/16.0,  9.0/)), &
         tessvertex((/15.0,  9.5/)), tessvertex((/16.0, 10.0/)), tessvertex((/15.0, 10.0/)), &
         tessvertex((/15.5, 11.0/)), tessvertex((/14.5, 10.0/)), tessvertex((/14.0, 11.0/)), &
         tessvertex((/14.0, 10.0/)), tessvertex((/13.0,  9.0/)), tessvertex((/11.0, 11.0/)), &
         tessvertex((/ 9.0, 11.0/)) /)

    type(tessvertex), target, save, dimension(14) :: leg = &
      (/ tessvertex((/ 8.0,  6.0/)), tessvertex((/ 8.0, 4.0/)), tessvertex((/ 9.0, 3.0/)), &
         tessvertex((/ 9.0,  2.0/)), tessvertex((/ 8.0, 1.0/)), tessvertex((/ 8.0, 0.5/)), &
         tessvertex((/ 9.0,  0.0/)), tessvertex((/12.0, 0.0/)), tessvertex((/10.0, 1.0/)), &
         tessvertex((/10.0,  2.0/)), tessvertex((/12.0, 4.0/)), tessvertex((/11.0, 6.0/)), &
         tessvertex((/10.0,  7.0/)), tessvertex((/ 9.0, 7.0/)) /)

    type(tessvertex), target, save, dimension(6) :: eye = &
      (/ tessvertex((/ 8.75,15.0/)),  tessvertex((/ 9.0, 14.7/)), &
         tessvertex((/ 9.6, 14.7/)),  tessvertex((/10.1, 15.0/)), &
         tessvertex((/ 9.6, 15.25/)), tessvertex((/ 9.0, 15.25/)) /)

    call extrudeSolidFrompolygon(body, size(body), bodyWidth, &
      BODY_SIDE, BODY_EDGE, BODY_WHOLE)
    call extrudeSolidFrompolygon(arm, size(arm), bodyWidth / 4, &
      ARM_SIDE, ARM_EDGE, ARM_WHOLE)
    call extrudeSolidFrompolygon(leg, size(leg), bodyWidth / 2, &
      LEG_SIDE, LEG_EDGE, LEG_WHOLE)
    call extrudeSolidFrompolygon(eye, size(eye), bodyWidth + 0.2, &
      EYE_SIDE, EYE_EDGE, EYE_WHOLE)
      
    call glnewlist(DINOSAUR, GL_COMPILE)
    call glmaterialfv(GL_FRONT, GL_DIFFUSE, skinColor)
    call glcalllist(BODY_WHOLE)
    call glpushmatrix()
    call gltranslatef(0.0, 0.0, bodyWidth)
    call glcalllist(ARM_WHOLE)
    call glcalllist(LEG_WHOLE)
    call gltranslatef(0.0, 0.0, -bodyWidth - bodyWidth / 4)
    call glcalllist(ARM_WHOLE)
    call gltranslatef(0.0, 0.0, -bodyWidth / 4)
    call glcalllist(LEG_WHOLE)
    call gltranslatef(0.0, 0.0, bodyWidth / 2 - 0.1)
    call glmaterialfv(GL_FRONT, GL_DIFFUSE, eyeColor)
    call glcalllist(EYE_WHOLE)
    call glpopmatrix()
    call glendlist()

  end subroutine makeDinosaur

  subroutine recalcmodelView()

    real, parameter :: pi = 3.1415926
    
    call glpopmatrix()
    call glpushmatrix()
    call glrotatef(angle%x, 0.0, 1.0, 0.0)
    call glrotatef(angle%y, cos(pi*angle%x/180.0), 0.0, sin(pi*angle%x/180.0))
    call gltranslatef(-8.0, -8.0, -bodyWidth / 2)
    newmodel = .false.

  end subroutine recalcmodelView

  subroutine redraw() bind(C)

    if( newmodel )then
      call recalcmodelView()
    end if
    
    call glclear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
    call glcalllist(DINOSAUR)
    call glutswapbuffers()

  end subroutine redraw

  subroutine mouse(button, state, x, y) bind(C)
  
    integer(kind=glcint), intent(in), value :: button, state, x, y

    if( button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN )then
      moving = .true.
      begin = coord(x,y)
    end if
    
    if( button == GLUT_LEFT_BUTTON .and. state == GLUT_UP )then
      moving = .false.
    end if

  end subroutine mouse

  subroutine motion(x, y) bind(C)

    integer(kind=glcint), intent(in), value :: x, y

    if( moving )then
      angle%x = angle%x + (x - begin%x)
      angle%y = angle%y + (y - begin%y)
      begin = coord(x,y)
      newmodel = .true.
      call glutpostredisplay()
    end if

  end subroutine motion

  subroutine controlLights(value) bind(C)

    integer(kind=glcint), intent(in), value :: value

    select case (value)
    case(1)
      lightZeroSwitch = .not. lightZeroSwitch
      if (lightZeroSwitch) then
        call glenable(GL_LIGHT0)
      else
        call gldisable(GL_LIGHT0)
      end if
    case(2)
      lightOneSwitch = .not. lightOneSwitch
      if (lightOneSwitch) then
        call glenable(GL_LIGHT1)
      else
        call gldisable(GL_LIGHT1)
      end if
    end select

    call glutpostredisplay()

  end subroutine controlLights

end module gltdino2
program main

  use opengl_gl
  use opengl_glu
  use opengl_glut
  
  use gltdino2
  
  implicit none

  integer :: i

  call glutinit()
  call glutinitdisplaymode(ior(ior(GLUT_RGB,GLUT_DOUBLE),GLUT_DEPTH))
  i = glutcreatewindow("glutdino"//char(0))
  call glutdisplayfunc(redraw)
  call glutmousefunc(mouse)
  
  call glutmotionfunc(motion)
  i = glutcreatemenu(controlLights)
  call glutaddmenuentry("Toggle right light"//char(0), 1)
  call glutaddmenuentry("Toggle left light"//char(0), 2)
  call glutattachmenu(GLUT_RIGHT_BUTTON)
  
  call makeDinosaur()
  
  call glenable(GL_CULL_FACE)
  call glenable(GL_DEPTH_TEST)
  call glenable(GL_LIGHTING)
  call glmatrixmode(GL_PROJECTION)
  call gluperspective(40.0_gldouble,    & ! field of view in degree
                          1.0_gldouble, & ! aspect ratio
                          1.0_gldouble, & ! Z near
                         40.0_gldouble)   ! Z far
  call glmatrixmode(GL_MODELVIEW)
  call glulookat( &
   0.0_gldouble, 0.0_gldouble, 30.0_gldouble, & ! eye is at (0,0,30)
   0.0_gldouble, 0.0_gldouble, 0.0_gldouble,  & ! center is at (0,0,0)
   0.0_gldouble, 1.0_gldouble, 0.0_gldouble)    ! up is in postivie Y direction
  
  call glpushmatrix()  ! dummy push so we can pop on model recalc 
  call gllightmodeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1)
  call gllightfv(GL_LIGHT0, GL_POSITION, lightZeroPosition)
  call gllightfv(GL_LIGHT0, GL_DIFFUSE, lightZeroColor)
  call gllightf(GL_LIGHT0, GL_CONSTANT_ATTENUATION, 0.1)
  call gllightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.05)
  call gllightfv(GL_LIGHT1, GL_POSITION, lightOnePosition)
  call gllightfv(GL_LIGHT1, GL_DIFFUSE, lightOneColor)
  call glenable(GL_LIGHT0)
  call glenable(GL_LIGHT1)

  call glutmainloop()
  
end program main
