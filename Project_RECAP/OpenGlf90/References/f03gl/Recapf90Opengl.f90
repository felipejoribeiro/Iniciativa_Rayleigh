module Lib
    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none
    ! vertex variable
    type Vertex
        real(Kind = gldouble) :: x, y, z        ! localização 3D
        real(Kind = gldouble) :: r, g, b, a     ! cor e alpha
    end type Vertex
    ! Data variable
    type Data
        real(Kind = gldouble) :: x , y , z
    end type Data
        integer :: frame                        ! auto explicativo
    ! functions to use
    contains

        !display content
        subroutine display() bind(c)
            use opengl_gl
            use opengl_glut
            implicit none

            frame = frame + 1
            print*, "frame = " , frame

            call glclear(GL_COLOR_BUFFER_BIT)

            call glcalllist(1)
            call glutswapbuffers

        end subroutine display


        !Create window
        function createwindow(WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_NAME)
            use opengl_gl
            use opengl_glu
            use opengl_glut
            implicit none
            integer(Kind = GLint) :: createwindow
            integer(Kind = GLint) :: WINDOW_WIDTH, WINDOW_HEIGHT
            character(:), allocatable :: string
            character(len=*) :: WINDOW_NAME
            string = WINDOW_NAME
            call glutinit()
            call glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT)
            call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)
            createwindow = glutcreatewindow(string)
            if(createwindow /= 1) then
                print*, "Erro! Janela não criada"
                stop
            end if
            return
        end function createwindow

end module Lib




!  Recap algorithm, with heatMap
! Heat map for temperature simulations:
program main
    use opengl_gl
    use opengl_glu
    use opengl_glut
    use Lib
    implicit none
    integer:: window, i


    ! Cria a janela!
    window = createwindow(450, 450, "Thermal simulation - Recap")


    ! Cria geometrias
    call glnewlist(1, GL_COMPILE)

        ! crate point
        call glPointSize(100.0)
        call glColor4f(1.0, 0.0 , 1.0, 0.1)
        call glBegin(GL_POINTS)
            call glVertex2f(-0.25, 0.0)
        call glEnd()
        call glColor4f(0.0, 1.0 , 1.0, 0.1)
        call glBegin(GL_POINTS)
            call glVertex2f(0.25, 0.0)
        call glEnd()

    call glendlist


    do i = 1,10

        ! display content
    call glutdisplayfunc(display)

    ! return
    call glutmainloop()

    end do

end program main






