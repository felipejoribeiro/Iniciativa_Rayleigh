! Program for de bidimensional simulation of a cavity system, using MPI for parallel processing and OPENGL for graphics
! Undergraduate: Felipe J. O. Ribeiro
! Professor: Aristeu da Silveira Neto

!Module with variables and functions of the OPENGL API.
module graphics
    !OPENGL libraries
    use global
    use, intrinsic :: ISO_C_BINDING
    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none
    !Parameters for the program
    integer :: window, frame = 0                     !Identificador de janela, um integer, frames

    real , dimension(:,:), allocatable :: dBuffer              !dados para mostrar na tela

    !Functions for dysplaing data
    contains


    !Function that is called once per frame.
    subroutine idle() bind(C)
        use mpi

        integer:: status(MPI_STATUS_SIZE) , ERROR

        call assimilation()           !Function that get values

        call scatter()                !Render heatmap

        call Legend()                 !Render Legend

    end subroutine idle




    !Rotine that DRAWS the points on the screen from dBuffer
    subroutine scatter() bind(C)
        use global
        integer :: i , ii
        call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)             !Clear screen for new frame

        !Draw points:

        call glBlendFunc(GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA)
        call glPointSize(real(1000 / N))
        !call glEnable(GL_POINT_SMOOTH)
        !call glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
        !call glEnable(GL_BLEND)
        !call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        do i = 1 , size(dBuffer(: , 1))
            do ii = 1 , size(dBuffer(1 , :))
                if(dBuffer(i , ii) < 0)then
                    call glcolor3f(0.1, 1.0 - abs(dBuffer(i , ii)), 0.1 + abs(dBuffer(i , ii)))
                    call glBegin(GL_POINTS)
                    call glVertex2f( real(0.05) + real(0.9)*(real(i-0.5))/real(size(dBuffer(:,1))), &
                            real(0.05) + real(0.9)*(real(ii - 0.5))/real(size(dBuffer(1 , :))) )
                    call glEnd()
                else
                    call glcolor3f(dBuffer(i , ii) + 0.1, 1 - dBuffer(i , ii), 0.1)
                    call glBegin(GL_POINTS)
                    call glVertex2f( real(0.05) + real(0.9)*(real(i-0.5))/real(size(dBuffer(:,1))), &
                            real(0.05) + real(0.9)*(real(ii - 0.5))/real(size(dBuffer(1 , :))) )
                    call glEnd()
                end if
            end do
        end do



        call glutSwapBuffers()
        call glflush()     !Process OPENGL precompiled codes
    end subroutine scatter


    !Rotine that DRAWS the Legend on the screen
    subroutine legend() bind(C)






    end subroutine legend



    !Subroutine that get the values to be printed on the screen
    subroutine assimilation
        use mpi
        use global
        implicit none
        integer:: i , ii , ERROR , status(MPI_STATUS_SIZE)
        double precision:: check
        logical:: FLAG

        call MPI_RECV( T , size(T) , MPI_DOUBLE_PRECISION , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)
        !call MPI_SEND(check , 1 , MPI_DOUBLE_PRECISION , 0 , 0 , MPI_COMM_WORLD , ERROR)

        do i = 1 , size(dBuffer(:, 1))
            do ii = 1 , size(dBuffer(1, :))
                dBuffer(i , ii) =  T(i , ii)
            end do
        end do


    end subroutine assimilation

end module graphics

!Global variables used in the simulation and both processes
module global
    implicit none
    double precision , dimension(:,:) , allocatable :: T , Ti , P , u , v , e , ul , vl !Temperature and pressure domains
    integer :: N                                                  !Number of cells (ao quadrado)
    double precision :: L , alpha , ds , cfl , dt , incremento , tempo &
    , PI = 3.14159265359 , ui , vi

end module global











!Main program, with OPENGL and MPI initializations
program cavidade
    use graphics                                                   ! Module with graphics variables
    use global                                                     ! Module with global simulation variables
    use mpi                                                        ! Library for MPI usage
    implicit none

    !MPI related variables:
    integer :: ERROR , numprocs, ID

    call MPI_INIT(ERROR)                                           !Initiate MPI capabilities
    call MPI_COMM_RANK(MPI_COMM_WORLD, ID , ERROR)                 !Asks for the number of identity of the process
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ERROR)            !Asks for the total number of affiliated processes

    !Moment where each process is made. There is a principal process, where
    !the simulation occurs, and a secondary, where the OPENGL visualization tool is running.


    !Parametrization:

    N = 100                                                      !Number of cells (x = 22 , y = 22)
    L =  5                                                  !Length of the simulation square domain (meters)
    ds =  L / (N- 1)                                             !Cells length
    alpha =  1.0                                                 !Thermal condutivity (aluminium)
    cfl = 0.1                                                   !valor de convergencia
    dt = cfl * ds**2/alpha                                       !time step length (seconds)
    tempo = 2.5                                                    !Time (seconds)
    ui =  10.0                                                     !X velocity (m/s)
    vi =   0.0                                                     !Y velocity (m/s)
    incremento = 1 * 10**(-5)                                    !Imcrement for the implicit method

    print*, dt , ds


    !Allocating memory:
    allocate(T(N + 2,N + 2))
    allocate(Ti(N + 2,N + 2))
    allocate(u(N + 2,N + 2))
    allocate(v(N + 2,N + 2))
    allocate(ul(N + 2,N + 2))
    allocate(vl(N + 2,N + 2))
    allocate(e(N + 2,N + 2))
    allocate(dBuffer(N + 2,N + 2))

    if (numprocs == 2)then  !Check if there is two processes
        if(ID == 0)then
            call Simulation()
        else
            call Visualization()
        end if
    end if

    print*, tempo
    call CPU_TIME(tempo)
    print*, tempo

    !Desalocando variáveis
    deallocate(e)
    deallocate(T)
    deallocate(Ti)
    deallocate(u)
    deallocate(v)
    deallocate(ul)
    deallocate(vl)
    deallocate(dBuffer)

    call MPI_FINALIZE(ERROR)                                    !End all MPI capabilities

end program cavidade

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CFD Realm



!Where there is the simulation algorithm. What means that all the CFD Code is here.
subroutine Simulation()
    use global
    use mpi
    implicit none
    integer :: i , ii , step , ERROR , status(MPI_STATUS_SIZE)
    double precision :: check




    !Stabilishing initial conditions
    do i = 1 , N + 2
        do ii = 1 , N + 2

            if(i > N/4 .AND. i < N/2 .AND. ii > N/4 .AND. ii < N/2 )then
                T(i , ii) = 2
            else
                T(i , ii) =-1
            end if

            u(i , ii) = ui

            v(i , ii) = vi

        end do
    end do


    step = 1

    ! The T matrix is sent to data ploting
    call MPI_SEND( T , size(T) , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , ERROR)
    !call MPI_RECV(check , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)


    ! DEMO iterations
    ! do while(-3 < 0)
    !     CALL CPU_TIME(check)
    !     do i = 1 , size(T(:, 1))
    !         do ii = 1 , size(T(1, :))
    !             T(i , ii) = &!2 * (exp( - ( real(i - 150 - 100 * sin(check) )/50)**2 )  *   &
    !             !exp( -( real(ii - 150 - 50 * cos(check))/50)**2 ) ) - 1
    !             (sin( (ii * ds * 100 + 100 * check) / 50 ) + cos ( (i * ds * 100 + 100 * check)/50  ))/2
    !         end do
    !     end do




    !     ! The T matrix is sent to data ploting
    !     call MPI_SEND( T , size(T) , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , ERROR)
    !     !call MPI_RECV(check , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)

    ! end do


    ! Explicit simulation
    do while(step * dt < tempo - dt)

        !Simulation for this step.
        do i = 2 , N + 1
            do ii = 2 , N + 1

                Ti(i , ii) = T(i , ii) * (1 - 4 * alpha * dt/(ds**2)) &
                + T(i + 1, ii) * (alpha * dt / (ds**2) - u(i , ii) * dt/(2 * ds)) &
                + T(i , ii + 1) * (alpha * dt /(ds**2) - v(i , ii) * dt / (2 * ds))&
                + T(i -1, ii) * ((alpha * dt) / (ds * ds) + u(i , ii) * dt /(2 * ds)) &
                + T(i, ii - 1) *((alpha * dt) /(ds * ds) + v(i , ii) * dt/(2 * ds))

            end do
        end do


        do i = 2 , N + 1
            do ii = 2 , N + 1

                Ti(i , ii) = T(i , ii) * (1 - 4 * alpha * dt/(ds**2)) &
                + T(i + 1, ii) * (alpha * dt / (ds**2) - u(i , ii) * dt/(2 * ds)) &
                + T(i , ii + 1) * (alpha * dt /(ds**2) - v(i , ii) * dt / (2 * ds))&
                + T(i -1, ii) * ((alpha * dt) / (ds * ds) + u(i , ii) * dt /(2 * ds)) &
                + T(i, ii - 1) *((alpha * dt) /(ds * ds) + v(i , ii) * dt/(2 * ds))

            end do
        end do


        !Boundary conditions
        do i = 1 , size(T(:,1))
            Ti(i , N + 2) = Ti(i , N + 1 )
            Ti(i , 1) = Ti(i , 2)
        end do

        do i = 1 , size(T(1,:))
            Ti(N + 2, i) = Ti(N + 1, i)
            Ti(1 , i) = Ti(2 , i)
        end do

        !Simulation for this step.
        do i = 1 , size(T(:,1))
            do ii = 1 , size(T(1,:))
                T(i , ii) = Ti(i , ii)
            end do
        end do


        ! The T matrix is sent to data ploting
        call MPI_SEND( T , size(T) , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , ERROR)
        !call MPI_RECV(check , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)
        step = step + 1
    end do


    tempo = step



end subroutine simulation







































































!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Visualization Realm


!The OPENGL code, for creation of the visualization tool.
subroutine Visualization()
    use graphics
    implicit none
    call NewWindow()         !Window creation
    call ConfigureWindow()   !Window configuration
    call glutmainloop()      !Enter IDLE protocol
end subroutine Visualization



!Subroutine to create a window
subroutine NewWindow()
  use graphics
  implicit none
  integer :: xpixel , ypixel

  xpixel = 600
  ypixel = 600

  call glutinit()
  call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)              !Exibicion mode
  call glutInitWindowSize(xpixel, ypixel)                                !Determine window width in pixels
  window = glutcreatewindow("Cavity simulation")                         !A name is given to the window
  call glutIdleFunc(idle)                                                !Repeticion function
  return

end subroutine NewWindow

! Configuring the window
subroutine ConfigureWindow()
  use graphics
  implicit none

  call glclearcolor(1.0, 1.0, 1.0, 0.0)                                  !seta cor no background.
  call glmatrixmode(GL_PROJECTION)                                       !Matrix context of operations
  call glloadidentity()                                                  !Identity matrix loaded
  call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !Limits (left, right, down, up, close to the camera, far from the camera)
  call glEnableClientState(GL_VERTEX_ARRAY)                              !Funcionalities

end subroutine ConfigureWindow