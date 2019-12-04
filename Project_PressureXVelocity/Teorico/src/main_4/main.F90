!##################################################################################################################################
! Program for de bidimensional simulation of arbitrary dymensions, using MPI for parallel processing and OPENGL for graphics     !#
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! It is necessary the presence in the same directory of the file visalization.f90 and simulation.f90.                            !#
! MPIRUN and OPEMGL are necessary                                                                                                !#
! Version 4.0.0                                                                                                                  !#
!##################################################################################################################################

include 'visualization.F90'   !Graphical codes (version 2.0.0 required)
include 'simulation.F90'      !Codes for 2D CFD solving (version 1.0.0 required)
include 'parallel_simu.F90'   !Code for parallelisation of the physical routines.

!##################################################################################################################################
! Main module for global variables                                                                                               !#
Module global                                                                                                                    !#
    use mpi                                                                                                                      !#
    use, intrinsic :: ISO_C_BINDING                                                                                              !#
    use opengl_gl                                                                                                                !#
    use opengl_glu                                                                                                               !#
    use opengl_glut                                                                                                              !#
    implicit none                                                                                                                !#

    ! Definition of the CELL entity
    type Cell
        double precision:: T , Ti                      !Temperature variables
        double precision:: P , Pi , Pl                 !Double precision
        double precision:: v , u , vl , ul             !velocities variables
        double precision:: dx , dy                     !Size variables
        double precision:: x , y                       !Absolute coordinates location
        double precision:: alpha , nu , rho , mi       !Local physical variables
        double precision:: div , divi                  !Divergent of velocity in this cell
        LOGICAL:: Wall                                 !Is Wall?
        integer,dimension(3):: type_Wall               !What type of wall?
                                !Temerature - velocity - Preassure : (1,0,1) (example)
                                !1 ----> (velocity: dirichlet = (u=ud , v=vd), Temperature:dirichlet = (T=td) , Preassure: dirichlet = (p=pd))
                                !0 ----> (velocity: Neumann = 0 Temperature: Neumann = 0 , Preassure: Neumann = 0)
    end type

    !Phisical parameters
    integer:: Nx , Ny                                              !Space discretization
    double precision:: Lx , Ly , dx , dy                           !Geometry of the space domain
    double precision:: alpha , nu , mi , rho , gx , gy             !physical variables
    double precision:: vi , ui , pi , Ti , Reynolds, V_top         !Initial condition variables
    double precision:: time , dt , cfl , increment                 !Convergence variables
    type(Cell), dimension(:,:) , allocatable :: C                  !Temperature and extra matrix
    double precision, dimension(:,:) , allocatable :: tr           !Transition variable

    !Computational parameters
    double precision, dimension(:,:,:) , allocatable :: Dbuffer    !Transition variable
    character(LEN=200):: OS, dirname , filename ,string,bar,logg   !Names for file creation and directory structure
    character(LEN=200) :: windows_name                             !Name of the window
    character(LEN=200):: date_os , time_os                         !Time and date
    Logical:: save_image , Exist_Thermal_simulation                !Simulation options
    integer:: what_thermal_simulation, what_velocity_simulation &  !Simulation options
            , image_frequence
    integer:: i , ii , iii                                         !Integer counters
    integer:: step , pressure_step , velo_step                     !Number of iterations in simulation
    integer :: ERROR , numprocs, ID , status(MPI_STATUS_SIZE)      !MPI integers
    logical :: interface, parrallel_implicit                       !MPI arguments
    integer :: window ,  xpixel , ypixel                           !window id , size of window


    !Function for dysplaing data (IDLE function)
    contains

        !Function that is called once per frame.
        subroutine idle() bind(C)

            call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))        !Clear screen for new frame
            call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)             !Clear screen for new frame

            ! Prearing for DATA aquisition, there is 10 slots for physical matrixes, but can be increased
            Allocate(Dbuffer(10 , Nx + 2 , Ny + 2))       ! DATA allocation
            
            !Rendering routines:
            call RENDERING_ENGINE()                       ! code with all rendering and interaction routines

            deallocate(Dbuffer)                           ! DATA deallocation after shown
            
            call glutSwapBuffers()       !send new buffer of collors simutaneusly for rendering, prevent tearing
            call glflush()               !Process OPENGL precompiled codes

        end subroutine idle
                                                                                                                                 !#
end Module                                                                                                                       !#
!##################################################################################################################################




!##################################################################################################################################
!Main program, responsible for distributin mpi jobs                                                                               #
program main                                                                                                                     !#
	use global                                                                                                                   !#
	implicit none                                                                                                                !#

    !MPI initialization
	call MPI_INIT(ERROR)                                           !Initiate MPI capabilities
    call MPI_COMM_RANK(MPI_COMM_WORLD, ID , ERROR)                 !Asks for the number of identity of the process
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ERROR)            !Asks for the total number of affiliated processes

    !Parameters for the number of processes
    interface =  .TRUE.                                           !There will be a graphicall interface?
    parrallel_implicit = .FALSE.                                   !The implicit iterations must be divided by various prosesses?

    !The courent directory is identified for file creation and get OS
    call identity_directory()                                       !The courent directory is identified and the OS

    !Where each process will receive diferente objectives
    call identity_decision()                                       !where the tasks are distributed


    !MPI finalization
    call MPI_FINALIZE(ERROR)                                       !End all MPI capabilities

end program main                                                                                                                 !#
!##################################################################################################################################







!##################################################################################################################################
!Routine that identify what tipe of process will be implemented in the job                                                        #
subroutine identity_decision()                                                                                                   !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#

    !MPI task manager
    if (numprocs == 1) then

        !Serial simulation terminal dependant
        call simulation()

    elseif(numprocs == 2) then
        if (interface) then

            if (parrallel_implicit) then

                !Serial simulation with graphycal interface
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                else
                    print*,"CAUTION -------> Visualization and Parallel algorithm are .TRUE. but there is only 2"
                    print*,"                  processes available. Visulaization will be used."
                    call Visualization()                                   !The visualization routine
                end if

            else

                !Serial simulation with graphycal interface
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                else
                    call Visualization()                                   !The visualization routine
                end if

            end if

        else
            if (parrallel_implicit) then

                !Simulation parallelised terminal dependant
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                else
                    call Simu_parallel()                                   !The slave routine
                end if

            else

                !Serial simulation terminal dependant
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                else
                    write(string , "(I3)") ID
                    print*,"CAUTION -------> process " , TRIM(string) , "  not utilised"
                end if

            end if
        end if
    else
        if (interface) then
            if (parrallel_implicit) then

                !Simulation parallelised with graphycal interface
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                elseif(ID == 1)then
                    call Visualization()                                   !The visualization routine
                else
                    call Simu_parallel()                                   !The slave routine
                end if

            else

                !Serial simulation with graphycal interface
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                elseif(ID == 1)then
                    call Visualization()                                   !The visualization routine
                else
                    write(string , "(I3)") ID
                    print*,"CAUTION -------> process " , TRIM(string) , "  not utilised"
                end if

            end if
        else
            if (parrallel_implicit) then

                !Simulation parallelised terminal dependant
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                elseif(ID == 1)then
                    write(string , "(I3)") ID
                    print*,"CAUTION -------> process " , TRIM(string) , "  not utilised"
                else
                    call Simu_parallel()                                   !The slave routine
                end if

            else

                !Serial simulation terminal dependant
                if(ID == 0)then
                    call Simulation()                                      !The physical simulation routine
                elseif(ID == 1)then
                    write(string , "(I3)") ID
                    print*,"CAUTION -------> process " , TRIM(string) , "  not utilised"
                else
                    write(string , "(I3)") ID
                    print*,"CAUTION -------> process " , TRIM(string) , "  not utilised"
                end if

            end if
        end if
    end if

    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine identity_decision                                                                                                 !#
!##################################################################################################################################





!##################################################################################################################################
!This piece of code read the courrent directory and save the actual directory structure for saving propourses, and get OS         #
subroutine identity_directory()                                                                                                  !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#
    integer, dimension(8) :: date_time
    character*10 :: b(3)

    !Intriscic functions
    call GETCWD(dirname)                                                         !Gets courrent directory
    call date_and_time( b(1), b(2), b(3), date_time )                            !Gets time and date

    !Formating date and time
    date_os = b(1)(7:8) // "/" // b(1)(5:6) // "/" // b(1)(1:4)
    time_os = b(2)(1:2) // ":" // b(2)(3:4) // ":" // b(2)(5:8)

    !Checking the system OS
    if(dirname(1:1) == "/")then                                                   !This type of bar is a UNIX thing
    OS = "LINUX"
    bar = "/"
    else
    OS = "WINDOWS"
    bar = "\"
    endif

    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine identity_directory                                                                                                !#
!##################################################################################################################################






!##################################################################################################################################
!Routine that prints stuf in the terminal                                                                                        !#
subroutine LOG(what_case)                                                                                                        !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#
    character(LEN=200), intent (in) :: what_case                       !Variable for selecting what text is to print


    !The subroutine see what is the context
    if (interface .eqv. .TRUE.) then

        select case(what_case)

            case("inicio")! First print on the screen, presenting the program.

                Print*, ""
                Print*, ""
                Print*, ""
                Print*, "#########################################################################################"
                Print*, "#                                                                                       #"
                Print*, "#   Program for bidimensional simulations                                               #"
                Print*, "#                                                                                       #"
                Print*, "#   student: Felipe Jose Oliveira Ribeiro                                               #"
                Print*, "#   Professor: Aristeu Silveira Neto                                                    #"
                Print*, "#                                                                                       #"
                Print*, "#########################################################################################"
                Print*, trim(filename)
                Print*, trim(date_os) , "   (" , trim(time_os) , ")"
                Print*, "Reynolds = " , Reynolds
                Print*, "dt = " , dt
                Print*, "dx = " , dx
                Print*, "CFL =" , cfl

            case("iterações")! Print tha is called once a time step

                !WRITE(*,101) char(13), MAXVAL(C%div) , pressure_step , velo_step
                101 FORMAT(1a1,ES7.1, I5, I5,$)

            case("fim") !End of simulation, last log

            print*, "######################################################"
            print*, "#                                                    #"
            print*, "#                  Fim da simulação                  #"
            print*, "#                                                    #"
            print*, "######################################################"

        end select
    end if


    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine LOG                                                                                                               !#
!##################################################################################################################################






!##################################################################################################################################
!Routine that prints stuf in the terminal                                                                                        !#
subroutine Graph(what_case)                                                                                                      !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#
    character(LEN=200), intent (in) :: what_case                       !Variable for selecting what text is to print


    !The subroutine see what is the context
    if (interface) then

        select case(what_case)

            case("inicio_send")!First comunication with visulaization process, with valuable information

                !Initial parametrization and window creation.
                call MPI_SEND( Nx  , 1 , MPI_INTEGER , 1 , 1 , MPI_COMM_WORLD , ERROR)                                   !Size of simulation data buffer in x
                call MPI_SEND( Ny  , 1 , MPI_INTEGER , 1 , 0 , MPI_COMM_WORLD , ERROR)                                   !Size of simulation data buffer in y
                call MPI_SEND( Windows_name  , 200 , MPI_CHARACTER, 1 , 1 , MPI_COMM_WORLD , ERROR)                      !Name of the window of visualization

            case("inicio_receive")!!First comunication with visulaization process, with valuable information

                call MPI_RECV( Nx , 1 , MPI_INTEGER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in x
                call MPI_RECV( Ny , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in y
                call MPI_RECV( Windows_name , 200 , MPI_CHARACTER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)            !Name of the window of visualization


            case("iterações")!DATA from the phisicall iterations


        end select
    end if


    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine Graph                                                                                                             !#
!##################################################################################################################################