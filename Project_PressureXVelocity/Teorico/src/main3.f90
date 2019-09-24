! Program for de bidimensional simulation of a cavity system, using MPI for parallel processing and OPENGL for graphics
! Undergraduate: Felipe J. O. Ribeiro
! Professor: Aristeu da Silveira Neto
! It is necessary the presence in the same directory of the file visalization.f90

include 'visualization.f90'  ! Graphical codes

! Main module for global variables
Module global
    implicit none

    ! Definition of Cell Data type:
    type Cell
        double precision:: T , Ti                      !Temperature variables
        double precision:: v , u , vl , ul             !velocities variables
        double precision:: dx , dy                     !Size variables
        double precision:: x , y                       !Absolute coordinates location
        double precision:: alpha , nu , rho            !Local physical variables
        LOGICAL:: Wall                                 !Is Wall?
    end type cell

    !Other global parameters
    integer:: Nx , Ny                                              !Space discretization
    integer:: i , ii , iii                                         !Integer counters
    double precision:: Lx , Ly , dx , dy                           !Geometry of the space domain
    double precision:: alpha , nu , rho , gx , gy                  !physical variables
    double precision:: time , dt , cfl                             !Convergence variables
    type(Cell), dimension(:,:) , allocatable :: C                  !Temperature and extra matrix
    character*100:: dirname , filename                             !Names for file creation and directory structure
    Logical:: save_image                                           !Simulation options
    integer:: what_thermal_simulation, what_velocity_simulation    !Simulation options

end module global






! Main program, responsible for distributin mpi jobs
program cavidade
    use mpi                                                        !Library for MPI usage
    implicit none
    integer :: ERROR , numprocs, ID                                !MPI integers

    call MPI_INIT(ERROR)                                           !Initiate MPI capabilities
    call MPI_COMM_RANK(MPI_COMM_WORLD, ID , ERROR)                 !Asks for the number of identity of the process
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ERROR)            !Asks for the total number of affiliated processes



    if (numprocs == 2)then  !Check if there is two processes
        if(ID == 0)then
            call Simulation()     ! down the code, the simulation routine
        else
            call Visualization()  ! In other file
        end if
    else if(numprocs == 1)then    ! down the code, the simulation routine
        call Simulation()
    end if



    call MPI_FINALIZE(ERROR)                                    !End all MPI capabilities


end program cavidade







! Routine for the simulation
subroutine Simulation()
    use global
    use mpi
    implicit none
    integer :: ERROR        ! MPI callback
    character*200 :: windows_name                              !Name of the window

    !Parameters of the simulation:
    Nx = 100                            !Space cells in x direction
    Ny = Nx                             !Space cells in y direction
    Lx = 1.d0                           !Size of space domain in x  (m)
    Ly = Lx                             !Size of space domain in y  (m)
    dx =  Lx / (Nx)                     !Cells length in x (m)
    dy =  Ly / (Ny)                     !Cells length in y (m)
    !Physical determination of fluid:
    alpha =  1.43d-7                    !Thermal condutivity (water with 25 degree Celcius) (m**2/s)
    nu = 8.891d-4                       !viscosity (water with 25 degree Celcius) (n*s/m**2) (https://www.engineeringtoolbox.com/water-dynamic-kinematic-viscosity-d_596.html)
    rho = 9.777d+3                      !specific mass (water with 25 degree Celcius) (N/m**3)
    gx = 0.d0                           !gravity in x direction (m/s**2)
    gy  = 9.7838d0                      !gravity in y direction (m/s**2) (http://lilith.fisica.ufmg.br/~dsoares/g/g.htm)
    !Simulation convergence parameters:
    cfl = 0.1                           !relation betwen time and space steps
    dt = cfl * dx**2/alpha              !time step length (s)
    time = 25                           !Total time of simulation (s)

    !Simulation Pannel control:

    save_image = .FALSE.                !Save file is wanted?
    filename = "simulacao_piloto"       !Name of saved file
    Windows_name = "Program Cavidade"   !Name of the window of graphical representation
    what_thermal_simulation = 1         !Type of thermal numerical solution (1 = explicit / 2 = implicit)
    what_velocity_simulation = 1        !Type of velocity numerical solution (1 = explicit / 2 = implicit)


    !First Contact with visualization process, for initial parametrization and window creation.
    call MPI_SEND( Nx  , 1 , MPI_INTEGER , 1 , 1 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in x
    call MPI_SEND( Ny  , 1 , MPI_INTEGER , 1 , 0 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in y
    call MPI_SEND( Windows_name  , 200 , MPI_CHARACTER, 1 , 1 , MPI_COMM_WORLD , ERROR)           !Name of the window of visualization


    ! Allocation of simulations buffers:
    allocate(C(Nx + 2  , Ny + 2))           !   1 extra cell   |wall|      N cells      |wall| 1 extra cell


    !Simulation Routines:

    call initialconditions()     !Create the initial condition for the simulation






    !Simulation Statistics:

    print*, "------------------------------------------------------"
    print*, "-                                                    -"
    print*, "-                  Fim da simulação                  -"
    print*, "-                                                    -"
    print*, "------------------------------------------------------"

    ! Dellocation of simulations buffers:
    deallocate(C)

end subroutine simulation







! Set initial values for all the domain and simulation values
subroutine initialconditions()
    implicit none



end subroutine initialconditions