!##################################################################################################################################
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! Code for the creation of the simulation native in fortran.                                                                     !#
! Version 1.0.0                                                                                                                  !#
! MPI directives are necessary! (install mpifor and mpirun)                                                                      !#
!##################################################################################################################################





!##################################################################################################################################
! Main module for global variables                                                                                               !#
Module global                                                                                                                    !#
    use mpi                                                                                                                      !#
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
        integer:: type_Wall                            !What type of wall?
                                !Temerature - velocity - Preassure : 101 (example)
                                !1 ----> (velocity: dirichlet = (u=ud , v=vd), Temperature: Neumann = 0 , Preassure: Neumann = 0)
                                !0 ----> (Neumann: )
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
    character(LEN=200):: dirname , filename , string               !Names for file creation and directory structure
    Logical:: save_image , Exist_Thermal_simulation                !Simulation options
    integer:: what_thermal_simulation, what_velocity_simulation &  !Simulation options
            , image_frequence
    integer:: i , ii , iii                                         !Integer counters
    integer:: step , pressure_step , velo_step                     !Number of iterations in simulation
    integer :: ERROR , numprocs, ID                                !MPI integers
    integer :: Type_of_visualization                               !Type of visualization
    logical :: interface, parrallel_implicit                       !MPI arguments
                                                                                                                                 !#
end Module                                                                                                                       !#
!##################################################################################################################################







!##################################################################################################################################
!Routine for the simulation                                                                                                      !#
subroutine Simulation()                                                                                                          !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#
    character(LEN=200) :: windows_name                             !Name of the window

    !Parameters of the simulation:
    Nx = 30                                                        !Space cells in x direction
    Ny = Nx                                                        !Space cells in y direction
    Lx = 1.d0                                                      !Size of space domain in x  (m)
    Ly = Lx                                                        !Size of space domain in y  (m)
    dx =  Lx / (Nx)                                                !Cells length in x (m)
    dy =  Ly / (Ny)                                                !Cells length in y (m)
    !Physical determination of fluid flow:
    alpha =  1.43d-7                                               !Thermal condutivity (water with 25 degree Celcius) (m**2/s)
    mi = 8.891d-4                                                  !viscosity (water with 25 degree Celcius) (n*s/m**2) (https://www.engineeringtoolbox.com/water-dynamic-kinematic-viscosity-d_596.html)
    rho = 8.2d0 !997.7d0                                                  !Specific mass (water with 25 degree Celcius) (N/m**3)
    nu = mi/rho                                                    !Knematic viscosity
    V_top = 0.02d0                                                 !Velocity of top plate
    Reynolds = V_top*Lx/(nu)                                       !Reynolds number
    gx = 0.0d0                                                     !Gravity in x direction (m/s**2)
    gy = 0.0d0                                                     !Gravity in y direction (m/s**2) (http://lilith.fisica.ufmg.br/~dsoares/g/g.htm)
    vi = 0.0d0                                                     !Initial condition parameter for vertical velocity
    ui = 0.0d0                                                     !Initial condition parameter for horizontal velocity
    pi = 0.0d0                                                     !Initial condition parameter for preassure
    ti = 25.0d0                                                    !Initial condition parameter for temperature
    !Simulation convergence parameters:
    cfl = 0.01                                                     !Relation betwen time and space steps
    dt = (cfl * dx**2 )/ nu                                        !Time step length (s)
    time = 2500                                                    !Total time of simulation (s)
    increment = 1.d-10                                             !Increment for implicity Gaus-Seidel solutions
    !Simulation Pannel control:
    save_image = .FALSE.                                           !Save file is wanted?
    image_frequence = 100                                          !In how many iterations the image must be saved?
    filename = "simulacao_piloto"                                  !Name of saved file
    Windows_name = "Program Cavidade"                              !Name of the window of graphical representation
    what_thermal_simulation = 2                                    !Type of thermal numerical solution (1 = explicit / 2 = implicit)
    Exist_Thermal_simulation = .FALSE.                             !If there is thermal simulation, or isotermic hipotesis
    what_velocity_simulation = 2                                   !Type of velocity numerical solution (1 = explicit / 2 = implicit)
    Type_of_visualization = 2                                      !Type og graphics in the screen from simulation


    !Data from the present simulation
    Print*, "Simulation " , filename
    Print*, "Reynolds = " , Reynolds
    Print*, "dt= " , dt
    Print*, "dx= " , dx


    !First Contact with visualization process, for initial parametrization and window creation.
    call MPI_SEND( Nx  , 1 , MPI_INTEGER , 1 , 1 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in x
    call MPI_SEND( Ny  , 1 , MPI_INTEGER , 1 , 0 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in y
    call MPI_SEND( Windows_name  , 200 , MPI_CHARACTER, 1 , 1 , MPI_COMM_WORLD , ERROR)           !Name of the window of visualization
    call MPI_SEND( Type_of_visualization  , 1 , MPI_INTEGER, 1 , 0 , MPI_COMM_WORLD , ERROR)      !Name of the window of visualization


    !Allocation of simulations buffers:
    allocate(C(Nx + 2  , Ny + 2))                                  !   1 extra cell   |wall|      N cells      |wall| 1 extra cell
    allocate(tr(Nx + 2  , Ny + 2))


    !Simulation Routines:

     !call Simu_routines()                                          !The protocolls are called for simulation development

    !Simulation Statistics:

    print*, "------------------------------------------------------"
    print*, "-                                                    -"
    print*, "-                  Fim da simulação                  -"
    print*, "-                                                    -"
    print*, "------------------------------------------------------"

    !Dellocation of simulations buffers:
    deallocate(C)
    deallocate(tr)
                                                                                                                                 !#
end subroutine simulation                                                                                                        !#
!##################################################################################################################################