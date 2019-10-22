! Program for de bidimensional simulation of arbitrary dymensions, using MPI for parallel processing and OPENGL for graphics
! Undergraduate: Felipe J. O. Ribeiro
! Professor: Aristeu da Silveira Neto
! Institution: MfLab - FEMEC - UFU
! It is necessary the presence in the same directory of the file visalization.f90, with
! the rights MPI send/rcv routines.

include 'visualization.f90'  !Graphical codes

! Main module for global variables
Module global
	use mpi
	implicit none

	! Definitio of the CELL entity 
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
                                !1 ----> (velocity: dirichlet = (u=ud , v=vd), Temperature: Neumann = 0 , Preassure: Neumann = 0)
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
    character*100:: dirname , filename                             !Names for file creation and directory structure
    Logical:: save_image , Exist_Thermal_simulation                !Simulation options
    integer:: what_thermal_simulation, what_velocity_simulation &  !Simulation options
            , image_frequence        
    integer:: i , ii , iii                                         !Integer counters
    integer:: step , pressure_step , velo_step                     !Number of iterations in simulation
    integer :: ERROR , numprocs, ID                                !MPI integers
    integer :: Type_of_visualization                               !Type of visualization

end Module







!Main program, responsible for distributin mpi jobs
program main
	use global
	implicit none


	call MPI_INIT(ERROR)                                           !Initiate MPI capabilities
    call MPI_COMM_RANK(MPI_COMM_WORLD, ID , ERROR)                 !Asks for the number of identity of the process
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ERROR)            !Asks for the total number of affiliated processes

    
    if (numprocs == 2)then                                         !Check if there is two processes
        if(ID == 0)then
            call Simulation()                                      !Down the code, the simulation routine
        else
            call Visualization()                                   !In other file
        end if
    else if(numprocs == 1)then                                     !Down the code, the simulation routine
        call Simulation()
    end if



    call MPI_FINALIZE(ERROR)                                       !End all MPI capabilities

end program main