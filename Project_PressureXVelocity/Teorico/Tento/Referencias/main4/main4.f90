! Program for de bidimensional simulation of a cavity system, using MPI for parallel processing and OPENGL for graphics
! Undergraduate: Felipe J. O. Ribeiro
! Professor: Aristeu da Silveira Neto
! It is necessary the presence in the same directory of the file visalization.f90 da série 2.0.0

include 'visualization.f90'  !Graphical codes

!Main module for global variables
Module global
    use mpi
    implicit none

    !Definition of Cell Data type:
    type Cell
        double precision:: T , Ti                      !Temperature variables
        double precision:: P , Pi , Pl                 !Double precision
        double precision:: v , u , vl , ul             !velocities variables
        double precision:: dx , dy                     !Size variables
        double precision:: x , y                       !Absolute cobordinates location
        double precision:: alpha , nu , rho , mi       !Local physical variables
        double precision:: div , divi                  !Divergent of velocity in this cell
        LOGICAL:: Wall                                 !Is Wall?
        integer:: type_Wall                            !What type of wall?
                                !1 ----> (velocity: dirichlet = (u-0 , v-0), Temperature: Neumann = 0 , Preassure: Neumann = 0)
                                !2 ----> (velocity: dirichlet = (u-ui , v-vi), Temperature: Neumann = 0 , Preassure: Neumann = 0)
    end type cell

    ! Phisical parameters
    integer:: Nx , Ny                                                    !Space discretization
    double precision:: Lx , Ly , dx , dy                                 !Geometry of the space domain
    double precision:: alpha , nu , mi , rho , gx , gy                   !physical variables
    double precision:: vi , ui , pi , Ti , Reynolds, V_top,ta,tb, dil, RA!Initial condition variables
    double precision:: time , dt , cfl , increment                       !Convergence variables
    type(Cell), dimension(:,:) , allocatable :: C                        !Temperature and extra matrix
    CHARACTER :: CR = CHAR(13)

    !Computational parameters
    integer:: i , ii , iii                                         !Integer counters
    integer:: step , pressure_step , velo_step                     !Number of iterations in simulation
    double precision, dimension(:,:) , allocatable :: tr           !Transition variable
    character*200:: dirname , filename , misc                      !Names for file creation and directory structure
    Logical:: save_image , Exist_Thermal_simulation , folder       !Simulation options
    integer:: what_thermal_simulation, what_velocity_simulation &  !Simulation options
            , image_frequence , step_image
    integer :: ERROR , numprocs, ID                                !MPI integers
    integer :: Type_of_visualization                               !Type of visualization

    contains

    subroutine StripSpaces(string)
        character(len=*) :: string
        integer :: stringLen
        integer :: last, actual

        stringLen = len (string)
        last = 1
        actual = 1

        do while (actual < stringLen)
            if (string(last:last) == ' ') then
                actual = actual + 1
                string(last:last) = string(actual:actual)
                string(actual:actual) = ' '
            else
                last = last + 1
            if (actual < last) &
                actual = last
            endif
        end do

    end subroutine

end module global






!Main program, responsible for distributin mpi jobs
program cavidade
    use global                                                     !Library for global variables
    use mpi                                                        !Library for MPI usage
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


end program cavidade







!Routine for the simulation
subroutine Simulation()
    use global
    use mpi
    implicit none
    character*200 :: windows_name                                  !Name of the window

    !Parameters of the simulation:
    Nx = 40                                                        !Space cells in x direction
    Ny = Nx                                                        !Space cells in y direction
    Lx = 1.d0                                                      !Size of space domain in x  (m)
    Ly = Lx                                                        !Size of space domain in y  (m)
    dx =  Lx / (Nx)                                                !Cells length in x (m)
    dy =  Ly / (Ny)                                                !Cells length in y (m)
    !Physical determination of fluid flow:
    alpha =  1.43d-4                                               !Thermal condutivity (water with 25 degree Celcius) (m**2/s)
    mi = 8.891d-4                                                  !viscosity (water with 25 degree Celcius) (n*s/m**2) (https://www.engineeringtoolbox.com/water-dynamic-kinematic-viscosity-d_596.html)
    rho = 8.2d0 !997.7d0                                           !Specific mass (water with 25 degree Celcius) (N/m**3)
    nu = mi/rho                                                    !Knematic viscosity
    V_top = 0.0d0!2d0                                              !Velocity of top plate
    Reynolds = V_top*Lx/(nu)                                       !Reynolds number
    gx = 0.0d0                                                     !Gravity in x direction (m/s**2)
    gy = 10.0d0                                                    !Gravity in y direction (m/s**2) (http://lilith.fisica.ufmg.br/~dsoares/g/g.htm)
    vi = 0.0d0                                                     !Initial condition parameter for vertical velocity
    ui = 0.0d0                                                     !Initial condition parameter for horizontal velocity
    pi = 0.0d0                                                     !Initial condition parameter for preassure
    ti = 25.0d0                                                    !Initial condition parameter for temperature
    ta = 5.0d0                                                     !Temperature on the left wall
    tb = 50.0d0                                                    !Temperature on the right wall
    dil = 0.0003d0                                                 !Thermal dilatation linear coefficient
    Ra = (gy * dil * (Tb - Ta) * Lx**2)/(alpha * nu)               !Número de Rayleigh.
    !Simulation convergence parameters:
    cfl = 0.025d0                                                  !Relation betwen time and space steps
    dt = (cfl * dx**2 )/ nu                                        !Time step length (s)
    time = 2500                                                    !Total time of simulation (s)
    increment = 1.d-10                                             !Increment for implicity Gaus-Seidel solutions
    !Simulation Pannel control:
    save_image = .FALSE.                                           !Save file is wanted?
    folder = .FALSE.                                               !The results folder is already there?
    image_frequence = 10                                           !In how many iterations the image must be saved?
    Windows_name = "Program Cavidade"                              !Name of the window of graphical representation
    what_thermal_simulation = 2                                    !Type of thermal numerical solution (1 = explicit / 2 = implicit)
    Exist_Thermal_simulation = .FALSE.                             !If there is thermal simulation, or isotermic hipotesis
    what_velocity_simulation = 2                                   !Type of velocity numerical solution (1 = explicit / 2 = implicit)
    Type_of_visualization = 2                                      !Type og graphics in the screen from simulation


    !Data from the present simulation
    Print*, "Simulation " , filename
    Print*, "Ra = " , Ra
    Print*, "dt= " , dt
    Print*, "dx= " , dx
    Print*, "CFl = " , cfl


    !First Contact with visualization process, for initial parametrization and window creation.
    call MPI_SEND( Nx , 1 , MPI_INTEGER , 1 , 1 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in x
    call MPI_SEND( Ny , 1 , MPI_INTEGER , 1 , 0 , MPI_COMM_WORLD , ERROR)                        !Size of simulation data buffer in y
    call MPI_SEND( Lx  , 1 , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)               !Size of simulation data buffer in x
    call MPI_SEND( Ly  , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , ERROR)               !Size of simulation data buffer in y
    call MPI_SEND( Windows_name  , 200 , MPI_CHARACTER, 1 , 1 , MPI_COMM_WORLD , ERROR)           !Name of the window of visualization
    call MPI_SEND( Type_of_visualization  , 1 , MPI_INTEGER, 1 , 0 , MPI_COMM_WORLD , ERROR)      !Name of the window of visualization


    !Allocation of simulations buffers:
    allocate(C(Nx + 2  , Ny + 2))                                  !   1 extra cell   |wall|      N cells      |wall| 1 extra cell
    allocate(tr(Nx + 2  , Ny + 2))


    !Simulation Routines:

     call Simu_routines()                                          !The protocolls are called for simulation development

    !Simulation Statistics:
    print*, " "
    print*, "------------------------------------------------------"
    print*, "-                                                    -"
    print*, "-                  Fim da simulação                  -"
    print*, "-                                                    -"
    print*, "------------------------------------------------------"

    !Dellocation of simulations buffers:
    deallocate(C)
    deallocate(tr)

end subroutine simulation





!Simulation center of routines:
subroutine Simu_routines()
    use global
    implicit none

    call initialconditions()                                       !Create the initial condition for the simulation

    call time_steps()                                              !Initiate time iterations

    if(save_image)then
        call save_this_image()                                     !Save image in non volitile memory.
    end if

end subroutine Simu_routines





!Set initial values for all the domain and simulation values
subroutine initialconditions()
    use mpi
    use global
    implicit none

    !Geometric topologicall determination (is wall ?)
    do i = 1 , Nx + 2
        do ii = 1 , Ny + 2

            if ( (i == 1 .or. ii == 1 .or. i == Nx + 2 ) .and. ii /= Ny + 2)then
                C(i , ii)%Wall = .True.
                C(i , ii)%type_Wall = 1
            else if(ii == Ny + 2)then
                C(i , ii)%Wall = .True.
                C(i , ii)%type_Wall = 2
            else
                C(i , ii)%Wall = .False.
            end if

        end do
    end do

    !Initial phisicall considerations:
    do i = 1 , Nx + 2
        do ii = 1 , Ny + 2

            if ( C(i , ii)%Wall .eqv. .False. )then
                !Determinations for all the physicall domain

                C(i , ii)%u = ui
                C(i , ii)%v = vi
                C(i , ii)%alpha = alpha
                C(i , ii)%nu = nu
                C(i , ii)%mi = mi
                C(i , ii)%rho = rho

            end if

            C(i , ii)%T = ti
            C(i , ii)%P = pi
            C(i , ii)%dx = dx
            C(i , ii)%dy = dy

        end do
    end do


    !Initial condition exibition:

    call MPI_SEND( DBLE(C%type_Wall)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call MPI_SEND( DBLE(C%P)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call MPI_SEND( DBLE(C%v)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call MPI_SEND( DBLE(C%u)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call MPI_SEND( DBLE(C%u)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call MPI_SEND( DBLE(C%v)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
    call sleep(1)

end subroutine initialconditions





subroutine time_steps()
    use global
    implicit none

    !initiate simulation loop
    step = 1
    do while(step * dt < time - dt)


        call boundary_conditions()                                 !Set boundary conditions
        call velocity_solver()                                     !Solve the velocity field
        call preassure_definition()                                !Simulate preassure of the next step
        call velocity_corrector()                                  !Correct the velocity with the preassure simulated
        call boundary_conditions()                                 !Set boundary conditions
        call divergent_calculator()                                !Calcule the divergent of the velocity domain
        call preassure_atualization()                              !Calculate the preassure of new step
        call temperature_atualization()                            !Calculate new temperature
        call boundary_conditions()                                 !Set boundary conditions

        if(mod(step , image_frequence) == 0.0)then
            call save_this_image()                                     !Save domain for paraview
        end if

        !At final of a step

        WRITE(*,101) char(13), MAXVAL(C%div) , "   " , pressure_step , velo_step
        101 FORMAT(1a1,ES7.1 , A , I5, I5, $)

        call MPI_SEND( DBLE(C%u)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        call MPI_SEND( DBLE(C%v)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        call MPI_SEND( DBLE(C%P)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        call MPI_SEND( DBLE(C%T)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        call MPI_SEND( DBLE(C%u)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        call MPI_SEND( DBLE(C%v)  , size(C%type_Wall) , MPI_DOUBLE_PRECISION , 1 , 1 , MPI_COMM_WORLD , ERROR)
        step = step + 1

    end do

end subroutine time_steps






subroutine boundary_conditions()
    use global
    implicit none
    !Boundary conditions
        do i = 1 , Ny + 2
            !Preassure on all the walss
            C(Nx + 2 , i)%P  =  C(Nx + 1, i)%P     !>
            C(1     , i)%P     =  C(2 , i)%P       !<
            C(Nx + 2 , i)%Pl  =  C(Nx + 1, i)%Pl     !>
            C(1     , i)%Pl     =  C(2 , i)%Pl       !<

            C(1     , i)%v = - C(2     , i)%v
            C(Nx+2   , i)%v = - C(Nx+1   , i)%v
            C(1     , i)%vl = - C(2     , i)%vl
            C(Nx+2   , i)%vl = - C(Nx+1   , i)%vl

            C(1     , i)%u = 0
            C(2     , i)%u = 0
            C(Nx+2   , i)%u = 0
            C(1     , i)%ul = 0
            C(2     , i)%ul = 0
            C(Nx+2   , i)%ul = 0

            C(1     , i)%T = ta
            C(Nx + 2, i)%T = tb
        end do



        do i = 1 , Nx + 2
            C(i , Ny + 2)%P =  C(i , Ny  + 1)%P
            C(i , 1    )%P =  C(i , 2     )%P
            C(i , Ny + 2)%Pl =  C(i , Ny  + 1)%Pl
            C(i , 1    )%Pl =  C(i , 2     )%Pl

            C(i , 1    )%v = 0
            C(i , 2    )%v = 0
            C(i , Ny + 2)%v = 0
            C(i , 1    )%vl = 0
            C(i , 2    )%vl = 0
            C(i , Ny + 2)%vl = 0

            C(i , 1    )%u = -C(i , 2     )%u
            C(i , Ny + 2)%u = 2 * V_top  - C(i , Ny + 1)%u
            C(i , 1    )%ul = -C(i , 2     )%ul
            C(i , Ny + 2)%ul = 2 * V_top - C(i , Ny + 1)%ul

            C(i , 1     )%T = C(i , 2     )%T
            C(i , Ny + 2)%T = C(i , Ny + 1)%T
        end do

end subroutine boundary_conditions





subroutine velocity_solver()
    use global
    implicit none

    if(what_velocity_simulation == 2)then
    ! IMPLICITY SOLVER
    !print*, "implicit velocity"

        !Initial value
        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i , ii)%ul = 0
                C(i , ii)%vl = 0

            end do
        end do

        velo_step = 0

        !Implicit velocity line creation
        C(3, 3)%div = 10
        C(3, 3)%divi = 10
        do while( MAXVAL(C%div) > increment .or. MAXVAL(C%divi) > increment)

            do i = 2 , Nx + 1
                do ii = 2 , Ny + 1

                    C(i, ii)%div = C(i, ii)%ul      !div é utilizado para se ver o incremento da velocidade
                    C(i, ii)%divi = C(i, ii)%vl     !divi é utilizado para se ver o incremento da velocidade

                end do
            end do

            do i = 3 , Nx + 1
                do ii = 2 , Ny + 1

                    C(i, ii)%ul = (C(i , ii)%u * (C(i , ii)%dx ** 2)*(C(i , ii)%dy ** 2))/((C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dy ** 2) + (C(i , ii)%dx ** 2) ) ) &
                        - ( (   &
                            ((C(i , ii)%v + C(i - 1 , ii)%v  + C(i , ii + 1)%v  + C(i - 1, ii + 1)%v )/4)&
                            * (C(i , ii)%dx ** 2) *(C(i , ii)%dy) * dt)/( 2 * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 4 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) ) * &
                        (C(i , ii + 1)%u - C(i , ii - 1)%u ) &
                        - ( (C(i , ii)%u * (C(i , ii)%dx) *(C(i , ii)%dy**2) * dt)/( 2 * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 4 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )  *&
                        (C(i + 1, ii)%u - C(i - 1 , ii)%u ) &
                        - ( ( (C(i , ii)%dx) *(C(i , ii)%dy**2) * dt)/( rho * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * rho * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i , ii)%P - C(i - 1, ii)%P) &
                        +( ( (C(i , ii)%dx ** 2) *(C(i , ii)%dy**2) * dt * (C(i,ii)%rho - rho) * gx)/( rho * &
                            (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * rho * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )&
                        + ( ( nu *(C(i , ii)%dy**2) * dt)/( (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i + 1, ii)%ul + C(i - 1, ii)%ul)&
                        + ( ( nu *(C(i , ii)%dx**2) * dt)/( (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i , ii + 1)%ul + C(i, ii - 1)%ul)
                end do
            end do

            do i = 2 , Nx + 1
                do ii = 3 , Ny + 1
                    C(i, ii)%vl = (C(i , ii)%v * (C(i , ii)%dx ** 2) *(C(i , ii)%dy ** 2))/((C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dy ** 2) + (C(i , ii)%dx ** 2) ) ) &
                        - ( (C(i , ii)%v * (C(i , ii)%dx ** 2) *(C(i , ii)%dy) * dt)/( 2 * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 4 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) ) * &
                        (C(i , ii + 1)%v - C(i , ii - 1)%v ) &
                        - ( ( &
                            (C(i , ii)%u + C(i , ii - 1)%u + C(i + 1, ii)%u + C(i + 1 , ii - 1)%u)/4 &
                                * (C(i , ii)%dx) *(C(i , ii)%dy**2) * dt)/( 2 * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 4 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )  *&
                        (C(i + 1, ii)%v - C(i - 1 , ii)%v ) &
                        - ( ( (C(i , ii)%dx**2) *(C(i , ii)%dy) * dt)/( rho * (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * rho * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i , ii)%P - C(i, ii - 1)%P) &
                        +( ( (C(i , ii)%dx ** 2) *(C(i , ii)%dy**2) * dt * (  dil * (C(i , ii)%T - ti) )* gy)/( rho * &
                            (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * rho * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )&
                        + ( ( nu *(C(i , ii)%dy**2) * dt)/( (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i + 1, ii)%vl + C(i - 1, ii)%vl)&
                        + ( ( nu *(C(i , ii)%dy**2) * dt)/( (C(i , ii)%dx ** 2) * &
                        (C(i , ii)%dy ** 2) + 2 * nu * dt *( (C(i , ii)%dx ** 2) + (C(i , ii)%dy ** 2) ) ) )*&
                        (C(i , ii + 1)%vl + C(i , ii - 1)%vl)

                end do
            end do

            call boundary_conditions()

            velo_step = velo_step + 1

            do i = 2 , Nx + 1
                do ii = 2 , Ny + 1

                    C(i, ii)%div = abs ( C(i, ii)%div - C(i, ii)%ul )
                    C(i, ii)%divi = abs ( C(i, ii)%divi - C(i, ii)%vl )

                end do
            end do

        end do

    else if(what_velocity_simulation == 1)then
    !EXPLICIT SOLVER
    !print*, "explicit velocity"

        do i = 3 , Nx + 1
            do ii = 2 , Ny + 1

                C(i , ii)%ul = C(i, ii)%u &
                    - C(i, ii)%u * ((dt)/(C(i,ii)%dx)) *(C(i+1, ii)%u - C(i, ii)%u) &
                    - ( C(i , ii)%v + C(i - 1 , ii)%v  + C(i , ii + 1)%v  + C(i - 1, ii + 1)%v )/4 &
                    * ((dt)/(C(i,ii)%dy)) *(C(i, ii + 1)%u - C(i, ii)%u)&
                    - ((dt)/(rho * C(i,ii)%dx )) * (C(i , ii)%P - C(i - 1 , ii)%P)&
                    + dt * gx * (C(i,ii)%rho - rho)/(rho) &
                    + (nu * dt / (C(i,ii)%dx ** 2) ) * (C(i+1, ii)%u - 2 * C(i , ii)%u + C(i-1, ii)%u) &
                    + (nu * dt / (C(i,ii)%dy ** 2) ) * (C(i, ii+ 1)%u - 2 * C(i , ii)%u + C(i, ii-1)%u)
            end do
        end do

        do i = 2 , Nx + 1
            do ii = 3 , Ny + 1
                C(i , ii)%vl = C(i, ii)%v &
                    - (C(i , ii)%u + C(i , ii - 1)%u + C(i + 1, ii)%u + C(i + 1 , ii - 1)%u)/4 &
                    * ((dt)/(C(i,ii)%dx)) *(C(i+1, ii)%v - C(i, ii)%v) &
                    - C(i, ii)%v * ((dt)/(C(i,ii)%dy)) *(C(i, ii + 1)%v - C(i, ii)%v) &
                    - ((dt)/(rho * C(i,ii)%dy)) * (C(i , ii)%P - C(i , ii - 1)%P)&
                    + dt * gy * (C(i , ii)%rho - rho)/(rho) &
                    + (nu * dt / (C(i , ii)%dx ** 2) ) * (C(i+1, ii)%v - 2 * C(i , ii)%v + C(i-1, ii)%v) &
                    + (nu * dt / (C(i , ii)%dy ** 2) ) * (C(i, ii+ 1)%v - 2 * C(i , ii)%v + C(i, ii-1)%v)

            end do
        end do

        call boundary_conditions()

    end if

end subroutine velocity_solver






subroutine preassure_definition()
    use global
    implicit none

    !Pressure initial gaus saidel value (zero)
    do i = 1 , Nx + 2
        do ii = 1 , Ny + 2

                C(i , ii)%Pl = 0

        end do
    end do
    pressure_step = 0

    !Implicit preassure line creation
    C(3, 3)%divi = 10
    do while( MAXVAL(C%divi) > increment)

        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i, ii)%divi = C(i, ii)%Pl

            end do
        end do
       C(Nx / 2, Ny / 2)%Pl = 0

        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i, ii)%Pl = -1 * ( (rho * C(i , ii)%dx * C(i , ii)%dy**2)/(2 * (C(i , ii)%dx**2 + C(i , ii)%dy**2) * dt )) &
                * ( C(i + 1, ii)%ul - C(i, ii)%ul) &
                -1 * ( (rho * C(i , ii)%dx**2 * C(i , ii)%dy)/(2 * (C(i , ii)%dx**2 + C(i , ii)%dy**2) * dt )) &
                * ( C(i, ii+ 1)%vl - C(i, ii)%vl) &
                + ( (C(i , ii)%dy**2)/ (2 * (C(i , ii)%dx**2 + C(i , ii)%dy**2)))*( C(i + 1, ii)%Pl + C(i - 1, ii)%Pl) &
                + ( (C(i , ii)%dx**2)/ (2 * (C(i , ii)%dx**2 + C(i , ii)%dy**2)))*( C(i, ii + 1)%Pl + C(i, ii - 1)%Pl)

            end do
        end do

        call boundary_conditions()

        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i, ii)%divi = abs ( C(i, ii)%divi - C(i, ii)%Pl )

            end do
        end do

        pressure_step = pressure_step + 1
    end do

end subroutine preassure_definition






subroutine velocity_corrector()
    use global
    implicit none

    do i = 3 , Nx + 1
        do ii = 2 , Ny + 1

                C(i , ii)%u = C(i , ii)%ul - (dt/(C(i , ii)%dx * rho)) * (C(i , ii)%Pl - C(i - 1 , ii)%Pl)

        end do
    end do


    do i = 2 , Nx + 1
        do ii = 3 , Ny + 1

                C(i , ii)%v =  C(i , ii)%vl - (dt/(C(i , ii)%dy * rho)) * (C(i , ii)%Pl - C(i , ii - 1)%Pl)

        end do
    end do

end subroutine velocity_corrector






subroutine divergent_calculator()
    use global
    implicit none

    do i = 2 , Nx + 1
        do ii = 2 , Ny + 1

            C(i , ii)%div = (C(i + 1 , ii)%u - C(i , ii)%u)/ C(i , ii)%dx + (C(i , ii + 1)%v - C(i, ii)%v)/ C(i , ii)%dy

        end do
    end do

end subroutine divergent_calculator







subroutine preassure_atualization()
    use global
    implicit none

    !Pressure atualization
    do i = 2 , Nx + 1
        do ii = 2 , Ny + 1

            C(i , ii)%P = C(i , ii)%Pl +  C(i , ii)%P

        end do
    end do

end subroutine preassure_atualization







subroutine temperature_atualization()
    use global
    implicit none


    !Explicit simulation for this step.

        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i , ii)%Ti = C(i , ii)%T * (1 - 4 * alpha * dt/(dx*dy)) &
                + C(i + 1, ii)%T * (alpha * dt / (dx**2) - C(i + 1 , ii)%u * dt/(2 * dx)) &
                + C(i , ii + 1)%T * (alpha * dt /(dy**2) - C(i , ii + 1)%v * dt / (2 * dy))&
                + C(i -1, ii)%T * ((alpha * dt) / (dx * dx) + C(i , ii)%u * dt /(2 * dx)) &
                + C(i, ii - 1)%T *((alpha * dt) /(dy * dy) + C(i , ii)%v * dt/(2 * dy))

            end do
        end do



        do i = 2 , Nx + 1
            do ii = 2 , Ny + 1

                C(i , ii)%T = C(i , ii)%Ti

            end do
        end do


        call boundary_conditions()


end subroutine temperature_atualization






subroutine save_this_image()
    use global
    use mpi
    implicit none

    call GETCWD(dirname)


    if(folder .eqv. .FALSE.)then
        !Creation of new folder for data saving
        write(misc,FMT=187)  Nx , "_" , Ny , "_" , Ta ,"_" , Tb , "_" , Ra
        187     format( "cavity_" , I3 , A , I3 , A , F6.1 , A , F6.1 , A , e7.1)
        call StripSpaces (misc)
        call system("cd results && mkdir " // misc)
        misc = trim(dirname) // "/results/" //  trim(misc)
        step_image = 0
        folder = .TRUE.
    end if

    ! Step file creation
    write(filename,FMT=186) trim(misc) , "/step" , step_image , ".dat"
    186     format(  A , A , I10 , A)
    call StripSpaces (filename)

    ! Open instance of write
    open(unit=10,file= filename)

    !Data write
    write(10,*) "TITLE = " , '"Cavity_simu"'
    write(10,*) 'Variables="X","Y","P","U","V","T","div"'
    write(10,*) 'Zone I=', Nx ,', J=', Ny ,', F=POINT'


    do i = 2 , Nx + 1
        do ii = 2 , Ny + 1

            write(10,*) (dx * i - 1.5 * dx) , (dy * ii - 1.5 * dy) , C(i , ii)%P , &
            (C(i + 1 , ii)%u + C(i , ii)%u)/2 , (C(i , ii + 1)%v + C(i , ii)%v)/2 , &
            C(i , ii)%T , C(i , ii)%div

        end do
    end do

    ! Close instance of write
    close(10)

    step_image = step_image + 1
    return

end subroutine save_this_image