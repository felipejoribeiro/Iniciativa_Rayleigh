!##################################################################################################################################
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! Code for the creation of the simulation native in fortran.                                                                     !#
! Version 1.0.0                                                                                                                  !#
! MPI directives are necessary! (install mpifor and mpirun)                                                                      !#
!##################################################################################################################################






!##################################################################################################################################
!Routine for the simulation                                                                                                      !#
subroutine Simulation()                                                                                                          !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#

    !Parameters of the simulation:
    Nx = 100                                                       !Space cells in x direction
    Ny = Nx                                                        !Space cells in y direction
    Lx = 1.d0                                                      !Size of space domain in x  (m)
    Ly = Lx                                                        !Size of space domain in y  (m)
    dx =  Lx / (Nx)                                                !Cells length in x (m)
    dy =  Ly / (Ny)                                                !Cells length in y (m)
    !Physical determination of fluid flow:
    alpha =  1.43d-7                                               !Thermal condutivity (water with 25 degree Celcius) (m**2/s)
    mi = 8.891d-4                                                  !viscosity (water with 25 degree Celcius) (n*s/m**2) (https://www.engineeringtoolbox.com/water-dynamic-kinematic-viscosity-d_596.html)
    rho = 8.2d0 !997.7d0                                           !Specific mass (water with 25 degree Celcius) (N/m**3)
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
    cfl = 0.25d0                                                   !Relation betwen time and space steps
    dt = (cfl * dx**2 )/ nu                                        !Time step length (s)
    time = 2500.0d0                                                !Total time of simulation (s)
    increment = 1.d-10                                             !Increment for implicity Gaus-Seidel solutions
    !Simulation Pannel control:
    save_image = .FALSE.                                           !Save file is wanted?
    image_frequence = 100                                          !In how many iterations the image must be saved?
    filename = "simulacao_piloto"                                  !Name of saved file
    Windows_name = "ProgramCavidade_4.0.0"                         !Name of the window of graphical representation
    what_thermal_simulation = 2                                    !Type of thermal numerical solution (1 = explicit / 2 = implicit)
    Exist_Thermal_simulation = .FALSE.                             !If there is thermal simulation, or isotermic hipotesis
    what_velocity_simulation = 2                                   !Type of velocity numerical solution (1 = explicit / 2 = implicit)


    !Print data from the present simulation
    logg = "inicio"
    call LOG(logg)                                                 !Function of comunication with the terminal

    !First comunication with Graphics
    logg = "inicio_send"
    call Graph(logg)                                               !Function of comunication with graphycall interface

    !Allocation of simulations buffers:
    allocate(C(Nx + 2  , Ny + 2))                                  !   1 extra cell   |wall|      N cells      |wall| 1 extra cell
    allocate(tr(Nx + 2  , Ny + 2))                                 !   1 extra cell   |wall|      N cells      |wall| 1 extra cell


    !Simulation Routines:
     call Simu_routines()                                          !The protocolls are called for simulation development


    !Dellocation of simulations buffers:
    deallocate(C)
    deallocate(tr)

    !Last LOG, for finish the routine
    logg = "fim"
    call LOG(logg)

    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine simulation                                                                                                        !#
!##################################################################################################################################






!##################################################################################################################################
!Simulation development, with DATA creation, saving and manipulation                                                             !#
subroutine Simu_routines()                                                                                                       !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#



    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine Simu_routines                                                                                                     !#
!##################################################################################################################################

