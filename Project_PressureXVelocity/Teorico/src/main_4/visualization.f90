!##################################################################################################################################
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! Code for the creation of visualization native in fortran.                                                                      !#
! Version 2.0.0                                                                                                                  !#
! OPENGL directives are necessary! (install glut and opengl)                                                                     !#
!##################################################################################################################################





!##################################################################################################################################
! Main module for global variables                                                                                               !#
module graphics                                                                                                                  !#
    !OPENGL libraries                                                                                                            !#
    use, intrinsic :: ISO_C_BINDING
    use mpi
    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none

    !Parameters for graphical global variables:
    integer :: window ,  xpixel , ypixel                                                   !window id , size of window
    double precision , dimension(:,:), allocatable :: dBuffer,dBuffer1,dBuffer2,dBuffer3   !Screen data buffer
    character*200 :: windows_name                                                          !Name of the window
    integer:: ERROR , status(MPI_STATUS_SIZE)                                              !MPI stuf
    integer:: i , ii , iii                                                                 !Integer counters
    integer:: Nx, Ny                                                                       !Simulation buffer size
    integer:: Type_of_visualization                                                        !Type of graphics
                                                                                                                                 !#
end module graphics                                                                                                              !#
!##################################################################################################################################







!##################################################################################################################################
! Rotine of creation, configuration and data assimilation of simulation results                                                  !#
subroutine Visualization()                                                                                                       !#
    use graphics                                                                                                                 !#
    implicit none                                                                                                                !#

    !Initial comunication with simulation
    call MPI_RECV( Nx , 1 , MPI_INTEGER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in x
    call MPI_RECV( Ny , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in y
    call MPI_RECV( Windows_name , 200 , MPI_CHARACTER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)            !Name of the window of visualization
    call MPI_RECV( Type_of_visualization , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)       !Type of visualization
















                                                                                                                                 !#
end subroutine Visualization                                                                                                     !#
!##################################################################################################################################