! Code for the creation of visualization native in fortran.
! Version 1.0.0

! Main module for global variables
module graphics
    !OPENGL libraries
    use, intrinsic :: ISO_C_BINDING
    use mpi
    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none

    !Parameters for graphical global variables:
    integer :: window ,  xpixel , ypixel                       !window id , size of window
    real , dimension(:,:), allocatable :: dBuffer              !Screen data buffer
    character*200 :: windows_name                              !Name of the window
    integer:: ERROR , status(MPI_STATUS_SIZE)                  !MPI stuf
    integer:: i , ii , iii                                     !Integer counters
    integer:: Nx, Ny                                           !Simulation buffer size




    !Functions for dysplaing data
    contains

        !Function that is called once per frame.
        subroutine idle() bind(C)

            !call assimilation()           !Function that get values
            !call scatter()                !Render heatmap
            !call Legend()                 !Render Legend

        end subroutine idle

end module graphics







! Rotine of creation, configuration and data assimilation of simulation results
subroutine Visualization()
    use graphics
    implicit none

    !Initial comunication with simulation
    call MPI_RECV( Nx , 1 , MPI_INTEGER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)                  !Size of simulation data buffer in x
    call MPI_RECV( Ny , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)                  !Size of simulation data buffer in y
    call MPI_RECV( Windows_name , 200 , MPI_CHARACTER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)    !Name of the window of visualization

    !Initial parametrization determinations
    ypixel = 600                                     !vertical size of window in pixels
    xpixel = int(ypixel * real(Nx)/real(Ny))         !horizontal size of window in pixels

    !Main visualization subroutines
    call NewWindow()         !Window creation
    call ConfigureWindow()   !Window configuration
    call glutmainloop()      !Enter IDLE protocol


end subroutine Visualization



!Subroutine to create a window
subroutine NewWindow()
  use graphics
  implicit none

  call glutinit()
  call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)              !Exibicion mode
  call glutInitWindowSize(xpixel, ypixel)                                !Determine window width in pixels
  window = glutcreatewindow(trim(Windows_name))                          !A name is given to the window

  call glutIdleFunc(idle)                                                !Repetition function
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