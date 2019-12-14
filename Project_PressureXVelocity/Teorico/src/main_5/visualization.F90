!##################################################################################################################################
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! Code for the creation of visualization native in fortran.                                                                      !#
! Version 3.0.0                                                                                                                  !#
! OPENGL directives are necessary! (install glut and opengl)                                                                     !#
!##################################################################################################################################





!##################################################################################################################################
! Rotine of creation, configuration and data assimilation of simulation results                                                  !#
subroutine Visualization()                                                                                                       !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#

    !First comunication with Simulation
    logg = "inicio_receive"
    call Graph(logg)                                                !Function of comunication
                                                                    ! > Nx , Ny , Windows_name

    !Main visualization subroutines
    call NewWindow()         !Window creation
    call ConfigureWindow()   !Window configuration
    call glutmainloop()      !Enter IDLE protocol

    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine Visualization                                                                                                     !#
!##################################################################################################################################






!##################################################################################################################################
!Subroutine to create a window                                                                                                   !#
subroutine NewWindow()                                                                                                           !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#

    !Initial parametrization determinations
    ypixel = 600                                                           !vertical size of window in pixels
    xpixel = int(ypixel * real(Nx + 2)/real(Ny + 2))                       !horizontal size of window in pixels

    !Windows creation
    call glutinit()
    call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)              !Exibicion mode
    call glutInitWindowSize(xpixel, ypixel)                                !Determine window width in pixels
    window = glutcreatewindow(trim(Windows_name))                          !A name is given to the window

    call glutIdleFunc(idle)                                                !Function called during the routine continuasly.

    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine NewWindow                                                                                                         !#
!##################################################################################################################################







!##################################################################################################################################
! Configuring the window                                                                                                         !#
subroutine ConfigureWindow()                                                                                                     !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#

    call glclearcolor(0.0, 0.0, 0.0, 0.0)                                  !seta cor no background.
    call glmatrixmode(GL_PROJECTION)                                       !Matrix context of operations
    call glloadidentity()                                                  !Identity matrix loaded
    call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !Limits (left, right, down, up, close to the camera, far from the camera)
    call glEnableClientState(GL_VERTEX_ARRAY)                              !Funcionalities
    call glEnable(GL_BLEND)                                                !Blend
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)                 !Blend function
    call glEnable(GL_LINE_SMOOTH)                                          !Smooth edges


    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine ConfigureWindow                                                                                                   !#
!##################################################################################################################################





!##################################################################################################################################
! What is drawed and observed trhowout the frame                                                                                 !#
subroutine RENDERING_ENGINE()                                                                                                    !#
    use global                                                                                                                   !#





    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine RENDERING_ENGINE                                                                                                  !#
!##################################################################################################################################