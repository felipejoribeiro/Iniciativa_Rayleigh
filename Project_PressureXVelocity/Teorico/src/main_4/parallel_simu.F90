!##################################################################################################################################
! Undergraduate: Felipe J. O. Ribeiro                                                                                            !#
! Professor: Aristeu da Silveira Neto                                                                                            !#
! Institution: MfLab - FEMEC - UFU                                                                                               !#
! Code for paralelization of the physical simulation                                                                             !#
! Version 1.0.0                                                                                                                  !#
! MPI directives are necessary! (install mpifor and mpirun)                                                                      !#
!##################################################################################################################################





!##################################################################################################################################
!Parallel solver for physical domains                                                                                            !#
subroutine Simu_parallel()                                                                                                       !#
    use global                                                                                                                   !#
    implicit none                                                                                                                !#


    print*, "slave called!!"


    return                                                                                                                       !#
                                                                                                                                 !#
end subroutine Simu_parallel                                                                                                     !#
!##################################################################################################################################