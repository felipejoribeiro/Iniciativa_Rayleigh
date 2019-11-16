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