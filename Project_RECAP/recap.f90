! Simulation of temperature in an constant velocity regim, but is advection already.

program recap

    implicit none
    INTEGER :: i , ii, N 
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: e , T
    print*, "Program recap: Routine of termal simplifyed simulation"


    !Variables declaration
    N = 100                                   !number of space cells 



    !Allocation of allocatables
    allocate(T(N,N))


    ! Initial temperature condition
    do i = 1 , N 
        do ii = 1 , N

            T(i , ii) = 0
        end do
    end do


    ! Initiating the explicity simulations

    







    read(*,*)
end program recap