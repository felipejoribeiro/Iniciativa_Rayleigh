module geral
use mpi
    double precision :: tempo
    integer:: i , ii , ERROR , status(MPI_STATUS_SIZE)
    logical:: FLAG


end module




program piloto

    use mpi
    implicit none
    integer ierr , numprocs, procid

    call MPI_INIT(ierr)

    call MPI_COMM_RANK(MPI_COMM_WORLD, procid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)


    if(procid == 0)then
        call processo0()
    elseif(procid == 1) then
        call processo1()
    elseif(procid == 2) then
        call processo2()
    end if

    call MPI_FINALIZE(ierr)

end program


subroutine processo0
    use geral
    implicit none
    tempo = 10.4
    print*, "processo 0 chamado"
    call MPI_SEND(tempo , 1 , MPI_DOUBLE_PRECISION ,1, 0 , MPI_COMM_WORLD , ERROR)
    print*, " 0 enviou para 1"

end subroutine

subroutine processo1
    use geral
    implicit none
    print*, "processo 1 chamado"
    call sleep(3)
    call MPI_iprobe( MPI_ANY_SOURCE, MPI_ANY_TAG , MPI_COMM_WORLD , FLAG , STATUS , ERROR);
    print*, FLAG
    call MPI_RECV(tempo , 1 , MPI_DOUBLE_PRECISION , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)
    print*, " 1 recebeu de 0"
    call MPI_SEND(tempo , 1 , MPI_DOUBLE_PRECISION ,2, 0 , MPI_COMM_WORLD , ERROR)
    print*, " 1 enviou para 2"
    call MPI_SEND(tempo , 1 , MPI_DOUBLE_PRECISION ,2, 0 , MPI_COMM_WORLD , ERROR)

end subroutine

subroutine processo2
    use geral
    implicit none
    print*, "processo 2 chamado"
    call MPI_RECV(tempo , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)
    print*, " 2 recebeu de 1"
    print*, FLAG
    call MPI_RECV(tempo , 1 , MPI_DOUBLE_PRECISION , 1 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)
    print*, tempo

end subroutine



