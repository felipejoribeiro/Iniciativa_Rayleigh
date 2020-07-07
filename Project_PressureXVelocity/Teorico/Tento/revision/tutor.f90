! Program to solve LAPLACE EQUATION OVER A DOMAIN
! D2T = 0
! 0 <= x <= 1 , 0 <= y <= 1
! with boundary conditions phi = xy
!##########################################################

program tutor 
    implicit none
    integer, parameter :: nx = 25 , ny = 25
    integer:: i, j, iterations
    real:: x , y , z , dx , dy , omega
    real, dimension(1:nx , 1:ny) :: AE, AW, AN, AS, AP, SP, SU
    real, dimension(0:nx+1 , 0:ny+1):: phi

    !###########################################################


    dx = 1./float(nx); dy = 1./float(ny)

    !INICIALIZA AS MATRIZES SU E SP COM TUDO IGUAL A ZERO

    SU = 0; SP = 0

    phi = 0.0

    !INICIALIZA AE , AW , AN, AS , AP nas células

    !para valores fora da fronteira
    AE = 1. ; AW = 1. ; AN = 1. ; AS = 1. 
    !Valores de primeria parede (W)
    AW(1,:) = 0.
    SU(1,:) = 0.
    SP(1,:) = -2.
    !Valores da segunda parede (e)
    AE(Nx,:) = 0.
    SP(NX,:) = -2. 

    do j = 1 , NY
        SU(NX , j) = 2. *( (dy/2.) + dy * float(j - 1) )
    end do

    print*, "estoiu vivo"

    !############################################################






end program tutor
