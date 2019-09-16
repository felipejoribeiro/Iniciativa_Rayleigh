program simple
    ! Uma questão de bibliotecas
    use, intrinsic :: ISO_C_BINDING
    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none
    ! Parametros para o programa
    integer :: window, frame = 0                     ! Identificador de janela, um integer
    integer :: i , ii
    double precision :: tempo                        ! Tempo total de processamento.
    real , dimension(200,200):: dBuffer              ! dados para mostrar na tela


end program simple



subroutine display() bind(C)
        use, intrinsic :: ISO_C_BINDING
        use opengl_gl
        use opengl_glu
        use opengl_glut

        real , dimension(200,200):: dBuffer              ! dados para mostrar na tela
        !Limpa a tela
        call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)
        call glBlendFunc(GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA)

        !Desenhando pontos HEAT MAP

        call glPointSize(1.5)
        do i = size(dBuffer(: , 1)) , 1 , -1
            do ii = size(dBuffer(1 , :)) , 1 , -1
                call glcolor3f(dBuffer(i , ii), dBuffer(i , ii), 0.9)
                call glBegin(GL_POINTS)
                call glVertex2f( (real(i) - 1)/real(size(dBuffer(: , 1))) , (real(ii)- 1)/real(size(dBuffer(1 , :))) )
                call glEnd()
            end do
        end do

        ! Troca buffers de visualização
        call glutSwapBuffers()
        ! Processar processos de OPENGL compilados
        call glflush()

    end subroutine display