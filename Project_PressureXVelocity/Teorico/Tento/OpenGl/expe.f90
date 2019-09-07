! módulo com as funções de OPEN GL
module plot

  ! Uma questão de bibliotecas
  use, intrinsic :: ISO_C_BINDING
  use opengl_gl
  use opengl_glu
  use opengl_glut


  implicit none
  ! Parametros para o programa
  integer :: window, frame = 0                     ! Identificador de janela, um integer
  double precision :: tempo , tempo2               ! Tempo total de processamento.
  double precision :: geometric1,   geometric2                  ! geometria qualquer
  real , dimension(200,200):: dBuffer              ! dados para mostrar na tela

  public :: display1, display2                     ! Objeto que refere-se a janela.

  contains

    subroutine display1() bind(C)

      use opengl_gl
      use opengl_glu
      use opengl_glut

      real, target :: P1(8)=[0.0,0.45, .75,.45, .75,.75, .45,.75]
      real, target :: P2(8)=[0.2, 0.2, .5,.2, .5,.5, .2,.5]

      !Limpa a tela
      call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)

      !Desenha o retângulo
      call glcolor3f(1.0, 0.5, 0.5)                        !cor do polígono
      call glVertexPointer( 2, GL_FLOAT, 0, c_loc(P1) )    !(dimensões de vertice, type, bytes de offset entre vertices, vetor alvo)
      call glDrawArrays(GL_POLYGON, 0, 4)                  !(primitiva alvo, vértice de início da contagem, número de vértices)

      !Desenha outro retângulo
      call glcolor3f(0.0, 0.8, 0.8)                        !cor do poligono
      call glVertexPointer( 2, GL_FLOAT, 0, c_loc(P2) )    !(dimensões de vertice, type, bytes de offset entre vertices, vetor alvo)
      call glDrawArrays(GL_POLYGON, 0, 4)                  !(primitiva alvo, vértice de início da contagem, número de vértices)


      !processar processos de OPENGL compilados
      call glflush()

    end subroutine display1


    subroutine display2() bind(C)

      use opengl_gl
      use opengl_glu
      use opengl_glut

      integer :: i , ii
      real, target :: P1(8)=[0.45,0.45, .75,.45, .75,.75, .45,.75]
      real, target :: P2(8)=[0.2, 0.2, .5,.2, .5,.5, .2,.5]

      !Limpa a tela
      call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)

      !Desenha o retângulo
      ! call glcolor3f(1.0, 0.5, 0.5)                        !cor do polígono
      ! call glVertexPointer( 2, GL_FLOAT, 0, c_loc(P1) )    !(dimensões de vertice, type, bytes de offset entre vertices, vetor alvo)
      ! call glDrawArrays(GL_POLYGON, 0, 4)                  !(primitiva alvo, vértice de início da contagem, número de vértices)

      !Desenha outro retângulo (de outra forma)
      ! call glcolor3f(0.0, 0.8, 0.8)                        !cor do poligono
      ! call glBegin(GL_POLYGON)
      !   call glVertex2f(real(geometric1), P2(2))
      !   call glVertex2f(P2(3), P2(4))
      !   call glVertex2f(P2(5), P2(6))
      !   call glVertex2f(P2(7), P2(8))
      ! call glEnd()

      !Outros tipos de primitivas: GL_POINTS, GL_LINES, GL_TRIANGLES, GL_LINE_STRIP, GL_LINE_LOOP...

      ! Desenha pontos

        call glBlendFunc(GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA)
        call glPointSize(4.0)

      do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
          call glcolor3f(dBuffer(i , ii), dBuffer(i , ii), 0.9)
          call glBegin(GL_POINTS)
            call glVertex2f( (real(i) - 1)/real(size(dBuffer(: , 1))) , (real(ii)- 1)/real(size(dBuffer(1 , :))) )
          call glEnd()
        end do
      end do



      !processar processos de OPENGL compilados
      call glflush()

    end subroutine display2


    subroutine idle() bind(C)
      use opengl_gl
      use opengl_glu
      use opengl_glut
      frame = frame + 1
      CALL CPU_TIME(tempo)


      call simulation()




      if( mod(frame , 5) == 0)then
        print*, frame/tempo
        call display2()
      end if
    end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine simulation
      integer:: i , ii


      do i = 1 , size(dBuffer(:, 1))
        do ii = 1 , size(dBuffer(1, :))
          dBuffer(i , ii) = 0.5
        end do
      end do

    end subroutine simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module plot





program main

  ! Importa as capacidades openGL
  use plot

  implicit none
  Character(len = 20) :: u

  !criação de janela junto de modo de exibição
  call criarjanela()

  !seta configurações de visualização
  call configurajanela()

  !chamado para finalizar os processamentos
  call glutmainloop()

end program



subroutine criarjanela()
  use plot
  implicit none
  integer :: xpixel , ypixel

  xpixel = 400
  ypixel = 400

  call glutinit()
  call glutinitdisplaymode(GLUT_SINGLE+GLUT_RGB+GLUT_DEPTH)              !modos de exibição
  call glutInitWindowSize(xpixel, ypixel)                                !determina tamanho de janela em pixels
  window = glutcreatewindow("Nome da janela")                            !Cria-se janela com esse nome
  call glutIdleFunc(idle)                                                !Função de repetĩção
  return

end subroutine criarjanela


subroutine configurajanela()
  use plot
  implicit none

  call glclearcolor(0.0, 1.0, 1.0, 0.0)                                  !seta cor no background.
  call glmatrixmode(GL_PROJECTION)                                       !seta o contexto de edição das matrizes.
  call glloadidentity()                                                  !carega a matriz identidade.
  call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !(limite esquerda, limite direita, limite inferior, limite superior, corte de procimidade com a camera, corte de distância com a câmera)
  call glEnableClientState(GL_VERTEX_ARRAY)                              !destrava funcionalidades

end subroutine











