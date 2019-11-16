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
    integer :: window ,  xpixel , ypixel                                                   !window id , size of window
    double precision , dimension(:,:), allocatable :: dBuffer,dBuffer1,dBuffer2,dBuffer3   !Screen data buffer
    character*200 :: windows_name                                                          !Name of the window
    integer:: ERROR , status(MPI_STATUS_SIZE)                                              !MPI stuf
    integer:: i , ii , iii                                                                 !Integer counters
    integer:: Nx, Ny                                                                       !Simulation buffer size
    integer:: Type_of_visualization                                                        !Type of graphics




    !Functions for dysplaing data
    contains

        !Function that is called once per frame. Type 1
        subroutine idle1() bind(C)

            call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
            call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)             !Clear screen for new frame
            Allocate(dBuffer(Nx + 2 , Ny + 2))
            call assimilation()           !Function that get values
            call scatter1()                !Render heatmap
            call Legend()                 !Render Legend
            deallocate(dBuffer)
            call glutSwapBuffers()       !send new buffer of collors simutaneusly for rendering, prevent tearing
            call glflush()               !Process OPENGL precompiled codes

        end subroutine idle1


        !Function that is called once per frame.Type 2
        subroutine idle2() bind(C)


            call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
            call glclear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)             !Clear screen for new frame
            Allocate(dBuffer(Nx + 2 , Ny + 2))
            call scatter2()                !Render heatmap
            call Legend()                 !Render Legend
            deallocate(dBuffer)
            call glutSwapBuffers()       !send new buffer of collors simutaneusly for rendering, prevent tearing
            call glflush()               !Process OPENGL precompiled codes

        end subroutine idle2


        !Function to text drawning
        subroutine output(x, y, text)

            real(glfloat) x,y
            character(len=*) text
            integer(glcint) p

            call glRasterPos2i(50, 60)
            call glPushMatrix()
            call glTranslatef(x, y, 0.0_glfloat)
            do i=1,len(text)
                p = ichar(text(i:i))
                call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, p)
            end do
            call glPopMatrix()
        end subroutine output

end module graphics







! Rotine of creation, configuration and data assimilation of simulation results
subroutine Visualization()
    use graphics
    implicit none

    !Initial comunication with simulation
    call MPI_RECV( Nx , 1 , MPI_INTEGER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in x
    call MPI_RECV( Ny , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)                          !Size of simulation data buffer in y
    call MPI_RECV( Windows_name , 200 , MPI_CHARACTER , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)            !Name of the window of visualization
    call MPI_RECV( Type_of_visualization , 1 , MPI_INTEGER , 0 , 0 , MPI_COMM_WORLD , STATUS  , ERROR)       !Type of visualization

    !Initial parametrization determinations
    ypixel = 300                                             !vertical size of window in pixels

    select case (Type_of_visualization)
        case(1)
            xpixel = int(ypixel * real(Nx + 2)/real(Ny + 2))                      !horizontal size of window in pixels for type 1
        case(2)
            xpixel = real(4.25) * int(ypixel * real(Nx + 2)/real(Ny + 2))         !horizontal size of window in pixels for type 2
    end select

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

    select case(Type_of_visualization)                                     !Select type of visualization (single of multiple)
        case(1)
            call glutIdleFunc(idle1)                                       !single window
        case(2)
            call glutIdleFunc(idle2)                                       !multi window
    end select

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
    call glEnable(GL_BLEND)                                                !Blend
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)                 !Blend function
    call glEnable(GL_LINE_SMOOTH)                                          !Smooth edges

end subroutine ConfigureWindow



!Assimilation of DATA from simulation routine
subroutine assimilation()
    use graphics
    implicit none

    !Allocate allocatables
    call MPI_RECV( dBuffer , size(dBuffer) , MPI_DOUBLE_PRECISION , 0 , 1 , MPI_COMM_WORLD , STATUS  , ERROR)                  !DATA from simulation routine

end subroutine assimilation


!Heatmap rendering engine
subroutine scatter1()
    use graphics
    implicit none

    !Draw points:
    call glBlendFunc(GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA)
    call glPointSize( 1 +  real( ypixel/(Ny + 2) ) )                       !Size of points



    !Drawing points in the screen
    call glBegin(GL_POINTS)
    do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
            if(dBuffer(i , ii) < 0)then
                call glcolor3f(0.1, 1.0 - real(abs(dBuffer(i , ii)))/MAXVAL(real(abs(dBuffer(:,:)))), 0.1 + &
                    abs(real((dBuffer(i , ii))/MAXVAL(real(abs(dBuffer(:,:)))))))
                call glVertex2f( real(0.05) + real(0.9)*(real(i-0.5))/real(size(dBuffer(:,1))), &
                            real(0.05) + real(0.9)*(real(ii - 0.5))/real(size(dBuffer(1 , :))) )
            else
                call glcolor3f(real(dBuffer(i , ii)/MAXVAL(real(abs(dBuffer(:,:)))) )+ 0.1, 1 - real(dBuffer(i , ii)/&
                        MAXVAL(real(abs(dBuffer(:,:))))), 0.1)
                call glVertex2f( real(0.05) + real(0.9)*(real(i-0.5))/real(size(dBuffer(:,1))), &
                            real(0.05) + real(0.9)*(real(ii - 0.5))/real(size(dBuffer(1 , :))) )
            end if
        end do
    end do
    call glEnd()


end subroutine scatter1


!Heatmap rendering engine
subroutine scatter2()
    use graphics
    implicit none

    !Draw points:
    call glBlendFunc(GL_DST_ALPHA,GL_ONE_MINUS_DST_ALPHA)
    call glPointSize( real(1 +  ypixel/(Ny + 2) ) )                       !Size of points

    call assimilation()           !Function that get values

    !Drawing points in the screen 1
    call glBegin(GL_POINTS)
    do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
            if(dBuffer(i , ii) < 0)then
                call glcolor3f(0.1, 1.0 - real(abs(dBuffer(i , ii)))/MAXVAL(real(abs(dBuffer(:,:)))), 0.1 + &
                    abs(real((dBuffer(i , ii))/MAXVAL(real(abs(dBuffer(:,:)))))))
            else
                call glcolor3f(real(dBuffer(i , ii)/MAXVAL(real(abs(dBuffer(:,:)))) )+ 0.1, 1 - real(dBuffer(i , ii)/&
                        MAXVAL(real(abs(dBuffer(:,:))))), 0.1)
            end if

            call glVertex2f( real(0.0117647059) + real(0.2352941176) * (real(i))/real(size(dBuffer(:,1))), &
                            real(0.05)      + real(0.9) * (real(ii))/real(size(dBuffer(1,:)))  )
        end do
    end do
    call glEnd()

    call assimilation()           !Function that get values

    !Drawing points in the screen 2
    call glBegin(GL_POINTS)
    do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
            if(dBuffer(i , ii) < 0)then
                call glcolor3f(0.1, 1.0 - real(abs(dBuffer(i , ii)))/MAXVAL(real(abs(dBuffer(:,:)))), 0.1 + &
                    abs(real((dBuffer(i , ii))/MAXVAL(real(abs(dBuffer(:,:)))))))
            else
                call glcolor3f(real(dBuffer(i , ii)/MAXVAL(real(abs(dBuffer(:,:)))) )+ 0.1, 1 - real(dBuffer(i , ii)/&
                        MAXVAL(real(abs(dBuffer(:,:))))), 0.1)
            end if

            call glVertex2f( real(0.2588235294) + real(0.2352941176) * (real(i))/real(size(dBuffer(:,1))), &
                            real(0.05)      + real(0.9) * (real(ii))/real(size(dBuffer(1,:)))  )
        end do
    end do
    call glEnd()

    call assimilation()           !Function that get values

    !Drawing points in the screen 3
    call glBegin(GL_POINTS)
    do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
            if(dBuffer(i , ii) < 0)then
                call glcolor3f(0.1, 1.0 - real(abs(dBuffer(i , ii)))/MAXVAL(real(abs(dBuffer(:,:)))), 0.1 + &
                    abs(real((dBuffer(i , ii))/MAXVAL(real(abs(dBuffer(:,:)))))))
            else
                call glcolor3f(real(dBuffer(i , ii)/MAXVAL(real(abs(dBuffer(:,:)))) )+ 0.1, 1 - real(dBuffer(i , ii)/&
                        MAXVAL(real(abs(dBuffer(:,:))))), 0.1)
            end if

            call glVertex2f( real(0.5058823529) + real(0.2352941176) * (real(i))/real(size(dBuffer(:,1))), &
                            real(0.05)      + real(0.9) * (real(ii))/real(size(dBuffer(1,:)))  )
        end do
    end do
    call glEnd()

    call assimilation()           !Function that get values

    !Drawing points in the screen 4
    call glBegin(GL_POINTS)
    do i = 1 , size(dBuffer(: , 1))
        do ii = 1 , size(dBuffer(1 , :))
            if(dBuffer(i , ii) < 0)then
                call glcolor3f(0.1, 1.0 - real(abs(dBuffer(i , ii)))/MAXVAL(real(abs(dBuffer(:,:)))), 0.1 + &
                    abs(real((dBuffer(i , ii))/MAXVAL(real(abs(dBuffer(:,:)))))))
            else
                call glcolor3f(real(dBuffer(i , ii)/MAXVAL(real(abs(dBuffer(:,:)))) )+ 0.1, 1 - real(dBuffer(i , ii)/&
                        MAXVAL(real(abs(dBuffer(:,:))))), 0.1)
            end if

            call glVertex2f( real(0.7529411765) + real(0.2352941176) * (real(i))/real(size(dBuffer(:,1))), &
                            real(0.05)      + real(0.9) * (real(ii))/real(size(dBuffer(1,:)))  )
        end do
    end do
    call glEnd()



end subroutine scatter2





!Rotine that DRAWS the Legend on the screen
subroutine legend() bind(C)
    use graphics

  call glPushAttrib(GL_ENABLE_BIT)
  call glcolor3f(0.0, 0.0 , 0.0)
  call glMatrixMode(GL_PROJECTION)
  call glPointSize( 0.2 )                       !Size of points
  call glLineWidth(0.1)
  call glLoadIdentity()
  call gluOrtho2D(0.0_gldouble, 1500.0_gldouble, 0.0_gldouble, 1500.0_gldouble * ypixel/xpixel)
  call glscalef(.11,.11,.11)
  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()
!  Rotate text slightly to help show jaggies
  call output(100.0, 60.0, "Aqui temos texto escrito na tela, quem diria algo assim aqui!")
  call glPopMatrix()
  call glMatrixMode(GL_PROJECTION)
  call glPopMatrix()
    call glscalef(1.0/0.11,1.0/0.11,1.0/0.11)
  call glPopAttrib()


  call glloadidentity()                                                  !Identity matrix loaded
  call glortho(0.0d0, 1.0d0, 0.0d0, 1.0d0, -0.5d0, 0.5d0)                !Limits (left, right, down, up, close to the camera, far from the camera)
  call glEnableClientState(GL_VERTEX_ARRAY)                              !Funcionalities




end subroutine legend
