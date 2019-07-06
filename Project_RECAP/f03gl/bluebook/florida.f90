!
! Florida.cpp
! OpenGL SuperBible
! Demonstrates polygon tesselation
! Original program by Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   enum, bind(C) 
     enumerator :: DRAW_LOOPS = 1, DRAW_CONCAVE, DRAW_COMPLEX
   end enum
   
   integer :: Method = DRAW_LOOPS
  
   integer, parameter :: COAST_POINTS = 24
   integer, parameter :: LAKE_POINTS  =  4
   
   double precision, dimension(3), target :: dummy = 1.0

   double precision, dimension(3,COAST_POINTS), target :: vCoastCstyle
   
   double precision, dimension(COAST_POINTS,3), target :: vCoast 
   data vCoast / &
   -70., -50., -50.,  -5.,   0.,   8.,  12.,  10.,  15.,  20.,  20.,  10., & !x
     0.,  -5., -12., -13., -12., -20., -30., -40., -50., -55., -60., -70., & !
    30.,  30.,  27.,  27.,  20.,  10.,   5.,   0., -10., -20., -35., -40., & !y
   -30., -20., -10.,  -5.,   5.,  10.,  20.,  15.,  15.,  20.,  25.,  25., & !
     0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0., & !z
     0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.  / !

   double precision, dimension(3,LAKE_POINTS), target :: vLakeCstyle

   double precision, dimension(LAKE_POINTS,3) , target:: vLake 
  !real, dimension(LAKE_POINTS,3) , target:: vLake 
   data vLake / &
      10.0,  15.0,  10.0,   5.0, &
     -20.0, -25.0, -30.0, -25.0, &
       0.0,   0.0,   0.0,   0.0  / 
   
   type(C_PTR) :: pTess
 
end module spheremod
program florida

   use opengl_gl
   use opengl_glut

   use spheremod
   
   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ProcessMenu(i) bind(C)
       integer, intent(in), value :: i
     end subroutine ProcessMenu

     subroutine ChangeSize(w,h) bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     subroutine KeySpecialFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLint), intent(in), value  :: key, x, y
     end subroutine KeySpecialFunc
   end interface

   do i=1,COAST_POINTS
     vCoastCstyle(1,i) = vCoast(i,1)
     vCoastCstyle(2,i) = vCoast(i,2)
     vCoastCstyle(3,i) = vCoast(i,3)
   end do

   do i=1,LAKE_POINTS
     vLakeCstyle(1,i) = vLake(i,1)
     vLakeCstyle(2,i) = vLake(i,2)
     vLakeCstyle(3,i) = vLake(i,3)
   end do
   
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,GLUT_RGB))
   call glutInitWindowSize(500, 400)
   iwin = glutCreateWindow('Tesselated Florida'//char(0))

   ! Create the Menu
   im = glutCreateMenu(ProcessMenu)
   call glutAddMenuEntry('Line loops'//char(0),DRAW_LOOPS)
   call glutAddMenuEntry('Concave Polygon'//char(0),DRAW_CONCAVE)
   call glutAddMenuEntry('Complex Polygon'//char(0),DRAW_COMPLEX)
   call glutAttachMenu(GLUT_RIGHT_BUTTON)

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call setuprc
   
   call glutMainLoop

end program
subroutine ProcessMenu(in) bind(C)
	
   use opengl_glut

   use spheremod
   
   integer, intent(in), value :: in

   Method = in
      
   call glutPostRedisplay
	
end subroutine ProcessMenu
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine TessErrorHandler(i) BIND(C,NAME='TessErrorHandler')

   use opengl_gl
   use opengl_glu
   use opengl_glut
   
   interface ! strlen is a standard C function from <string.h>
      ! int strlen(char *string)
      function strlen(string) result(len) bind(C,name='strlen')
   	 use iso_c_binding
   	 type(c_ptr), value :: string ! a C pointer
      end function
   end interface   

   integer(kind=GLint), intent(in), value  :: i
   
   type(C_PTR) :: c_pointer                      ! start of C char string
   character(kind=C_CHAR), pointer :: f_pointer  ! start of f95 char string
   character(len=64) :: cMessage                 ! place holder

   c_pointer = gluErrorString(i)                 ! get the C string
   len = strlen(c_pointer)                       ! find char(0) (=length)
   call c_f_pointer(c_pointer, f_pointer)        ! turn into something usable
          
   cmessage = 'TESS error occured: '//f_pointer(1:len)
   
   write(*,*) cmessage
   ! Display the message to the user
   call glutSetWindowTitle(cMessage(1:lens(cMessage))//char(0))
   
end subroutine TessErrorHandler 
subroutine RenderScene() bind(C)
 
   use opengl_gl
   use opengl_glu
   use opengl_glut
   
   use spheremod
   
   interface
     subroutine TessErrorHandler(i)	 bind(C,name='TessErrorHandler')
       integer, intent(in), value  :: i
     end subroutine TessErrorHandler
   end interface
   
   ! Clear the window
   call glClear(GL_COLOR_BUFFER_BIT)
   	
   select case(Method)
     case(DRAW_LOOPS)			! Draw line loops

       call glColor3f(0.0, 0.0, 0.0)    ! Just black outline
         
       ! Line loop with coastline shape
       call glBegin(GL_LINE_LOOP)
       do i=1,COAST_POINTS
         call glVertex3dv(vCoast(i,:))
       end do
       call glEnd

       ! Line loop with shape of interior lake
       call glBegin(GL_LINE_LOOP)
       do i=1,LAKE_POINTS
         call glVertex3dv(vLake(i,:))
       end do
       call glEnd
         
     case(DRAW_CONCAVE) 	       ! Tesselate concave polygon
        
       write(*,*)'concave'
       ! Green polygon
       call glColor3f(0.0, 1.0, 0.0) 
       
       ! Create the tesselator object
       pTess = gluNewTess()
       
       ! Set callback functions
       ! Just call glBegin at begining of triangle batch
       call gluTessCallback(pTess, GLU_TESS_BEGIN, c_funloc(glBegin))
       
       ! Just call glEnd at end of triangle batch
       call gluTessCallback(pTess, GLU_TESS_END, c_funloc(glEnd))
       
       ! Just call glVertex3dv for each  vertex
       call gluTessCallback(pTess, GLU_TESS_VERTEX, c_funloc(glVertex3dv))
       
       ! Register error callback
       call gluTessCallback(pTess, GLU_TESS_ERROR, c_funloc(TessErrorHandler))
       
       ! Begin the polygon
       ! gluTessBeginPolygon - delimit a polygon description
       ! void gluTessBeginPolygon( GLUtesselator* tess,
       !                            GLvoid* data )
       call gluTessBeginPolygon(pTess, c_null_ptr)
       
       ! Begin the one and only contour
       ! gluTessBeginContour, gluTessEndContour
       ! void gluTessBeginContour( GLUtesselator* tess )
       call gluTessBeginContour(pTess)

       ! Feed in the list of vertices
       do i=1,COAST_POINTS	 
         ! gluTessVertex - specify a vertex on a polygon
         ! void gluTessVertex( GLUtesselator* tess,
         !                     GLdouble *location,
         !                     GLvoid* data )	 
	     call gluTessVertex(pTess, vCoastCstyle(1:3,i), &
	                        c_loc(vCoastCstyle(1:3,i))) ! Data can't be NULL
       end do
!call glutReportErrors
         
       ! Close contour and polygon
       call gluTessEndContour(pTess)
       call gluTessEndPolygon(pTess)
       
       ! All done with tesselator object
       call gluDeleteTess(pTess)
   
       call glColor3f(0.0, 0.0, 0.0)    ! Just black outline
       call glBegin(GL_LINE_LOOP)
       do i=1,COAST_POINTS
         call glVertex3dv(vCoastCstyle(1,i))
       end do
       call glEnd

     case(DRAW_COMPLEX) 	 ! Tesselate, but with hole cut out
              
       write(*,*)'complex'
       ! Green polygon
       call glColor3f(0.0, 1.0, 1.0) 

       ! Create the tesselator object
       pTess = gluNewTess()
       
       ! Set callback functions
       ! Just call glBegin at begining of triangle batch
       call gluTessCallback(pTess, GLU_TESS_BEGIN, c_funloc(glBegin))
       
       ! Just call glEnd at end of triangle batch
       call gluTessCallback(pTess, GLU_TESS_END, c_funloc(glEnd))
       
       ! Just call glVertex3dv for each  vertex
       call gluTessCallback(pTess, GLU_TESS_VERTEX, c_funloc(glVertex3dv))
       
       ! Register error callback
       call gluTessCallback(pTess, GLU_TESS_ERROR, c_funloc(TessErrorHandler))

       ! How to count filled and open areas
       call gluTessProperty(pTess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_ODD)
       
       ! Begin the polygon
       call gluTessBeginPolygon(pTess, c_null_ptr) ! No user data
       
       ! First contour, outline of state
       call gluTessBeginContour(pTess)
       do i=1,COAST_POINTS
        !call gluTessVertex(pTess, vCoastCstyle(1,i), c_loc(dummy))
	     call gluTessVertex(pTess, vCoastCstyle(1:3,i), &
	                        c_loc(vCoastCstyle(1:3,i)))
       end do
       call gluTessEndContour(pTess)
       
       ! Second contour, outline of lake
       call gluTessBeginContour(pTess)
       do i=1,LAKE_POINTS
        !call gluTessVertex(pTess, vLake(i,1:3), c_loc(dummy))
	     call gluTessVertex(pTess, vLakeCstyle(1:3,i), &
	                        c_loc(vLakeCstyle(1:3,i)))
       end do
       call gluTessEndContour(pTess)
       
       ! All done with polygon
       call gluTessEndPolygon(pTess)
       
       ! No longer need tessellator object
       call gluDeleteTess(pTess)	

       call glColor3f(0.0, 0.0, 0.0)    ! Just black outline
         
       ! Line loop with coastline shape
       call glBegin(GL_LINE_LOOP)
       do i=1,COAST_POINTS
         call glVertex3dv(vCoast(i,:))
       end do
       call glEnd

       ! Line loop with shape of interior lake
       call glBegin(GL_LINE_LOOP)
       do i=1,LAKE_POINTS
         call glVertex3dv(vLake(i,:))
       end do
       call glEnd

   end select     
   ! Swap buffers
   call glutSwapBuffers
   
end subroutine renderscene
subroutine SetupRC
    
   use opengl_gl
    
   ! Blue background
   call glClearColor(1.0, 1.0, 1.0, 0.0 )
    
   ! Fat smooth lines to make it look nicer
   call glLineWidth(2.0)
   call glEnable(GL_LINE_SMOOTH)
   call glEnable(GL_BLEND)
   call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    
end subroutine SetupRC
subroutine ChangeSize(win, hin) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLu
 
   integer(kind=GLcint), intent(IN), value :: win, hin
   integer(kind=GLcint) :: w, h
   real(kind=GLdouble)  :: Zero, One, Aspect

   w = win
   h = hin
   
   if( h == 0 ) h = 1
   call glViewport(0, 0, w, h)

   ! Reset coordinate system
   call glMatrixMode(GL_PROJECTION)
   call glLoadIdentity

   ! Set the clipping volume
   call gluOrtho2D(dble(-80.0), dble(35.0), dble(-50.0), dble(50.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc

