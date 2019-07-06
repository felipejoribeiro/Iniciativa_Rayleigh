!
! Nurbs.cpp
! OpenGL SuperBible
! Richard S. Wright Jr.
!
module spheremod

   use opengl_gl
   
   integer, parameter :: NumPoints = 4 ! 4x4
   
   real(kind=GLfloat), dimension(4) :: whiteLight   = (/ 0.7, 0.7, 0.7, 1. /)
   real(kind=GLfloat), dimension(4) :: ambientLight = (/ 0.3, 0.3, 0.3, 1. /)
   real(kind=GLfloat), dimension(4) :: diffuseLight = (/ 0.7, 0.7, 0.7, 1. /)
   real(kind=GLfloat), dimension(4) :: specular     = (/ 0.7, 0.7, 0.7, 1. /)
   real(kind=GLfloat), dimension(4) :: lightPos     = (/ 20., 0.0, 0.0, 0. /)

   real(kind=GLfloat), dimension(1) :: shine = (/ 100.0 /)

   ! Mesh extends four units -6 to +6 along x and y axis
   ! Lies in Z plane
   !                  u  v  (x,y,z)	
   !       ctrlPoints[4][4][3]
   !
   real(kind=GLfloat), dimension(NumPoints,NumPoints,3) :: ctrlPoints 
   
   ! Knot sequence for the NURB
   real(kind=GLfloat), dimension(8) :: Knots(8) = &
                                 (/ 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0 /)

   type(C_PTR) :: pNurb


end module spheremod
program nurbs

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
           
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('NURBS Surface'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)

   call setuprc
   
   call glutMainLoop

end program
subroutine DrawPoints
! This function is used to superimpose the control points over the curve

   use opengl_gl

   use spheremod
      
   ! Set point size larger to make more visible
   call glPointSize(5.0)
   call glColor3f(1.0,0.0,0.0)

   ! Loop through all control points for this example
   call glBegin(GL_POINTS)
   do i=1,NumPoints
     do j=1,NumPoints
       call glVertex3fv(ctrlPoints(i,j,:))
     end do
   end do
   call glEnd

end subroutine DrawPoints
integer function lens(string)

   character(len=*) string
   
   do i=len(string),1,-1
     if( string(i:i) .ne. ' ')goto 10
   end do
   i = 0
10 continue

   lens = i
    
end function lens
subroutine NurbsErrorHandler(i) BIND(C,NAME='NurbsErrorHandler')

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
   len = strlen(c_pointer)                       ! find the char(0) (=length)
   call c_f_pointer(c_pointer, f_pointer)        ! turn into something usable
          
   cmessage = 'NURBS error occured: '//f_pointer(1:len)
   
   write(*,*) cmessage
   ! Display the message to the user
   call glutSetWindowTitle(cMessage(1:lens(cMessage))//char(0))
   
end subroutine NurbsErrorHandler 
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glu
   use opengl_glut

   use spheremod

   real, dimension(NumPoints*NumPoints*3) :: tmp

   ! Draw in Blue
   call glColor3f(0.0,0.0,1.0)

   ic = 1
   do i=1,NumPoints
     do j=1,NumPoints
       tmp(ic:ic+2) = ctrlPoints(i,j,:)
       ic = ic + 3
     end do
   end do

   ! Clear the window with current clearing color
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   ! Save the modelview matrix stack
   call glMatrixMode(GL_MODELVIEW)
   call glPushMatrix

   ! Rotate the mesh around to make it easier to see
   call glRotatef(330.0, 1.0, 0.0, 0.0)

   ! Render the NURB
   ! Begin the NURB definition
   call gluBeginSurface(pNurb)
	
   ! Evaluate the surface
   call gluNurbsSurface(pNurb,& ! pointer to NURBS renderer
       8, Knots,               & ! No. of knots and knot array u direction       
       8, Knots,               & ! No. of knots and knot array v direction
       4 * 3,                  & ! Distance between control points in u dir.
       3,                      & ! Distance between control points in v dir.
       tmp,                    & ! Control points
       4, 4,                   & ! u and v order of surface
       GL_MAP2_VERTEX_3);      & ! Type of surface
               
   ! Done with surface
   call gluEndSurface(pNurb)

   ! Show the control points
   call DrawPoints

   ! Restore the modelview matrix
   call glPopMatrix

   ! Flush drawing commands
   call glutSwapBuffers
  
end subroutine renderscene
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLu

   use spheremod

   interface
     subroutine NurbsErrorHandler(i)	     BIND(C,NAME='NurbsErrorHandler')
       integer, intent(in), value  :: i
     end subroutine NurbsErrorHandler
   end interface

   ctrlPoints(1,1,:) = (/ -6.0, -6.0, 0.0 /) ! u = 0  v = 0 
   ctrlPoints(1,2,:) = (/ -6.0, -2.0, 0.0 /) !       	    
   ctrlPoints(1,3,:) = (/ -6.0,  2.0, 0.0 /) !       	    
   ctrlPoints(1,4,:) = (/ -6.0,  6.0, 0.0 /) !       	    
                                           
   ctrlPoints(2,1,:) = (/  2.0, -6.0, 0.0 /) ! u = 1  v = 0 
   ctrlPoints(2,2,:) = (/ -2.0, -2.0, 8.0 /) !       	    
   ctrlPoints(2,3,:) = (/ -2.0,  2.0, 8.0 /) !       	    
   ctrlPoints(2,4,:) = (/ -2.0,  6.0, 0.0 /) !       	    
                                            
   ctrlPoints(3,1,:) = (/  2.0, -6.0, 0.0 /) ! u = 2  v = 0 
   ctrlPoints(3,2,:) = (/  2.0, -2.0, 8.0 /) !       	    
   ctrlPoints(3,3,:) = (/  2.0,  2.0, 8.0 /) !       	    
   ctrlPoints(3,4,:) = (/  2.0,  6.0, 0.0 /) !       	    

   ctrlPoints(4,1,:) = (/  6.0, -6.0, 0.0 /) ! u = 3  v = 0 
   ctrlPoints(4,2,:) = (/  6.0, -2.0, 0.0 /) !                    
   ctrlPoints(4,3,:) = (/  6.0,  2.0, 0.0 /) !                    
   ctrlPoints(4,4,:) = (/  6.0,  6.0, 0.0 /) !                    
    
   ! Clear Window to white
   call glClearColor(1.0, 1.0, 1.0, 1.0)

   ! Enable lighting
   call glEnable(GL_LIGHTING)
   call glEnable(GL_LIGHT0)

   ! Enable color tracking
   call glEnable(GL_COLOR_MATERIAL)
	
   ! Set Material properties to follow glColor values
   call glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
   call glMaterialfv(GL_FRONT, GL_SPECULAR, specular)
   call glMaterialfv(GL_FRONT, GL_SHININESS, shine)
	
   ! Automatically generate normals for evaluated surfaces
   call glEnable(GL_AUTO_NORMAL)

   ! Setup the Nurbs object
   pNurb = gluNewNurbsRenderer()

   ! Install error handler to notify user of NURBS errors
   ! GLVoid (*callback)();
   call gluNurbsCallback(pNurb, GLU_ERROR, c_funloc(NurbsErrorHandler))

   call gluNurbsProperty(pNurb, GLU_SAMPLING_TOLERANCE, 25.0)
   ! Uncomment the next line and comment the one following to produce a
   ! wire frame mesh.
   ! gluNurbsProperty(pNurb, GLU_DISPLAY_MODE, GLU_OUTLINE_POLYGON)
   call gluNurbsProperty(pNurb, GLU_DISPLAY_MODE, float(GLU_FILL))

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

   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluPerspective(dble(45.), Aspect, dble(1.), dble(40.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

   call glTranslatef(0.0, 0.0, -20.0)

end subroutine ChangeSize 
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
