!
! Toon.cpp
! OpenGL SuperBible
! Demonstrates Cell/Toon shading with a 1D texture
! Original program by Richard S. Wright Jr.
!
program toon

   use opengl_gl
   use opengl_glut
   
   interface
     subroutine RenderScene() bind(C)
     end subroutine RenderScene

     subroutine ChangeSize(w,h) bind(C)
       use opengl_gl
       integer(glcint), intent(in), value :: w,h
     end subroutine ChangeSize

     subroutine KeyPressFunc(key, x,y) bind(C)
       use opengl_gl
       integer(kind=GLbyte), intent(IN), value :: key
       integer(kind=GLint), intent(in), value  :: x, y
     end subroutine KeyPressFunc

     recursive subroutine TimerFunction(i) bind(C)
       integer, intent(in), value :: i
     end subroutine TimerFunction
   end interface
     
   call glutInit
   call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
   call glutInitWindowSize(800, 600)
   iwin = glutCreateWindow('Toon/Cell Shading Demo'//char(0))

   call glutReshapeFunc(ChangeSize)
   call glutKeyboardFunc(KeyPressFunc)
   call glutDisplayFunc(RenderScene)
   call glutTimerFunc(33, TimerFunction, 1)

   call setuprc
   
   call glutMainLoop

end program
subroutine KeyPressFunc(key, x, y) bind(C)
   
   use OpenGL_GL
   use OpenGL_GLUT

   integer(kind=GLbyte), intent(IN), value :: key
   integer(kind=GLint), intent(in), value  :: x, y

   if( key == 27 )then
     stop
   endif
   
end subroutine KeyPressFunc
subroutine toonDrawTorus(RadMaj, RadMin, NMaj, Nmin, vlightdir)

   use opengl_gl
   use opengl_glut

   real, dimension(3)  :: vlightdir
   
   real, dimension(16) :: mModelViewMatrix 
   real, dimension(16) :: mInvertedLight 
   real, dimension( 3) :: vNewLight  
   real, dimension( 3) :: vNormal  
   
   stepMaj = 2.0*3.141592653589793/float(Nmaj) 
   stepMin = 2.0*3.141592653589793/float(Nmin) 
   
   ! Get the modelview matrix
   call glGetFloatv(GL_MODELVIEW_MATRIX, mModelViewMatrix)
    
   ! Instead of transforming every normal and then dotting it with
   ! the light vector, we will transform the light into object 
   ! space by multiplying it by the inverse of the modelview matrix
   call m3dInvertMatrix44(mInvertedLight, mModelViewMatrix)
   call m3dTransformVector3(vNewLight, vLightDir, mInvertedLight)
  
   vNewLight(1) = vNewLight(1) - mInvertedLight(13)
   vNewLight(2) = vNewLight(2) - mInvertedLight(14)
   vNewLight(3) = vNewLight(3) - mInvertedLight(15)
   call m3dNormalizeVector(vNewLight)
    
   ! Draw torus as a series of triangle strips
   do i=0,nMaj-1
      
     a0 = i * StepMaj
     a1 = a0 + StepMaj
     x0 = cos(a0)
     y0 = sin(a0)
     x1 = cos(a1)
     y1 = sin(a1)

     call glBegin(GL_TRIANGLE_STRIP)
     do j=0,nMin
       
       b = j * StepMin
       c = cos(b)
       r = RadMin * c + RadMaj
       z = RadMin * sin(b)

       ! First point
       vNormal(1) = x0*c
       vNormal(2) = y0*c
       vNormal(3) = z/RadMin
       call m3dNormalizeVector(vNormal)
       
       ! Texture coordinate is set by intensity of light
       call glTexCoord1f(Dot_Product(vNewLight, vNormal))
       call glVertex3f(x0*r, y0*r, z)

       ! Second point
       vNormal(1) = x1*c
       vNormal(2) = y1*c
       vNormal(3) = z/RadMin
       call m3dNormalizeVector(vNormal)
            
       ! Texture coordinate is set by intensity of light
       call glTexCoord1f(Dot_Product(vNewLight, vNormal))
       call glVertex3f(x1*r, y1*r, z)
     end do
     call glEnd
   end do

end subroutine toonDrawTorus   
subroutine RenderScene() bind(C)

   use opengl_gl
   use opengl_glut

   real, dimension(3)  :: vlightdir = (/ -1.0, 1.0, 1.0 /)

   real, save :: yRot = 0.0
   
   call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))

   call glPushMatrix
   call glTranslatef(0.0, 0.0, -2.5)
   call glRotatef(yRot, 0.0, 1.0, 0.0)
   call toonDrawTorus(0.35, 0.15, 50, 25, vLightDir)
   call glPopMatrix
   
   call glutSwapBuffers

   yRot = yRot + 0.5
   
end subroutine RenderScene
subroutine SetupRC

   use OpenGL_GL
   use OpenGL_GLu

   integer(kind=GLbyte), dimension(4,3), target :: toonTable
   
   ! Load a 1D texture with toon shaded values
   ! Green, greener...
   toonTable(1,:) = (/ 0,    32, 0 /)
   toonTable(2,:) = (/ 0,    64, 0 /)  
   toonTable(3,:) = (/ 0, transfer(z'80',GLbyte), 0 /)  
   toonTable(4,:) = (/ 0, transfer(z'C0',GLbyte), 0 /)  
   
   ! Bluish background
   call glClearColor(0.0, 0.0, 0.5, 1.0 )
   call glEnable(GL_DEPTH_TEST)
   call glEnable(GL_CULL_FACE)
        
   call glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL)
   call glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
   call glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
   call glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP)
   call glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
   call glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB, 4, 0, GL_RGB, &
                     GL_UNSIGNED_BYTE, c_loc(toonTable))
    
   call glEnable(GL_TEXTURE_1D)

end subroutine SetupRC
recursive subroutine TimerFunction( ivalue ) bind(C)

   use opengl_glut

   integer, intent(in), value :: ivalue

   call glutPostRedisplay()
   call glutTimerFunc(33,TimerFunction, 1)

end subroutine TimerFunction
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

   ! Establish clipping volume (left, right, bottom, top, near, far)
   aspect = float(w)/float(h)

   ! Set the clipping volume
   call gluPerspective(dble(35.), Aspect, dble(1.), dble(50.0))

   call glMatrixMode(GL_MODELVIEW)
   call glLoadIdentity

end subroutine ChangeSize 
