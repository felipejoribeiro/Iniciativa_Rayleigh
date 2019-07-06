
#cp ../GLUT_fonts.o .
#cp ../OpenGL_freeglut.o .
#cp ../OpenGL_gl.o .
#cp ../OpenGL_glu.o .
#cp ../opengl_gl*.mod .

#F03="gfortran"
F03="ifort"
INCL="-I.."
OBJS="GLUT_fonts.o OpenGL_freeglut.o OpenGL_glu.o OpenGL_gl.o"
LIBS="-L/usr/lib -L/opt/intel/composerxe/lib/intel64/ -lglut -lGL -lGLU"
MATH="math3d.o"
TOOLS="math3d.o gltools.o"
#
# nog niet ok: florida  
#
#gcc -c gltools_c.c -I /usr/include/GL  
$F03 -c math3d.f90
$F03 -c gltools.f90

$F03 -o block block.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o pointsz pointsz.f90 $INCL $OBJS $LIBS

$F03 -o triangle triangle.f90 $INCL $OBJS $LIBS

$F03 -o pstipple pstipple.f90 $INCL $OBJS $LIBS

$F03 -o scissor scissor.f90 $INCL $OBJS $LIBS

$F03 -o stencil stencil.f90 $INCL $OBJS $LIBS

$F03 -o solar solar.f90 $INCL $OBJS $LIBS

$F03 -o transform transform.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o transformgl transformgl.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o sphereworld1 sphereworld1.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o ambient ambient.f90 $INCL $OBJS $LIBS

$F03 -o litjet litjet.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o shinyjet shinyjet.f90 $INCL $MATH $OBJS $LIBS

$F03 -o spot spot.f90 $INCL $OBJS $LIBS

$F03 -o shadow shadow.f90 $INCL $MATH $OBJS $LIBS

$F03 -o sphereworld2 sphereworld2.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o reflection reflection.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o smoother smoother.f90 $INCL $OBJS $LIBS

$F03 -o sphereworld3 sphereworld3.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o fogged fogged.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o motionblur motionblur.f90 $INCL $OBJS $LIBS

$F03 -o pyramid pyramid.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o sphereworld4 sphereworld4.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o tunnel tunnel.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o toon toon.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o texgen texgen.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o cubemap cubemap.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o snowman snowman.f90 $INCL $OBJS $LIBS

$F03 -o bezier bezier.f90 $INCL $OBJS $LIBS

$F03 -o bez3d bez3d.f90 $INCL $OBJS $LIBS

$F03 -o bezlit bezlit.f90 $INCL $OBJS $LIBS

$F03 -o nurbs nurbs.f90 $INCL $OBJS $LIBS

$F03 -o nurbt nurbt.f90 $INCL $OBJS $LIBS

$F03 -o florida florida.f90 $INCL $OBJS $LIBS

$F03 -o sphereworld5 sphereworld5.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o starrynight starrynight.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o cubedx cubedx.f90 $INCL $OBJS $LIBS

$F03 -o thunderbird thunderbird.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o thunderbirdgl thunderbirdgl.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -O0 -g -o thunderbirdvbo thunderbirdvbo.f90 $INCL $TOOLS $OBJS $LIBS

$F03 -o planets planets.f90 $INCL $OBJS $LIBS

$F03 -o moons moons.f90 $INCL $OBJS $LIBS

$F03 -o planets2 planets2.f90 $INCL $OBJS $LIBS

$F03 -o oocquery oocquery.f90 $INCL  $TOOLS $OBJS $LIBS

$F03 -o shadowmap shadowmap.f90 $INCL  $TOOLS $OBJS $LIBS
  

