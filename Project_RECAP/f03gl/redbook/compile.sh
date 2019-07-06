rm fgl?? fgl17b

cp ../GLUT_fonts.o .
cp ../OpenGL_freeglut.o .
cp ../OpenGL_gl.o .
cp ../OpenGL_glu.o .
cp ../opengl_gl*.mod .

#F03="gfortran"
F03="ifort"
INCL="-I.."
OBJS="GLUT_fonts.o OpenGL_freeglut.o OpenGL_glu.o OpenGL_gl.o"
LIBS="-L/usr/lib -L/opt/intel/composerxe/lib/intel64/ -lglut -lGL -lGLU"

$F03 -o fgl10 fgl10.f90 $INCL $OBJS $LIBS
./fgl10

$F03 -o fgl11 fgl11.f90 -fno-range-check $INCL $OBJS $LIBS
./fgl11

$F03 -o fgl12 fgl12.f90 $INCL $OBJS $LIBS
./fgl12

$F03 -o fgl13 fgl13.f90 $INCL $OBJS $LIBS
./fgl13

$F03 -o fgl14 fgl14.f90 $INCL $OBJS $LIBS
./fgl14

$F03 -o fgl15 fgl15.f90 $INCL $OBJS $LIBS
./fgl15

$F03 -o fgl16 fgl16.f90 $INCL $OBJS $LIBS
./fgl16

$F03 -o fgl17 fgl17.f90 -fno-range-check $INCL $OBJS $LIBS
./fgl17

$F03 -o fgl17b fgl17b.f90 -fno-range-check $INCL $OBJS $LIBS
./fgl17b

$F03 -o fgl18 fgl18.f90 $INCL $OBJS $LIBS
./fgl18

$F03 -o fgl19 fgl19.f90 $INCL $OBJS $LIBS
./fgl19

$F03 -o fgl20 fgl20.f90 $INCL $OBJS $LIBS
./fgl20

$F03 -o fgl21 fgl21.f90 $INCL $OBJS $LIBS
./fgl21

