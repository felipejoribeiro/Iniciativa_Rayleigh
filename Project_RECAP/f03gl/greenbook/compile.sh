F03="ifort"
OBJS="GLUT_fonts.o OpenGL_glut.o OpenGL_glu.o OpenGL_gl.o"

$F03 -o fgl01 fgl01.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl01

$F03 -o fgl02 fgl02.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl02

$F03 -o fgl03 fgl03.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl03

$F03 -o fgl04 fgl04.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl04

$F03 -o fgl05 fgl05.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl05

$F03 -o fgl06 fgl06.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl06

$F03 -o fgl07 fgl07.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl07

$F03 -o fgl08 fgl08.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl08

$F03 -o fgl09 fgl09.f90 $OBJS -L/usr/lib -lglut -lGL -lGLU 
./fgl09

