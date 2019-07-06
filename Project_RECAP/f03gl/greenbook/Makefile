OGLLIBDIR := -L/usr/lib
#X11LIBDIR := -L/usr/X11R6/lib

#  Libraries for OpenGL, including GLUT, GLU and OpenGL
F90GLUTLIB := -lglut -lGL -lGLU
#  The X11 libraries
X11LIB := 

FC        := ifort
FFLAGS    := -module ..
#FC        := gfortran
#FFLAGS    := 

LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} 
LIBS      := ${F90GLUTLIB} ${X11LIB}
OBJECTS   := ../GLUT_fonts.o ../OpenGL_freeglut.o ../OpenGL_glu.o ../OpenGL_gl.o 

all: fgl01 fgl02 fgl03 fgl04 fgl05 fgl06 fgl07 fgl08 fgl09
%.o: %.f90
	${FC} ${FFLAGS} -c $<

fgl01 fgl02 fgl03 fgl04 fgl05 fgl06 fgl07 fgl08 fgl09 : %: %.f90
	${FC} ${FFLAGS} -c $<
	${FC} $@.o ${OBJECTS} ${LIBRARIES} ${LIBS} -o $@
	./$@

