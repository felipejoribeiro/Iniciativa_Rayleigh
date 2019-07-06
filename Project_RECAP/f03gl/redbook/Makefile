OGLLIBDIR := -L/usr/lib
X11LIBDIR := -L/usr/X11R6/lib

#  Libraries for OpenGL, including GLUT, GLU and OpenGL
F90GLUTLIB := -lglut -lGL -lGLU
#  The X11 libraries
X11LIB := 

FC        := ifort
FFLAGS    := -module ..
#FC        := gfortran
#FFLAGS    := -fno-range-check

LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib
LIBS      := ${F90GLUTLIB} ${X11LIB}
OBJECTS   := ../GLUT_fonts.o ../OpenGL_freeglut.o ../OpenGL_glu.o ../OpenGL_gl.o

# override built-in make rule for modules
%.o : %.mod

all: fgl10 fgl11 fgl12 fgl13 fgl14 fgl15 fgl16 fgl17 fgl17b \
     fgl18 fgl19 fgl20 fgl21 
%.o: %.f90
	${FC} ${FFLAGS} -c $<

fgl10 fgl11 fgl12 fgl13 fgl14 fgl15 fgl16 fgl17 fgl17b \
fgl18 fgl19 fgl20 fgl21 : %: %.f90 
	${FC} ${FFLAGS} -c $<
	${FC} $@.o ${OBJECTS} ${LIBRARIES} ${LIBS} -o $@
	./$@

force:

