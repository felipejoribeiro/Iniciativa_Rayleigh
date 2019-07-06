#DEBUG := -g -O0

#  Comment out for 32-bit architecture
BITS      := 64

#  The directories containing the OpenGL libraries and GLUT libraries
#OGLLIBDIR := -L/usr/lib${BITS}

#  The directory containing the X11 libraries
#X11LIBDIR := -L/usr/X11R6/lib${BITS}
X11LIBDIR := -L/usr/lib

#  Libraries for OpenGL, including GLUT, GLU and OpenGL
F90GLUTLIB := -lglut -lGL -lGLU

#  The X11 libraries
#X11LIB := -lXaw -lXt -lXmu -lXi -lXext -lX11 -lm -lXxf86vm
X11LIB := -lXt -lXmu -lXi -lXext -lX11 -lm 

ifndef COMPILER
  COMPILER := ifort
endif

FC=${COMPILER}

#  If you use OpenGLUT or FreeGlut, change this variable and adjust
#  the libraries appropriately.
# GLUT      := glut
# GLUT      := openglut
GLUT      := freeglut

ifeq (${COMPILER},nagfor)
#  Nag nagfor compiler, version 5.2
  FFLAGS    := $(DEBUG) -f2003 -colour -gline -DNAGF95 -DF2003 -DOPENGL
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib${BITS}
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl
endif
ifeq (${COMPILER},gfortran)
  FFLAGS    := $(DEBUG) -DOPENGL -fno-range-check -std=f2008
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib${BITS} 
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl -lgfortran
endif
ifeq (${COMPILER},gfortran-4.3)
#  Gfortran gcc version 4.3.4 (Ubuntu 4.3.4-10ubuntu1) 
  FFLAGS    := $(DEBUG) -DOPENGL -fno-range-check -std=f2008
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib${BITS} 
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl -lgfortran
endif
ifeq (${COMPILER},gfortran-4.4)
#  Gfortran gcc version 4.4.3 (Ubuntu 4.4.3-4ubuntu5)
  FFLAGS    := $(DEBUG) -DOPENGL -fno-range-check #-std=f2008
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib${BITS} 
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl -lgfortran
endif
ifeq (${COMPILER},ifort)
#  ifort compiler, version 12.0 for Intel 64 
  FFLAGS    := $(DEBUG) -DOPENGL #-stand f03 -warn stderrors 
# LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/lib${BITS}
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/opt/intel/composerxe/lib/intel64/
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl
endif
ifeq (${COMPILER},g95)
#  g95 compiler, version 0.92 (july 2009)
  FFLAGS    := $(DEBUG) -DOPENGL 
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib${BITS} -L.
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl
#  GLUT      := freeglut
endif


#LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib
MODULES    = GLUT_fonts.o OpenGL_${GLUT}.o OpenGL_glu.o OpenGL_gl.o 

#all: sphere stars blender scube modview plotfunc 
all: trivial blender checker fscene  glutdino gltdino2 logof90gl modview molehill olympic olympic3 scube \
sphere stars torus fbitfont eps array polyoff plotfunc
#all: plotfunc

%.o: %.f90
	${FC} ${FFLAGS} -c $<

#blender modview plotfunc scube sphere stars : %: %.f90 ${MODULES} force
trivial blender checker fscene  glutdino gltdino2 logof90gl modview molehill olympic olympic3 scube sphere stars torus fbitfont eps array polyoff plotfunc: %: %.f90 ${MODULES} force
#checker fscene  glutdino gltdino2 logo modview molehill olympic olympic3 scube sphere stars torus fbitfont eps array polyoff plotfunc : %: %.f90 ${MODULES} force
#plotfunc : %: %.f90 ${MODULES} force
	${FC} ${FFLAGS} -c $<
	${FC} $@.o ${MODULES} ${LIBRARIES} ${LIBS} -o $@
	./$@

OpenGL%.mod: OpenGL%.f90
	${FC} ${FFLAGS} -c $<

force:

OpenGL_${GLUT}.o OpenGL_glu.o: OpenGL_gl.o

clean:
	-rm -f *.mod *.o

