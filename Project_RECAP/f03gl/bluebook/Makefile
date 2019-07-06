#  
# g95, gfortran
#
#  The directory containing the OpenGL libraries and GLUT libraries
OGLLIBDIR := -L/usr/lib 

#  Libraries for OpenGL, including GLUT, GLU and OpenGL
F90GLUTLIB := -lglut -lGL -lGLU

#FC        := g95
#FC         := gfortran-4.4
#FFLAGS     :=-fno-range-check -std=f2008
FC        := ifort
FFLAGS    := -g -O0 -auto -debug -module ..

LIBRARIES := -I.. ${OGLLIBDIR} 
LIBS      := ${F90GLUTLIB} 
OBJECTS   := math3d.o gltools.o ../GLUT_fonts.o \
             ../OpenGL_freeglut.o ../OpenGL_glu.o ../OpenGL_gl.o 

# override built-in make rule for modules
%.o : %.mod

#all: block pointsz triangle pstipple scissor stencil solar transform \
#transformgl sphereworld1 ambient litjet shinyjet spot shadow sphereworld2 \
#reflection smoother sphereworld3 fogged motionblur pyramid sphereworld4 \
#tunnel toon texgen cubemap snowman bezier bez3d bezlit nurbs nurbt florida \
#sphereworld5 starrynight cubedx thunderbird thunderbirdgl thunderbirdvbo \
#planets moons planets2 oocquery shadowmap

all:  block pointsz triangle pstipple scissor stencil solar transform \
transformgl sphereworld1 ambient litjet shinyjet spot shadow sphereworld2 \
reflection smoother sphereworld3 fogged motionblur pyramid sphereworld4 \
tunnel toon texgen cubemap snowman bezier bez3d bezlit nurbs nurbt florida \
sphereworld5 starrynight cubedx thunderbird thunderbirdgl thunderbirdvbo \
planets moons planets2 oocquery shadowmap

%.o: %.f90
	${FC} ${FFLAGS} -c $<

block pointsz triangle pstipple scissor stencil solar transform \
transformgl sphereworld1 ambient litjet shinyjet spot shadow sphereworld2 \
reflection smoother sphereworld3 fogged motionblur pyramid sphereworld4 \
tunnel toon texgen cubemap snowman bezier bez3d bezlit nurbs nurbt florida \
sphereworld5 starrynight cubedx thunderbird thunderbirdgl thunderbirdvbo \
planets moons planets2 oocquery shadowmap : %: %.f90 force
	${FC} ${FFLAGS} -c $<
	${FC} ${FFLAGS} $@.o ${OBJECTS} ${LIBRARIES} ${LIBS} -o $@
	./$@

force: math3d.o gltools.o 

