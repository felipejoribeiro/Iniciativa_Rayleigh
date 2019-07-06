function clean {

  test -f $1      && echo removing $1
  test -f $1      && rm $1
  test -f $1.o    && rm $1.o
  test -f $1.mod  && rm $1.mod
  
}

echo "f03gl files..."

for Case in GLUT_fonts.o \
            OpenGL_freeglut.o OpenGL_gl.o OpenGL_glu.o \
            opengl_gl.mod opengl_glu.mod opengl_glut.mod  opengl_kinds.mod \
            trivial blender checker fscene  glutdino gltdino2 logof90gl \
            modview molehill olympic olympic3 scube sphere stars torus \
            fbitfont eps array polyoff plotfunc
  do
    clean $Case
  done

echo "greenbook files..."
cd greenbook

for Case in callbacks opengl_gl opengl_glu opengl_glut opengl_kinds \
            fgl01 fgl02 fgl03 fgl04 fgl05 fgl06 fgl07 fgl08 fgl09
  do
    clean $Case
  done

echo "redbook files..."
cd ../redbook

for Case in opengl_glee opengl_glext \
            fgl10 fgl11 fgl12 fgl13 fgl14 fgl15 fgl16 \
            fgl17 fgl17b fgl18 fgl19 fgl20 fgl21 fgl20m
  do
    clean $Case
  done

echo "bluebook files..."
cd ../bluebook

for Case in opengl_glee opengl_glext \
            ctrianglemeshes.mod glframes.mod vbomeshes.mod \
            block pointsz triangle pstipple scissor stencil solar transform \
            transformgl sphereworld1 ambient litjet shinyjet spot shadow sphereworld2 \
            reflection smoother sphereworld3 fogged motionblur pyramid sphereworld4 \
            tunnel toon texgen cubemap snowman bezier bez3d bezlit nurbs nurbt florida \
            sphereworld5 starrynight cubedx thunderbird thunderbirdgl thunderbirdvbo \
            planets moons planets2 oocquery shadowmap     
  do
    clean $Case
  done

cd ..
echo "done"

