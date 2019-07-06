function clean {

  test -f $1  && rm $1
  echo removed $1
}
clean ctrianglemeshes.mod
clean glframes.mod
clean opengl_glee.mod
clean opengl_glext.mod
clean vbomeshes.mod

