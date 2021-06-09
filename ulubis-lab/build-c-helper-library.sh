# -*- mode: bash;  -*-


rm -f *.o *.so ulubis-lab-simple-egl-test

export libs="-lm"

# Note, the -Wl,-R flags will make our shared library available to the
# executable app from the location that it was compiled, rather than
# having to be installed globably or adding the build path to
# LD_LIBRARY_PATH.

export ldflags="-L. -Wl,-R -Wl,."
export cflags="-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall -I. -Iprotocol"

gcc $cflags -c ulubis-lab-simple-egl.c

gcc -shared -Wl,-soname,libulubis-lab-simple-egl.so $ldflags  \
    -o libulubis-lab-simple-egl.so *o $libs \
    -lwayland-egl \
    -lwayland-client \
    -lwayland-cursor \
    -lEGL \
    -lglfw \
    -lOpenGL

gcc ulubis-lab-simple-egl-test.c $cflags $ldflags \
    -o ulubis-lab-simple-egl-test \
    -lulubis-lab-simple-egl

# Possible libraries to include.
# -lGLU
# -lOpenGL
# -lglut
# -lwayland-egl
# -lwayland-cursor
# -lwayland-client
