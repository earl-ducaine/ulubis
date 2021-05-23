# -*- mode: bash;  -*-


rm -f *.o *.so app

export libs="-lm"

# Note, the -Wl,-R flags will make our shared library available to the
# executable app from the location that it was compiled, rather than
# having to be installed globably or adding the build path to
# LD_LIBRARY_PATH.

export ldflags="-L. -Wl,-R -Wl,."
export cflags="-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall"

gcc $cflags -c os-compatibility.c
gcc -shared -Wl,-soname,libos-compatibility.so $ldflags -o libos-compatibility.so *o \
    $libs
gcc os-compatibility-test.c $cflags $ldflags -los-compatibility  \
    -o os-compatibility-test
