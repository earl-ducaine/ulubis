/* -*- mode: c;  -*-
   file: main.c
*/

#include "os-compatibility.h"

/* a.out wrapper for call into a shared library. */
int main() {
  int fd = os_create_anonymous_file(1024);
  return fd; 
}
