

#include <stdlib.h>
#include <math.h>
#include <stdio.h>


static inline wl_fixed_t
wl_fixed_from_double(double d)
{
  union access_bytes u;
  u.d = d + (3LL << (51 - 8));
  last_access_bytes = u;
  return (wl_fixed_t)u.i;
}

static inline double
wl_fixed_to_double(wl_fixed_t f)
{
  union access_bytes u;
  u.i = ((1023LL + 44LL) << 52) + (1LL << 51) + f;
  last_access_bytes = u;
  return u.d - (3LL << 43);
}

typedef int32_t wl_fixed_t;

struct bytes {
  unsigned char b1;
  unsigned char b2;
  unsigned char b3;
  unsigned char b4;
  unsigned char b5;
  unsigned char b6;
  unsigned char b7;
  unsigned char b8;
};

union access_bytes {
  struct bytes bts;
  double d;
  int64_t i;
};

union access_bytes last_access_bytes;

// 100.25 => 100 + 25/100 => #b1100100 + #b0100/(#b10^#b1000) =>
// ((#b1100100 * #b10^#b1000) + #b0100)/(#b10^#b1000)
// ((<integer-part> * #b10^#b1000) + <fraction part>)/(#b10^#b1000)

void print_bytes() {
  printf("Big endian bytes:");
  printf("0x%02x%02x 0x%02x%02x\n\n",
	 last_access_bytes.bts.b4,
	 last_access_bytes.bts.b3,
	 last_access_bytes.bts.b2,
	 last_access_bytes.bts.b1);
}

int main() {
  double d = 100.25;
  int32_t i = 0;
  i = wl_fixed_from_double(d);
  printf("wl_fixed_from_double(%f): %d\n", d, wl_fixed_from_double(d));
  print_bytes();
  printf("wl_fixed_to_double(%i): %f\n", i, wl_fixed_to_double(i));
  print_bytes();
  d = 527747.23;
  i = wl_fixed_from_double(d);
  printf("wl_fixed_from_double(%f): %d\n", d, wl_fixed_from_double(d));
  print_bytes();
  printf("wl_fixed_to_double(%i): %f\n", i, wl_fixed_to_double(i));
  print_bytes();
  return 0;
}
