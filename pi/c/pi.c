#include <stdio.h>
#include <stdlib.h>

int main() {
  long nit, it, in;
  double pi, x, y;

  fprintf(stderr, "Please eneter number of MC steps = ");
  scanf("%ld", &nit);

  in = 0;
  for (it = 0; it < nit; it++) {
    x = (double)random()/RAND_MAX;
    y = (double)random()/RAND_MAX;

    if (x*x + y*y <= 1.0f) {
      in++;
    }
  }

  pi = in*4.0f/nit;
  fprintf(stderr,"Pi = %.10f\n",pi);

  return 0;
}
