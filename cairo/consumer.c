#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <cairo.h>

extern void opencv_gaussian_blur (void *srcdata, int w, int h, double sigma);

int
main (int argc, char *argv[])
{
  cairo_surface_t *source;
  struct timespec begin, end;
  double secs;
  int w, h;

  if (argc < 2)
    {
      fprintf (stderr, "USAGE: fast_gaussian_blur <PNG_FILE>\n");
      return 1;
    }

  source = cairo_image_surface_create_from_png (argv[1]);

  w = cairo_image_surface_get_width (source);
  h = cairo_image_surface_get_height (source);
  printf ("Image geometry %d x %d\n", w, h);

  clock_gettime (CLOCK_MONOTONIC, &begin);
  opencv_gaussian_blur (cairo_image_surface_get_data (source), w, h, 5.0);
  clock_gettime (CLOCK_MONOTONIC, &end);
  secs = (double) (end.tv_sec - begin.tv_sec) + (double) (end.tv_nsec - begin.tv_nsec) / 1e9;
  printf ("Time = %g secs\n", secs);
  printf ("Average %g ns per pixel\n", secs * 1e9 / w / h);
  printf ("Average %g pixels per ns\n", w * h / (secs * 1e9));

  cairo_surface_write_to_png (source, "output.png");
  cairo_surface_destroy (source);
}
