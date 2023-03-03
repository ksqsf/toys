/* Gaussian blur for Cairo */
/* Unfortunately, this is pure CPU computation. */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <cairo.h>

typedef unsigned char uc;
typedef unsigned char v4uc __attribute__((vector_size (4)));
typedef float v4f __attribute__((vector_size (16)));
typedef unsigned int v4ui __attribute__((vector_size (16)));

#define MAX_NUMBER_OF_GAUSSIAN_BOXES 10

static int *
boxes_for_gaussian (double sigma, int n)
{
  static int sizes[MAX_NUMBER_OF_GAUSSIAN_BOXES];
  double wIdeal, mIdeal;
  int wl, wu, m;

  if (n > MAX_NUMBER_OF_GAUSSIAN_BOXES)
    return NULL;

  wIdeal = sqrt ((12 * sigma * sigma / n) + 1);
  wl = floor (wIdeal);
  if (wl % 2 == 0)
    wl --;
  wu = wl + 2;

  mIdeal = (12 * sigma * sigma - n * wl * wl - 4 * n * wl - 3 * n) / (- 4 * wl - 4);
  m = round(mIdeal);

  for (int i = 0; i < n; ++i)
    sizes[i] = i < m ? wl : wu;
  return sizes;
}

v4f
uc2f (v4uc v)
{
  return (v4f) {
    v[0] / 128.0f, v[1] / 128.0f, v[2] / 128.0f, v[3] / 128.0f
  };
}

v4uc
f2uc (v4f v)
{
  return (v4uc) {
    v[0] * 128.0f, v[1] * 128.0f, v[2] * 128.0f, v[3] * 128.0f
  };
}

v4uc
scaled_mult4 (v4uc v, float x)
{
  return f2uc (uc2f (v) * x);
}

static void
box_blur_h (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < h; ++i) {
    for (int j = 0; j < w; ++j) {
      t[i*w+j] = (v4uc){0};
      for (int k = -r; k <= r; ++k) {
        if (j+k >= 0 && j + k < w)
          t[i*w+j] += scaled_mult4(s[i*w+j+k], iarr);
      }
    }
  }
}

static void
box_blur_t (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < h; ++i) {
    for (int j = 0; j < w; ++j) {
      t[i*w+j] = (v4uc){0};
      for (int k = -r; k <= r; ++k) {
        if (i + k >= 0 && i + k < h) {
          t[i*w+j] += scaled_mult4 (s[(i+k)*w+j], iarr);
        }
      }
    }
  }
}

static void
box_blur (cairo_surface_t *s, cairo_surface_t *t, int w, int h, int r)
{
  v4uc *sdata, *tdata;

  sdata = (v4uc *) cairo_image_surface_get_data (s);
  tdata = (v4uc *) cairo_image_surface_get_data (t);

  memcpy (tdata, sdata, w * h * 4);

  box_blur_h (tdata, sdata, w, h, r);
  box_blur_t (sdata, tdata, w, h, r);
}

static void
gaussian_blur (cairo_surface_t *s, cairo_surface_t *t, int w, int h, double r)
{
  int *boxes = boxes_for_gaussian (r, 3);
  printf ("Boxes = %d %d %d\n", boxes[0], boxes[1], boxes[2]);
  box_blur (s, t, w, h, (boxes[0] - 1.0) / 2.0);
  box_blur (t, s, w, h, (boxes[1] - 1.0) / 2.0);
  box_blur (s, t, w, h, (boxes[2] - 1.0) / 2.0);
}

int
main ()
{
  cairo_surface_t *source, *target;
  int w, h;
  clock_t begin, end;

  source = cairo_image_surface_create_from_png ("cballs.png");
  w = cairo_image_surface_get_width (source);
  h = cairo_image_surface_get_height (source);

  target = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w, h);

  begin = clock ();
  gaussian_blur (source, target, w, h, 5);
  end = clock ();
  printf ("Time = %g\n", (double)(end - begin) / CLOCKS_PER_SEC);

  cairo_surface_write_to_png (target, "output.png");
  cairo_surface_destroy (source);
  cairo_surface_destroy (target);
}
