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

#define uc2f(v) (__builtin_convertvector((v), v4f) / 128.0f)
#define f2uc(v) (__builtin_convertvector((v) * 128.0f, v4uc))
#define scaled_mult4(v,x) (f2uc(uc2f((v))*x))

static void
box_blur_h (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < h; ++i)
    {
      int ti = i * w, li = ti, ri = ti + r;
      v4uc fv = s[ti], lv = s[ti+w-1];
      v4f val = uc2f(fv) * (float)(r+1);
      for(int j=0;j<r;++j)
        val += uc2f(s[ti+j]);
      for(int j=0;j<=r;++j)
        {
          val += uc2f(s[ri++])-uc2f(fv);
          t[ti++] = f2uc(val*iarr);
        }
      for(int j=r+1;j<w-r;++j)
        {
          val += uc2f(s[ri++])-uc2f(s[li++]);
          t[ti++] = f2uc(val*iarr);
        }
      for(int j=w-r;j<w;++j)
        {
          val += uc2f(lv)-uc2f(s[li++]);
          t[ti++] = f2uc(val*iarr);
        }
    }
}

static void
box_blur_t (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < w; ++i)
    {
      int ti = i, li = ti, ri = ti+r*w;
      v4uc fv = s[ti], lv = s[ti+w*(h-1)];
      v4f val = uc2f(fv) * (float)(r+1);
      for(int j=0; j<r; ++j)
        val += uc2f(s[ti+j*w]);
      for(int j=0; j<=r; ++j)
        {
          val += uc2f(s[ri]) - uc2f(fv);
          t[ti] = f2uc(val*iarr);
          ri+=w; ti+=w;
        }
      for(int j=r+1; j<h-r; ++j)
        {
          val += uc2f(s[ri]) - uc2f(s[li]);
          t[ti] = f2uc(val*iarr);
          li+=w; ri+=w; ti+=w;
        }
      for(int j=h-r; j<h; ++j)
        {
          val += uc2f(lv) - uc2f(s[li]);
          t[ti] = f2uc(val*iarr);
          li+=w; ti+=w;
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

#define TIME

#ifdef TIME
#define START do { clock_t begin, end; begin = clock();
#define END(label) end = clock(); printf("%s: %f\n", label, (double)(end-begin)/CLOCKS_PER_SEC); } while(0)
#else
#define START do {
#define END(label) } while (0)
#endif

static void
gaussian_blur (cairo_surface_t *s, double r)
{
  int *boxes = boxes_for_gaussian (r, 3);
  printf ("Boxes = %d %d %d\n", boxes[0], boxes[1], boxes[2]);

  int w = cairo_image_surface_get_width (s);
  int h = cairo_image_surface_get_height (s);

  cairo_surface_t *t = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w, h);

  v4uc *sdata = (v4uc *) cairo_image_surface_get_data (s);
  v4uc *tdata = (v4uc *) cairo_image_surface_get_data (t);

  START;
  box_blur_h (sdata, tdata, w, h, (boxes[0] - 1.0) / 2.0);
  END("h1");

  START;
  box_blur_t (tdata, sdata, w, h, (boxes[0] - 1.0) / 2.0);
  END("t1");

  START;
  box_blur_h (sdata, tdata, w, h, (boxes[1] - 1.0) / 2.0);
  END("h2");

  START;
  box_blur_t (tdata, sdata, w, h, (boxes[1] - 1.0) / 2.0);
  END("t2");

  START;
  box_blur_h (sdata, tdata, w, h, (boxes[2] - 1.0) / 2.0);
  END("h3");

  START;
  box_blur_t (tdata, sdata, w, h, (boxes[2] - 1.0) / 2.0);
  END("t3");

  cairo_surface_destroy (t);
}

int
main (int argc, char *argv[])
{
  cairo_surface_t *source;
  clock_t begin, end;
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

  begin = clock ();
  gaussian_blur (source, 5);
  end = clock ();
  secs = (double)(end - begin) / CLOCKS_PER_SEC;
  printf ("Time = %g secs\n", secs);
  printf ("Average %g ns per pixel\n", secs * 1e9 / w / h);
  printf ("Average %g pixels per ns\n", w * h / (secs * 1e9));

  cairo_surface_write_to_png (source, "output.png");
  cairo_surface_destroy (source);
}
