// g++ compute.cpp -march=native

#include <bits/stdc++.h>
using namespace std;

const int maxn = 1000;
unsigned long fib[maxn];

typedef float(*mathfunc1d_t)(float);

float myfunc1d(float);

// ----------------------------------------
// 1D Search Technique: Fibonacci method
// ----------------------------------------
void init_fib() {
    fib[0] = fib[1] = 1;
    for (int i = 2; i < maxn; ++i) {
        fib[i] = fib[i-2] + fib[i-1];
    }
}

float do_search_fib(mathfunc1d_t func, float a, float b, int n)
{
    float a1, b1, fa1, fb1, eps=0.01;
    a1 = a + ((float) fib[n-2])/fib[n] * (b-a);
    b1 = b - ((float) fib[n-2])/fib[n] * (b-a);
    fa1 = func(a1);
    fb1 = func(b1);
    printf("r:[%f,%f], f:[%f,%f]\n", a1, b1, fa1, fb1);

    while (n > 2) {
        n--;
        if (fa1 > fb1) {
            // next: [a1, b]
            a = a1;
            a1 = b1;
            fa1 = fb1;
            b1 = b - ((float) fib[n-2])/fib[n] * (b-a);
            fb1 = func(b1);
            printf("r:[%f,%f], f:[%f,%f]\n", a1, b1, fa1, fb1);
        } else {
            // next: [a, b1]
            b = b1;
            b1 = a1;
            fb1 = fa1;
            a1 = a + ((float) fib[n-2])/fib[n] * (b-a);
            fa1 = func(a1);
            printf("r:[%f,%f], f:[%f,%f]\n", a1, b1, fa1, fb1);
        }
    }

    a1 = (a+b)/2;
    b1 = a+(0.5 + eps)*(b-a);
    fa1 = func(a1);
    fb1 = func(b1);
    printf("final r: [%f,%f], f: [%f,%f]\n", a1, b1, fa1, fb1);
    return min(fa1, fb1);
}

// 搜索区间 [a,b]
// 绝对精度 eta
float search_fib(mathfunc1d_t func, float a, float b, float eta)
{
    float delta = eta / (b-a);
    int n = lower_bound(fib, fib+maxn, 1/delta) - fib;
    printf("%d points are required!\n", n);
    printf("d=%f, 1/d=%f, fib[%d]=%lu\n", delta, 1/delta, n, fib[n]);
    return do_search_fib(func, a, b, n);
}

// ----------------------------------------
// 1D Search Technique: Golden-ratio Section
// ----------------------------------------
float search_gr(mathfunc1d_t func, float a, float b, float eps)
{
    float r = 0.618;
    float p = a+(1-r)*(b-a), q = a+r*(b-a);
    float fp = func(p), fq = func(q);
    int n = log(eps) / log(r) + 2;
    printf("%d iterations are required\n", n);
    while (n--) {
        if (fp > fq) {
            a = p;
            p = q;
            fp = fq;
            q = a+r*(b-a);
            fq = func(q);
        } else {
            b = q;
            q = p;
            fq = fp;
            p = a+(1-r)*(b-a);
            fp = func(p);
        }
        printf("r:[%f,%f], f:[%f,%f]\n", p, q, fp, fq);
    }
    printf("end: f(%f)=%f\n", (p+q)/2, func((p+q)/2));
    return func((p+q)/2);
}

int main()
{
    init_fib();

    float m = search_fib(myfunc1d, -1, 3, 0.01);
    printf("%f\n", m);

    m = search_gr(myfunc1d, -1, 3, 0.01);
    printf("%f\n", m);

    return 0;
}

float myfunc1d(float x)
{
    return x*x-x+2;
}
