define double @main(double %x) {
entry:
  %0 = alloca double
  br label %body

body:
  store double %x, double* %0
  %1 = load double, double* %0
  %2 = fadd double %1, 1.0000000e+00
  ret double %2
}