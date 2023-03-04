CC := /usr/local/opt/llvm/bin/clang
# CFLAGS := `pkg-config --cflags --libs cairo`  -O3

CFLAGS := `pkg-config --cflags --libs cairo`  -Ofast -march=native -mavx2 -fopenmp

ALL: fast_gaussian_blur fast_gaussian_blur_transpose

clean:
	rm -f output.png fast_gaussian_blur

.PHONY: clean
