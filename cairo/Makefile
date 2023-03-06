CC := /usr/local/opt/llvm/bin/clang
CXX := /usr/local/opt/llvm/bin/clang++
# CFLAGS := `pkg-config --cflags --libs cairo`  -O3

CFLAGS := `pkg-config --cflags --libs cairo`  -Ofast -march=native -mavx2 -fopenmp
CXXFLAGS := -I/usr/local/include/opencv4 -L/usr/local/lib -lopencv_core -lopencv_imgproc -lopencv_imgcodecs -Ofast -march=native -mavx2 -fopenmp

ALL: fast_gaussian_blur fast_gaussian_blur_transpose opencv_baseline

clean:
	rm -f output.png fast_gaussian_blur opencv_baseline

.PHONY: clean
