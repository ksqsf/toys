CC := /usr/local/opt/llvm/bin/clang
CXX := /usr/local/opt/llvm/bin/clang++
# CFLAGS := `pkg-config --cflags --libs cairo`  -O3

CFLAGS := `pkg-config --cflags --libs cairo`  -Ofast -march=native -fopenmp
CXXFLAGS := -I/usr/local/include/opencv4 -L/usr/local/lib -lopencv_core -lopencv_imgproc -lopencv_imgcodecs -Ofast -march=native -fopenmp

ALL: fast_gaussian_blur fast_gaussian_blur_transpose opencv_baseline consumer

consumer: consumer.c provider.cc
	$(CXX) -c provider.cc -I/usr/local/include/opencv4 -Ofast -march=native -mavx2 -fopenmp
	$(CC) -c consumer.c  `pkg-config --cflags cairo` -Ofast -march=native -mavx2 -fopenmp
	$(CXX) `pkg-config --libs cairo` -L/usr/local/lib -lopencv_core -lopencv_imgproc provider.o consumer.o -o consumer

clean:
	rm -f output.png fast_gaussian_blur fast_gaussian_blur_transpose opencv_baseline *.o consumer

.PHONY: clean
