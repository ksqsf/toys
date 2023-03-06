#include <time.h>
#include <iostream>
#include <opencv2/imgproc.hpp>
#include <opencv2/ximgproc.hpp>
#include <opencv2/imgcodecs.hpp>

#define TIME
#ifdef TIME
#define START do { struct timespec begin, end; clock_gettime(CLOCK_MONOTONIC, &begin);
#define END(label) clock_gettime(CLOCK_MONOTONIC, &end); printf("%s: %f\n", label,  } while(0)
#else
#define START do {
#define END(label) } while (0)
#endif

int
main(int argc, const char *argv[])
{
    if (argc < 2) {
        std::cerr << "Usage: opencv_baseline <PNG_FILE>" << std::endl;
        return 1;
    }
    
    cv::Mat img = cv::imread(argv[1], cv::IMREAD_COLOR);
    cv::Mat dst (img.size(), img.type());
    
    printf ("Image geometry %d x %d\n", img.cols, img.rows);
    
    struct timespec begin, end; 
    clock_gettime(CLOCK_MONOTONIC, &begin);
    cv::GaussianBlur(img, dst, cv::Size(0,0), 5);
    clock_gettime(CLOCK_MONOTONIC, &end);
    double secs = (double) (end.tv_sec - begin.tv_sec) + (double) (end.tv_nsec - begin.tv_nsec) / 1e9;
    printf ("Time = %g secs\n", secs);
    printf ("Average %g ns per pixel\n", secs * 1e9 / (img.rows * img.cols));
    printf ("Average %g pixels per ns\n", (img.rows * img.cols) / (secs * 1e9));
    
    cv::imwrite("output.png", dst);
    
    return 0;
}
