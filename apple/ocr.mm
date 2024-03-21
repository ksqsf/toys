// ocr.mm
//
// This is a small utility that uses the native Vision framework to
// recognize human language texts in JPEG files.

#import <Vision/Vision.h>
#import <CoreGraphics/CoreGraphics.h>
#include <cstdio>

int main(int argc, char *argv[])
{
    if (argc < 2) {
        printf("give me a file name!\n");
        return 1;
    }
    auto provider = CGDataProviderCreateWithFilename(argv[1]);
    if (!provider) {
        printf("cannot open file!\n");
        return 2;
    }
    auto cgImage = CGImageCreateWithJPEGDataProvider(provider, nullptr, true, kCGRenderingIntentDefault);
    if (!cgImage) {
        printf("cannot create cgimage!\n");
        return 3;
    }
    auto requestHandler = [[VNImageRequestHandler alloc] initWithCGImage:cgImage options:@{}];
    auto request = [VNRecognizeTextRequest new];
    request.recognitionLevel = VNRequestTextRecognitionLevelAccurate;
    request.automaticallyDetectsLanguage = YES;
    request.usesLanguageCorrection = YES;
    auto requests = @[request];
    NSError *error = nullptr;
    if ([requestHandler performRequests:requests error:&error] == YES) {
        printf("OK!\n");
        for (VNRecognizedTextObservation *observation in [request results]) {
            auto topCandidate = [[observation topCandidates:1] firstObject];
            if (topCandidate) {
                NSString *recognizedText = topCandidate.string;
                printf("%s\n", [recognizedText cStringUsingEncoding:NSUTF8StringEncoding]);
            }
        }
    } else {
        printf("%s\n", [[error localizedDescription] cStringUsingEncoding:NSUTF8StringEncoding]);
    }
    CGImageRelease(cgImage);
    CGDataProviderRelease(provider);
}
