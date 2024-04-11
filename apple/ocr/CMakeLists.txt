add_executable(ocr ocr.mm)
target_link_libraries(ocr "-framework Vision" "-framework Cocoa")
target_compile_features(ocr PUBLIC cxx_std_17)
