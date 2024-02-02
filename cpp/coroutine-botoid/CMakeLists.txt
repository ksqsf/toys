cmake_minimum_required(VERSION 3.27)
project(botoid)
set(CMAKE_CXX_STANDARD 20)

set(CMAKE_CXX_FLAGS "-Wall -fsanitize=address -fexperimental-library")  # std::format is still experimental

# dependencies
find_package(PkgConfig REQUIRED)
pkg_check_modules(NlohmannJson REQUIRED IMPORTED_TARGET "nlohmann_json")
find_library(Curlpp "curlpp" REQUIRED)

# libbotoid
add_library(botoid STATIC botoid.cpp)
target_include_directories(botoid PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}" "/usr/local/include")
target_link_libraries(botoid PkgConfig::NlohmannJson -lcurl -lcurlpp -L/usr/local/lib)

# example program
add_executable(example main.cpp)
target_link_libraries(example botoid)
