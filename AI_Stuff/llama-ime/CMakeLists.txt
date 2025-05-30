cmake_minimum_required(VERSION 3.12)
project(myime LANGUAGES CXX)

# llama.cpp
set(LLAMA_METAL OFF CACHE BOOL "Disable Metal" FORCE)
set(LLAMA_NATIVE ON CACHE BOOL "Enable native optimizations" FORCE)
set(LLAMA_ACCELERATE ON CACHE BOOL "" FORCE)
set(GGML_METAL OFF CACHE BOOL "Disable GGML Metal" FORCE)
add_subdirectory(llama.cpp)
add_executable(llama_ime llama_ime.cpp)
target_link_libraries(llama_ime PRIVATE llama)


# fasttext
