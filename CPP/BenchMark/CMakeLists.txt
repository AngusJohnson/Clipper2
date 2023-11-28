cmake_minimum_required(VERSION 3.15)
project(Clipper2_benchmarks VERSION 1.0 LANGUAGES C CXX)

if(NOT DEFINED CMAKE_CXX_STANDARD OR CMAKE_CXX_STANDARD LESS 17)
    set(CMAKE_CXX_STANDARD 17)
endif()
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# fetch the google benchmark library
include(FetchContent)
set(BENCHMARK_ENABLE_GTEST_TESTS OFF)
set(BENCHMARK_ENABLE_TESTING OFF)
message("start fetching the googlebenchmark")
FetchContent_Declare(googlebenchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG v1.7.1
) 

FetchContent_MakeAvailable(
        googlebenchmark)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
message("fetching is done")

set(benchmark_srcs
    PointInPolygonBenchmark.cpp
    StripDuplicateBenchmark.cpp
    # more to add
)

# add each benchmark from the benchmark_srcs
foreach(benchmark ${benchmark_srcs})
    get_filename_component(benchmark_target ${benchmark} NAME_WE)

    message(STATUS "${PROJECT_NAME} add benchmark ${benchmark_target}")
    add_executable(${benchmark_target} ${benchmark})
    target_include_directories(${benchmark_target}
      PUBLIC ../Clipper2Lib/include
      PUBLIC ../Utils        
    )

    target_link_libraries(${benchmark_target}
        benchmark::benchmark
    )
endforeach()
