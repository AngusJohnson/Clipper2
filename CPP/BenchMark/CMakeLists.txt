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
    GetIntersectPtBenchmark.cpp
    PointInPolygonBenchmark.cpp
    StripDuplicateBenchmark.cpp
    # more to add
)

set(CLIPPER2_INC
  ${CLIPPER2_INC_FOLDER}/clipper.h
  ${CLIPPER2_INC_FOLDER}/clipper.version.h
  ${CLIPPER2_INC_FOLDER}/clipper.core.h
)

add_library(Clipper2_bm INTERFACE)
target_include_directories(Clipper2_bm INTERFACE CLIPPER2_INC)

set(CLIPPER2_UTILS_INC
  ../Utils/clipper.svg.h
  ../Utils/ClipFileLoad.h
  ../Utils/ClipFileSave.h
  ../Utils/Timer.h
  ../Utils/Colors.h
  ../Utils/CommonUtils.h
)
set(CLIPPER2_UTILS_SRC
  ../Utils/clipper.svg.cpp
  ../Utils/ClipFileLoad.cpp
  ../Utils/ClipFileSave.cpp
)
set(CLIPPER2_UTILS "")
  list(APPEND CLIPPER2_UTILS Clipper2utils_bm)
  add_library(Clipper2utils_bm STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})
  target_include_directories(Clipper2utils_bm
    PUBLIC ../Clipper2Lib/include
    PUBLIC ../Utils
  )
  target_link_libraries(Clipper2utils_bm PUBLIC Clipper2_bm)

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
        Clipper2_bm
        Clipper2utils_bm
    )
endforeach()
