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
    StripDuplicateBenchmark.cpp
    # more to add
)

# add each benchmark from the benchmark_srcs
foreach(benchmark ${benchmark_srcs})
    get_filename_component(benchmark_target ${benchmark} NAME_WE)

    message(STATUS "${PROJECT_NAME} add benchmark ${benchmark_target}")
    add_executable(${benchmark_target} ${benchmark})
    target_include_directories(${benchmark_target}  PUBLIC
        
    )

    target_link_libraries(${benchmark_target}
        benchmark::benchmark
        Clipper2
        Clipper2utils
    )
endforeach()
