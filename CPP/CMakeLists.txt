cmake_minimum_required(VERSION 3.10)
project(Clipper2 VERSION 1.0.6 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

option(CLIPPER2_UTILS "Build utilities" ON)
option(CLIPPER2_EXAMPLES "Build examples" ON)
option(CLIPPER2_TESTS "Build tests" ON)
option(USE_EXTERNAL_GTEST "Use system-wide installed GoogleTest" OFF)
option(BUILD_SHARED_LIBS "Build shared libs" OFF)

include(GNUInstallDirs)

set(CLIPPER2_INC
  Clipper2Lib/include/clipper2/clipper.h
  Clipper2Lib/include/clipper2/clipper.core.h
  Clipper2Lib/include/clipper2/clipper.engine.h
  Clipper2Lib/include/clipper2/clipper.export.h
  Clipper2Lib/include/clipper2/clipper.minkowski.h
  Clipper2Lib/include/clipper2/clipper.offset.h
  Clipper2Lib/include/clipper2/clipper.rectclip.h
)

set(CLIPPER2_SRC
  Clipper2Lib/src/clipper.engine.cpp
  Clipper2Lib/src/clipper.offset.cpp
  Clipper2Lib/src/clipper.rectclip.cpp
)

set(PCFILE "${CMAKE_CURRENT_BINARY_DIR}/Clipper2.pc")
set(PCFILEZ "${CMAKE_CURRENT_BINARY_DIR}/Clipper2Z.pc")

# 2d version of Clipper2
add_library(Clipper2 ${CLIPPER2_INC} ${CLIPPER2_SRC})

target_include_directories(Clipper2
  PUBLIC Clipper2Lib/include
)

# Clipper2 but with USINGZ defined
add_library(Clipper2Z ${CLIPPER2_INC} ${CLIPPER2_SRC})

target_compile_definitions(Clipper2Z PUBLIC USINGZ)

target_include_directories(Clipper2Z
  PUBLIC Clipper2Lib/include
)

if (MSVC)
  target_compile_options(Clipper2 PRIVATE /W4 /WX)
  target_compile_options(Clipper2Z PRIVATE /W4 /WX)
else()
  target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2 PUBLIC -lm)

  target_compile_options(Clipper2Z PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2Z PUBLIC -lm)
endif()

set_target_properties(Clipper2 Clipper2Z PROPERTIES FOLDER Libraries
                                         VERSION ${PROJECT_VERSION}
                                         SOVERSION ${PROJECT_VERSION_MAJOR}
                                         PUBLIC_HEADER "${CLIPPER2_INC}"
)

if(CLIPPER2_UTILS OR CLIPPER2_TESTS OR CLIPPER2_EXAMPLES)
  set(CLIPPER2_UTILS_INC
    Utils/clipper.svg.h
    Utils/ClipFileLoad.h
    Utils/ClipFileSave.h
  )
  set(CLIPPER2_UTILS_SRC
    Utils/clipper.svg.cpp
    Utils/ClipFileLoad.cpp
    Utils/ClipFileSave.cpp
  )

  add_library(Clipper2utils STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

  target_link_libraries(Clipper2utils PUBLIC Clipper2)
  target_include_directories(Clipper2utils
    PUBLIC Utils
  )

  add_library(Clipper2Zutils STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

  target_link_libraries(Clipper2Zutils PUBLIC Clipper2Z)
  target_include_directories(Clipper2Zutils
    PUBLIC Utils
  )

  set_target_properties(Clipper2utils Clipper2Zutils PROPERTIES FOLDER Libraries)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(Clipper2utils PRIVATE -Wno-unused-variable -Wno-unused-function)
    target_compile_options(Clipper2Zutils PRIVATE -Wno-unused-variable -Wno-unused-function)
  endif()
endif()

if(CLIPPER2_EXAMPLES)
  ##########################################################################
  ##########################################################################

  add_executable(Benchmarks Examples/Benchmarks/Benchmarks.cpp)
  target_link_libraries(Benchmarks PRIVATE Clipper2 Clipper2utils)

  add_executable(Inflate Examples/Inflate/Inflate.cpp)
  target_link_libraries(Inflate PRIVATE Clipper2 Clipper2utils)

  add_executable(MemLeakTest Examples/MemLeakTest/MemLeakTest.cpp)
  target_link_libraries(MemLeakTest PRIVATE Clipper2 Clipper2utils)

  add_executable(PolygonSamples Examples/PolygonSamples/PolygonSamples.cpp)
  target_link_libraries(PolygonSamples PRIVATE Clipper2 Clipper2utils)

  add_executable(RandomClipping Examples/RandomClipping/RandomClipping.cpp)
  target_link_libraries(RandomClipping PRIVATE Clipper2 Clipper2utils)

  add_executable(RandomClipping2 Examples/RandomClipping2/RandomClipping2.cpp)
  target_link_libraries(RandomClipping2 PRIVATE Clipper2 Clipper2utils)

  add_executable(RectClipping Examples/RectClipping/RectClipping.cpp)
  target_link_libraries(RectClipping PRIVATE Clipper2 Clipper2utils)

  add_executable(SimpleClipping Examples/SimpleClipping/SimpleClipping.cpp)
  target_link_libraries(SimpleClipping PRIVATE Clipper2 Clipper2utils)

  file(COPY Examples/Inflate/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  add_executable(UsingZ Examples/UsingZ/UsingZ.cpp)
  target_link_libraries(UsingZ PRIVATE Clipper2Z Clipper2Zutils)

  set_target_properties(Benchmarks Inflate MemLeakTest PolygonSamples RandomClipping RandomClipping2 RectClipping SimpleClipping UsingZ PROPERTIES FOLDER Examples)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(Benchmarks PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(Inflate PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(MemLeakTest PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(PolygonSamples PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(RandomClipping PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(RandomClipping2 PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(RectClipping PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(SimpleClipping PRIVATE -Wno-unused-result -Wno-unused-function)
    target_compile_options(UsingZ PRIVATE -Wno-unused-result -Wno-unused-function)
  endif()
endif()


if(CLIPPER2_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  if (WIN32)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  endif()
  set(BUILD_GMOCK OFF)
if(USE_EXTERNAL_GTEST)
  find_package(GTest REQUIRED)
else()
  include(GoogleTest)

  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")
  set_target_properties(gtest gtest_main PROPERTIES FOLDER GTest)
endif()
  set(ClipperTests_SRC
    Tests/TestLines.cpp
    Tests/TestOffsetOrientation.cpp
    Tests/TestOrientation.cpp
    Tests/TestPolygons.cpp
    Tests/TestPolytreeHoles1.cpp
    Tests/TestPolytreeHoles2.cpp
    Tests/TestPolytreeIntersection.cpp
    Tests/TestPolytreeUnion.cpp
    Tests/TestRandomPaths.cpp
    Tests/TestRectClip.cpp
    Tests/TestTrimCollinear.cpp
  )
  add_executable(ClipperTests ${ClipperTests_SRC})
  target_link_libraries(ClipperTests gtest gtest_main Clipper2 Clipper2utils)

  add_executable(ClipperTestsZ ${ClipperTests_SRC})
  target_link_libraries(ClipperTestsZ gtest gtest_main Clipper2Z Clipper2Zutils)

  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(ClipperTests PRIVATE -Wno-unused-variable -Wno-unused-function)
    target_compile_options(ClipperTestsZ PRIVATE -Wno-unused-variable -Wno-unused-function)
  endif()

  set_target_properties(ClipperTests ClipperTestsZ PROPERTIES FOLDER Tests)

  gtest_discover_tests(ClipperTests
        # set a working directory so your project root so that you can find test data via paths relative to the project root
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
        PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
  )

  gtest_discover_tests(ClipperTestsZ
    # set a working directory so your project root so that you can find test data via paths relative to the project root
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
    PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
    TEST_SUFFIX "_USINGZ"
  )

  file(COPY ../Tests/PolytreeHoleOwner.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/PolytreeHoleOwner2.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Lines.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Polygons.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()

CONFIGURE_FILE(Clipper2.pc.cmakein "${PCFILE}" @ONLY)
set(PCFILE_LIB_SUFFIX "Z")
CONFIGURE_FILE(Clipper2.pc.cmakein "${PCFILEZ}" @ONLY)

install(TARGETS Clipper2 Clipper2Z
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/clipper2
)
install(FILES ${PCFILE} ${PCFILEZ} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
