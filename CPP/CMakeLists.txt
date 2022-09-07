cmake_minimum_required(VERSION 3.10)
project(Clipper2 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

option(CLIPPER2_UTILS "Build utilities" ON)
option(CLIPPER2_EXAMPLES "Build examples" ON)
option(CLIPPER2_TESTS "Build tests" ON)

set(CLIPPER2_INC
  Clipper2Lib/clipper.core.h
  Clipper2Lib/clipper.engine.h
  Clipper2Lib/clipper.h
  Clipper2Lib/clipper.minkowski.h
  Clipper2Lib/clipper.offset.h
)

set(CLIPPER2_SRC
  Clipper2Lib/clipper.engine.cpp
  Clipper2Lib/clipper.offset.cpp
)

# 2d version of Clipper2
add_library(Clipper2 STATIC ${CLIPPER2_INC} ${CLIPPER2_SRC})

target_include_directories(Clipper2
  PUBLIC Clipper2Lib
  SYSTEM INTERFACE Clipper2Lib
)

# Clipper2 but with USINGZ defined
add_library(Clipper2Z STATIC ${CLIPPER2_INC} ${CLIPPER2_SRC})

target_compile_definitions(Clipper2Z PUBLIC USINGZ)

target_include_directories(Clipper2Z
  PUBLIC Clipper2Lib
  SYSTEM INTERFACE Clipper2Lib
)

if (WIN32)
  target_compile_options(Clipper2 PRIVATE /W4 /WX)
  target_compile_options(Clipper2Z PRIVATE /W4 /WX)
else()
  target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2 PUBLIC -lm)

  target_compile_options(Clipper2Z PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2Z PUBLIC -lm)
endif()

set_target_properties(Clipper2 Clipper2Z PROPERTIES FOLDER Libraries)

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
    SYSTEM INTERFACE Utils
  )

  add_library(Clipper2Zutils STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

  target_link_libraries(Clipper2Zutils PUBLIC Clipper2Z)
  target_include_directories(Clipper2Zutils
    PUBLIC Utils
    SYSTEM INTERFACE Utils
  )

  set_target_properties(Clipper2utils Clipper2Zutils PROPERTIES FOLDER Libraries)

endif()

if(CLIPPER2_EXAMPLES)
  ##########################################################################
  ##########################################################################

  add_executable(ConsoleDemo1 Examples/ConsoleDemo1/ConsoleDemo1.cpp)
  target_link_libraries(ConsoleDemo1 PRIVATE Clipper2 Clipper2utils)

  add_executable(ConsoleDemo2 Examples/ConsoleDemo2/ConsoleDemo2.cpp)
  target_link_libraries(ConsoleDemo2 PRIVATE Clipper2 Clipper2utils)

  file(COPY Examples/InflateDemo/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  add_executable(InflateDemo1 Examples/InflateDemo/InflateDemo1.cpp)
  target_link_libraries(InflateDemo1 PRIVATE Clipper2 Clipper2utils)

  add_executable(UsingZ1 Examples/UsingZ/UsingZ1.cpp)
  target_link_libraries(UsingZ1 PRIVATE Clipper2Z Clipper2Zutils)

  set_target_properties(ConsoleDemo1 ConsoleDemo2 InflateDemo1 UsingZ1 PROPERTIES FOLDER Examples)
endif()


if(CLIPPER2_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  if (WIN32)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  endif()
  set(BUILD_GMOCK OFF)
  include(GoogleTest)

  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")
  set_target_properties(gtest gtest_main PROPERTIES FOLDER GTest)

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
    Tests/TestTrimCollinear.cpp
  )
  add_executable(ClipperTests ${ClipperTests_SRC})
  target_link_libraries(ClipperTests gtest gtest_main Clipper2 Clipper2utils)

  add_executable(ClipperTestsZ ${ClipperTests_SRC})
  target_link_libraries(ClipperTestsZ gtest gtest_main Clipper2Z Clipper2Zutils)

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
