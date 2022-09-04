cmake_minimum_required(VERSION 3.10)
project(Clipper2 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(CLIPPER2_STATIC "Build static libraries" ON)
option(CLIPPER2_UTILS "Build utilities" ON)
option(CLIPPER2_EXAMPLES "Build examples" ON)
option(CLIPPER2_TESTS "Build tests" ON)

if (CLIPPER2_STATIC)
  set(CLIPPER2_VARIANT STATIC)
else()
  set(CLIPPER2_VARIANT SHARED)
endif()

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
add_library(Clipper2 ${CLIPPER2_VARIANT} ${CLIPPER2_INC} ${CLIPPER2_SRC})

target_include_directories(Clipper2
  PUBLIC Clipper2Lib
  SYSTEM INTERFACE Clipper2Lib
)

# Clipper2 but with the ability to accommodate 3d points
add_library(Clipper2Z ${CLIPPER2_VARIANT} ${CLIPPER2_INC} ${CLIPPER2_SRC})
target_include_directories(Clipper2Z
  PUBLIC Clipper2Lib
  SYSTEM INTERFACE Clipper2Lib
)
target_compile_definitions(Clipper2Z PUBLIC USINGZ)

if (WIN32)
  target_compile_options(Clipper2 PRIVATE /W4 /WX)
  target_compile_options(Clipper2Z PRIVATE /W4 /WX)
else()
  target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2 PUBLIC -lm)

  target_compile_options(Clipper2Z PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2Z PUBLIC -lm)
endif()

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

  add_library(Clipper2utils ${CLIPPER2_VARIANT} ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})
  
  target_link_libraries(Clipper2utils PUBLIC Clipper2)
  target_include_directories(Clipper2utils
    PUBLIC Utils
    SYSTEM INTERFACE Utils
  )

  add_library(Clipper2Zutils ${CLIPPER2_VARIANT} ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

  target_link_libraries(Clipper2Zutils PUBLIC Clipper2Z)
  target_include_directories(Clipper2Zutils
    PUBLIC Utils
    SYSTEM INTERFACE Utils
  )
endif()

if(CLIPPER2_EXAMPLES)

  ############################################################################
  # USINGZ
  ############################################################################

  # copy Clipper2Lib to Clipper2ZLib where it will be compiled with USINGZ
  install( DIRECTORY Clipper2Lib DESTINATION Clipper2ZLib )
  file(COPY Clipper2Lib/clipper.core.h DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.engine.h DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.engine.cpp DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.h DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.minkowski.h DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.offset.h DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Clipper2Lib/clipper.offset.cpp DESTINATION Clipper2ZLib FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  # copy utils
  install( DIRECTORY Utils DESTINATION UtilsZ )
  file(COPY Utils/clipper.svg.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/clipper.svg.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileLoad.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileLoad.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileSave.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileSave.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )


  ##########################################################################
  ##########################################################################

  add_executable(ConsoleDemo1 Examples/ConsoleDemo1/ConsoleDemo1.cpp)
  target_link_libraries(ConsoleDemo1 PRIVATE Clipper2 Clipper2utils)

  add_executable(ConsoleDemo2 Examples/ConsoleDemo2/ConsoleDemo2.cpp)
  target_link_libraries(ConsoleDemo2 PRIVATE Clipper2 Clipper2utils)

  install( FILES  Examples/InflateDemo/rabbit.svg DESTINATION . )
  file(COPY Examples/InflateDemo/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  add_executable(InflateDemo1 Examples/InflateDemo/InflateDemo1.cpp)
  target_link_libraries(InflateDemo1 PRIVATE Clipper2 Clipper2utils)

  add_executable(UsingZ1 Examples/UsingZ/UsingZ1.cpp)
  target_link_libraries(UsingZ1 PRIVATE Clipper2Z Clipper2Zutils)
endif()


if(CLIPPER2_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  if (WIN32)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  endif()
  include(GoogleTest)

  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")

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
  
  add_executable(ClipperTestsZ ${ClipperTests_SRC})

  target_link_libraries(ClipperTests gtest gmock gtest_main Clipper2 Clipper2utils)
  target_link_libraries(ClipperTestsZ gtest gmock gtest_main Clipper2Z Clipper2Zutils)

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

  install( FILES  ../Tests/PolytreeHoleOwner.txt DESTINATION . )
  install( FILES  ../Tests/PolytreeHoleOwner2.txt DESTINATION . )
  install( FILES  ../Tests/Lines.txt DESTINATION . )
  install( FILES  ../Tests/Polygons.txt DESTINATION . )

  file(COPY ../Tests/PolytreeHoleOwner.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/PolytreeHoleOwner2.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Lines.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Polygons.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()
