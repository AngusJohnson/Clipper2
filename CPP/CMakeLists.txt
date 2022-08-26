cmake_minimum_required(VERSION 3.10)
project(Clipper2 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(CLIPPER2_UTILS "Build utilities" OFF)
option(CLIPPER2_EXAMPLES "Build examples" OFF)
option(CLIPPER2_TESTS "Build tests" OFF)

add_library(Clipper2 STATIC
  Clipper2Lib/clipper.core.h
  Clipper2Lib/clipper.engine.cpp
  Clipper2Lib/clipper.engine.h
  Clipper2Lib/clipper.h
  Clipper2Lib/clipper.minkowski.h
  Clipper2Lib/clipper.offset.cpp
  Clipper2Lib/clipper.offset.h
)

target_include_directories(Clipper2
  PUBLIC Clipper2Lib
  SYSTEM INTERFACE Clipper2Lib
)

if (WIN32)
  target_compile_options(Clipper2 PRIVATE /W4 /WX)
else()
  target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
  target_link_libraries(Clipper2 PUBLIC -lm)
endif()

if(CLIPPER2_UTILS OR CLIPPER2_TESTS OR CLIPPER2_EXAMPLES)

  add_library(Clipper2utils STATIC
    Utils/clipper.svg.cpp
    Utils/clipper.svg.h
    Utils/ClipFileLoad.cpp
    Utils/ClipFileLoad.h
    Utils/ClipFileSave.cpp
    Utils/ClipFileSave.h
  )
  
  target_link_libraries(Clipper2utils PRIVATE Clipper2)
  target_include_directories(Clipper2utils
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

  # add the Clipper2Z library
  add_library(Clipper2Z STATIC
    Clipper2ZLib/clipper.core.h
    Clipper2ZLib/clipper.engine.cpp
    Clipper2ZLib/clipper.engine.h
    Clipper2ZLib/clipper.h
    Clipper2ZLib/clipper.minkowski.h
    Clipper2ZLib/clipper.offset.cpp
    Clipper2ZLib/clipper.offset.h
  )

  target_include_directories(Clipper2Z
    PUBLIC Clipper2ZLib
    SYSTEM INTERFACE Clipper2ZLib
  )

  if (WIN32)
    target_compile_options(Clipper2Z PRIVATE /W4 /WX)
  else()
    target_compile_options(Clipper2Z PRIVATE -Wall -Wextra -Wpedantic -Werror)
    target_link_libraries(Clipper2Z PUBLIC -lm)
  endif()

  # copy utils
  install( DIRECTORY Utils DESTINATION UtilsZ )
  file(COPY Utils/clipper.svg.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/clipper.svg.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileLoad.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileLoad.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileSave.cpp DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY Utils/ClipFileSave.h DESTINATION UtilsZ FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  add_library(Clipper2Zutils STATIC
    UtilsZ/clipper.svg.cpp
    UtilsZ/clipper.svg.h
    UtilsZ/ClipFileLoad.cpp
    UtilsZ/ClipFileLoad.h
    UtilsZ/ClipFileSave.cpp
    UtilsZ/ClipFileSave.h
  )

  target_link_libraries(Clipper2Zutils PRIVATE Clipper2Z)
  target_include_directories(Clipper2Zutils
    PUBLIC UtilsZ
    SYSTEM INTERFACE UtilsZ
  )
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
  target_compile_definitions(Clipper2Z PRIVATE USINGZ)
  target_compile_definitions(Clipper2Zutils PRIVATE USINGZ)
  target_compile_definitions(UsingZ1 PRIVATE USINGZ)

endif()


if(CLIPPER2_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  if (WIN32)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  endif()
  include(GoogleTest)

  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")

  add_executable(ClipperTests
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

  target_link_libraries(ClipperTests gtest gmock gtest_main Clipper2 Clipper2utils)

  gtest_discover_tests(ClipperTests
        # set a working directory so your project root so that you can find test data via paths relative to the project root
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
        PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
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
