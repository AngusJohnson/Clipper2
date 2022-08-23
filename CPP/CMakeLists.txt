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
  target_link_libraries(Clipper2 PRIVATE -lm)
  set_target_properties(Clipper2 PROPERTIES LINKER_LANGUAGE CXX)
  set_target_properties(Clipper2 PROPERTIES LIBRARY_OUTPUT_DIRECTORY "./lib")
  set_target_properties(Clipper2 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")
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
  
  if (WIN32)
    target_link_libraries(Clipper2utils PRIVATE Clipper2)
    target_include_directories(Clipper2utils
      PUBLIC Utils
      SYSTEM INTERFACE Utils
    )
  else()
    target_link_libraries(Clipper2utils PRIVATE -lm Clipper2)
    set_target_properties(Clipper2utils PROPERTIES LINKER_LANGUAGE CXX)
    set_target_properties(Clipper2utils PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")
    target_include_directories(Clipper2utils
      PUBLIC Utils
      SYSTEM INTERFACE Utils
    )
  endif()
endif()

if(CLIPPER2_EXAMPLES)
  add_executable(ConsoleDemo1 Examples/ConsoleDemo1/ConsoleDemo1.cpp)
  
  if (WIN32)
    # target_include_directories(ConsoleDemo1 PRIVATE Clipper2Lib)
    target_link_libraries(ConsoleDemo1 PRIVATE Clipper2 Clipper2utils)
  else()
    target_include_directories(ConsoleDemo1 PRIVATE Clipper2Lib)
    target_link_libraries(ConsoleDemo1 PRIVATE -lm Clipper2 Clipper2utils)
    set_target_properties(ConsoleDemo1 PROPERTIES LINKER_LANGUAGE CXX)
    set_target_properties(ConsoleDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")
  endif()

  install( FILES  Examples/InflateDemo/rabbit.svg DESTINATION . )
  file(COPY Examples/InflateDemo/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

  add_executable(InflateDemo1 Examples/InflateDemo/InflateDemo1.cpp)
  target_include_directories(InflateDemo1 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  if (WIN32)
    target_link_libraries(InflateDemo1 PRIVATE Clipper2 Clipper2utils)
  else()
    target_link_libraries(InflateDemo1 PRIVATE Clipper2 Clipper2utils)
    set_target_properties(InflateDemo1 PROPERTIES LINKER_LANGUAGE CXX)
    set_target_properties(InflateDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")
  endif()
endif()


if(CLIPPER2_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  include(GoogleTest)
  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")

  add_executable(tests
    Tests/Tests/TestLines.cpp
    Tests/Tests/TestOffsetOrientation.cpp
    Tests/Tests/TestOrientation.cpp
    Tests/Tests/TestPolygons.cpp
    Tests/Tests/TestPolytreeHoles1.cpp
    Tests/Tests/TestPolytreeHoles2.cpp
    Tests/Tests/TestPolytreeIntersection.cpp
    Tests/Tests/TestPolytreeUnion.cpp
    Tests/Tests/TestRandomPaths.cpp
    Tests/Tests/TestTrimCollinear.cpp
  )
  target_link_libraries(tests gtest gmock gtest_main Clipper2 Clipper2utils)
  gtest_discover_tests(tests
        # set a working directory so your project root so that you can find test data via paths relative to the project root
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
        PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
  )
  set_target_properties(tests PROPERTIES FOLDER tests)

  install( FILES  ../Tests/PolytreeHoleOwner.txt DESTINATION . )
  install( FILES  ../Tests/PolytreeHoleOwner2.txt DESTINATION . )
  install( FILES  ../Tests/Lines.txt DESTINATION . )
  install( FILES  ../Tests/Polygons.txt DESTINATION . )

  file(COPY ../Tests/PolytreeHoleOwner.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/PolytreeHoleOwner2.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Lines.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Polygons.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()
