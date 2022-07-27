cmake_minimum_required(VERSION 3.10)
project(Clipper2 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(Clipper2 SHARED
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

target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_link_libraries(Clipper2 PRIVATE -lm)
set_target_properties(Clipper2 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(Clipper2 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

add_library(Clipper2utils SHARED
  Utils/clipper.svg.cpp
  Utils/ClipFileLoad.cpp
  Utils/ClipFileSave.cpp
)
target_link_libraries(Clipper2utils PRIVATE -lm Clipper2)
set_target_properties(Clipper2utils PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(Clipper2utils PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

add_executable(ConsoleDemo1 Examples/ConsoleDemo1/ConsoleDemo1.cpp)
target_include_directories(ConsoleDemo1 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(ConsoleDemo1 PRIVATE -lm Clipper2 Clipper2utils)
set_target_properties(ConsoleDemo1 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(ConsoleDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

install( FILES  Examples/InflateDemo/rabbit.svg DESTINATION . )
file(COPY Examples/InflateDemo/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

add_executable(InflateDemo1 Examples/InflateDemo/InflateDemo1.cpp)
target_include_directories(InflateDemo1 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(InflateDemo1 PRIVATE -lm Clipper2 Clipper2utils)
set_target_properties(InflateDemo1 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(InflateDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")


option(PACKAGE_TESTS "Build the tests" ON)
if(PACKAGE_TESTS)
  # See: https://cliutils.gitlab.io/modern-cmake/chapters/testing/googletest.html

  enable_testing()
  include(GoogleTest)
  add_subdirectory("${PROJECT_SOURCE_DIR}/Tests/googletest/")

  macro(package_add_test TESTNAME FILES )
    # create an executable in which the tests will be stored
    add_executable(${TESTNAME} ${FILES})
    # link the Google test infrastructure, mocking library, and a default main function to
    # the test executable.  Remove g_test_main if writing your own main function.
    target_link_libraries(${TESTNAME} gtest gmock gtest_main Clipper2 Clipper2utils)
    # gtest_discover_tests replaces gtest_add_tests,
    # see https://cmake.org/cmake/help/v3.10/module/GoogleTest.html for more options to pass to it
    gtest_discover_tests(${TESTNAME}
        # set a working directory so your project root so that you can find test data via paths relative to the project root
        WORKING_DIRECTORY ${PROJECT_DIR}
        PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
    )
    set_target_properties(${TESTNAME} PROPERTIES FOLDER tests)
  endmacro()

  package_add_test(test1 Tests/Tests/TestLines.cpp )
  package_add_test(test2 Tests/Tests/TestOffsetOrientation.cpp )
  package_add_test(test3 Tests/Tests/TestOrientation.cpp )
  package_add_test(test4 Tests/Tests/TestPolygons.cpp )
  package_add_test(test5 Tests/Tests/TestPolytreeHoles1.cpp )
  package_add_test(test6 Tests/Tests/TestPolytreeHoles2.cpp )
  package_add_test(test7 Tests/Tests/TestPolytreeIntersection.cpp )
  package_add_test(test8 Tests/Tests/TestPolytreeUnion.cpp )
  package_add_test(test9 Tests/Tests/TestRandomPaths.cpp )
  package_add_test(test10 Tests/Tests/TestTrimCollinear.cpp )

  install( FILES  ../Tests/PolytreeHoleOwner.txt DESTINATION . )
  install( FILES  ../Tests/PolytreeHoleOwner2.txt DESTINATION . )
  install( FILES  ../Tests/Lines.txt DESTINATION . )
  install( FILES  ../Tests/Polygons.txt DESTINATION . )

  file(COPY ../Tests/PolytreeHoleOwner.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/PolytreeHoleOwner2.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Lines.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Polygons.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()
