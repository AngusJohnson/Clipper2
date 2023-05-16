cmake_minimum_required(VERSION 3.15)
project(Clipper2 VERSION 1.2.2 LANGUAGES C CXX)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

option(CLIPPER2_UTILS "Build utilities" ON)
option(CLIPPER2_EXAMPLES "Build examples" ON)
option(CLIPPER2_TESTS "Build tests" ON)
option(USE_EXTERNAL_GTEST "Use system-wide installed GoogleTest" OFF)
option(USE_EXTERNAL_GBENCHMARK "Use the googlebenchmark" OFF) 
option(BUILD_SHARED_LIBS "Build shared libs" OFF)
set(CLIPPER2_USINGZ "ON" CACHE STRING "Build Clipper2Z, either \"ON\" or \"OFF\" or \"ONLY\"")

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

set(CLIPPER2_LIBS "") # one or both of Clipper2/Clipper2Z

# 2d version of Clipper2
if (NOT (CLIPPER2_USINGZ STREQUAL "ONLY"))
  list(APPEND CLIPPER2_LIBS Clipper2)
  add_library(Clipper2 ${CLIPPER2_INC} ${CLIPPER2_SRC})

  target_include_directories(Clipper2
    PUBLIC Clipper2Lib/include
  )

  if (MSVC)
    target_compile_options(Clipper2 PRIVATE /W4 /WX)
  else()
    target_compile_options(Clipper2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
    target_link_libraries(Clipper2 PUBLIC -lm)
  endif()
endif()

# Clipper2 but with USINGZ defined
if (NOT (CLIPPER2_USINGZ STREQUAL "OFF"))
  list(APPEND CLIPPER2_LIBS Clipper2Z)
  add_library(Clipper2Z ${CLIPPER2_INC} ${CLIPPER2_SRC})

  target_compile_definitions(Clipper2Z PUBLIC USINGZ)

  target_include_directories(Clipper2Z
    PUBLIC Clipper2Lib/include
  )

  if (MSVC)
    target_compile_options(Clipper2Z PRIVATE /W4 /WX)
  else()
    target_compile_options(Clipper2Z PRIVATE -Wall -Wextra -Wpedantic -Werror)
    target_link_libraries(Clipper2Z PUBLIC -lm)
  endif()
endif()

set_target_properties(${CLIPPER2_LIBS} PROPERTIES FOLDER Libraries
                                         VERSION ${PROJECT_VERSION}
                                         SOVERSION ${PROJECT_VERSION_MAJOR}
                                         PUBLIC_HEADER "${CLIPPER2_INC}"
)

if(CLIPPER2_UTILS OR CLIPPER2_TESTS OR CLIPPER2_EXAMPLES)
  set(CLIPPER2_UTILS_INC
    Utils/clipper.svg.h
    Utils/ClipFileLoad.h
    Utils/ClipFileSave.h
    Utils/Timer.h
    Utils/Colors.h
    Utils/CommonUtils.h
  )
  set(CLIPPER2_UTILS_SRC
    Utils/clipper.svg.cpp
    Utils/ClipFileLoad.cpp
    Utils/ClipFileSave.cpp
  )
  set(CLIPPER2_UTILS "") # one or both of Clipper2utils/Clipper2Zutils

  if (NOT (CLIPPER2_USINGZ STREQUAL "ONLY"))
    list(APPEND CLIPPER2_UTILS Clipper2utils)
    add_library(Clipper2utils STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

    target_link_libraries(Clipper2utils PUBLIC Clipper2)
    target_include_directories(Clipper2utils
      PUBLIC Utils
    )
  endif()

  if (NOT (CLIPPER2_USINGZ STREQUAL "OFF"))
    list(APPEND CLIPPER2_UTILS Clipper2Zutils)
    add_library(Clipper2Zutils STATIC ${CLIPPER2_UTILS_INC} ${CLIPPER2_UTILS_SRC})

    target_link_libraries(Clipper2Zutils PUBLIC Clipper2Z)
    target_include_directories(Clipper2Zutils
      PUBLIC Utils
    )
  endif()

  set_target_properties(${CLIPPER2_UTILS} PROPERTIES FOLDER Libraries)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    foreach(lib ${CLIPPER2_UTILS})
      target_compile_options(${lib} PRIVATE -Wno-unused-variable -Wno-unused-function)
    endforeach()
  endif()
endif()

if(CLIPPER2_EXAMPLES)
  ##########################################################################
  ##########################################################################

  set(ALL_EXAMPLES "") # 2d and 3d examples (if enabled)

  if (NOT (CLIPPER2_USINGZ STREQUAL "ONLY"))
    set(EXAMPLES
      Benchmarks
      Inflate
      MemLeakTest
      PolygonSamples
      RandomClipping
      UnionClipping
      RectClipping
      SimpleClipping
      VariableOffset
    )

    foreach(ex ${EXAMPLES})
      add_executable(${ex} Examples/${ex}/${ex}.cpp)
      target_link_libraries(${ex} PRIVATE Clipper2 Clipper2utils)
    endforeach()

    file(COPY Examples/Inflate/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
    list(APPEND ALL_EXAMPLES ${EXAMPLES})
  endif()

  if (NOT (CLIPPER2_USINGZ STREQUAL "OFF"))
    set(EXAMPLESZ "UsingZ")
    foreach(ex ${EXAMPLESZ})
      add_executable(${ex} Examples/${ex}/${ex}.cpp)
      target_link_libraries(${ex} PRIVATE Clipper2Z Clipper2Zutils)
    endforeach()

    list(APPEND ALL_EXAMPLES ${EXAMPLESZ})
  endif()

  set_target_properties(${ALL_EXAMPLES} PROPERTIES FOLDER Examples)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    foreach(ex ${ALL_EXAMPLES})
      target_compile_options(${ex} PRIVATE -Wno-unused-variable -Wno-unused-function)
    endforeach()
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
    Tests/TestOffsets.cpp
    Tests/TestOffsetOrientation.cpp
    Tests/TestOrientation.cpp
    Tests/TestPolygons.cpp
    Tests/TestPolytreeHoles1.cpp
    Tests/TestPolytreeHoles2.cpp
    Tests/TestPolytreeHoles3.cpp
    Tests/TestPolytreeHoles4.cpp
    Tests/TestPolytreeIntersection.cpp
    Tests/TestPolytreeUnion.cpp
    Tests/TestRandomPaths.cpp
    Tests/TestRectClip.cpp
    Tests/TestTrimCollinear.cpp
  )

  set(CLIPPER2_TESTS "") # one or both of ClipperTests/ClipperTestsZ

  if (NOT (CLIPPER2_USINGZ STREQUAL "ONLY"))
    list(APPEND CLIPPER2_TESTS "ClipperTests")
    add_executable(ClipperTests ${ClipperTests_SRC})
    target_link_libraries(ClipperTests gtest gtest_main Clipper2 Clipper2utils)

    gtest_discover_tests(ClipperTests
      # set a working directory so your project root so that you can find test data via paths relative to the project root
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
      PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
    )
  endif()

  if (NOT (CLIPPER2_USINGZ STREQUAL "OFF"))
    list(APPEND CLIPPER2_TESTS "ClipperTestsZ")
    add_executable(ClipperTestsZ ${ClipperTests_SRC})
    target_link_libraries(ClipperTestsZ gtest gtest_main Clipper2Z Clipper2Zutils)

    gtest_discover_tests(ClipperTestsZ
      # set a working directory so your project root so that you can find test data via paths relative to the project root
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../Tests
      PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${PROJECT_DIR}"
      TEST_SUFFIX "_USINGZ"
    )
  endif()

  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    foreach(ts ${CLIPPER2_TESTS})
      target_compile_options(${ts} PRIVATE -Wno-unused-variable -Wno-unused-function)
    endforeach()
  endif()

  set_target_properties(${CLIPPER2_TESTS} PROPERTIES FOLDER Tests)

  file(COPY ../Tests/PolytreeHoleOwner.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/PolytreeHoleOwner2.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Lines.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
  file(COPY ../Tests/Polygons.txt DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()

if(USE_EXTERNAL_GBENCHMARK)
  add_subdirectory(BenchMark)
endif()

set(CLIPPER2_PCFILES "")
foreach(lib ${CLIPPER2_LIBS})
  set(pc "${CMAKE_CURRENT_BINARY_DIR}/${lib}.pc")
  list(APPEND CLIPPER2_PCFILES ${pc})
  CONFIGURE_FILE(Clipper2.pc.cmakein "${pc}" @ONLY)
endforeach()

install(TARGETS ${CLIPPER2_LIBS}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/clipper2
)
install(FILES ${CLIPPER2_PCFILES} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

# disable exceptions
#string(REGEX REPLACE "/W[3|4]" "/w" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
#add_definitions(-D_HAS_EXCEPTIONS=0) # for STL
