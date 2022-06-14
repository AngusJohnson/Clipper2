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
  PUBLIC
    Clipper2Lib
   
  SYSTEM INTERFACE
    Clipper2Lib
)

target_link_libraries(Clipper2
  PRIVATE
    -lm
)
set_target_properties(Clipper2 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(Clipper2 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

add_executable(ConsoleDemo1
  Examples/ConsoleDemo1/ConsoleDemo1.cpp
  Utils/clipper.svg.cpp
  Utils/ClipFileLoad.cpp
  Utils/ClipFileSave.cpp
)
target_include_directories(ConsoleDemo1 PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)
target_link_libraries(ConsoleDemo1 PRIVATE
  -lm
  Clipper2
)
set_target_properties(ConsoleDemo1 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(ConsoleDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

install( FILES  Examples/InflateDemo/rabbit.svg DESTINATION . )
file(COPY Examples/InflateDemo/rabbit.svg DESTINATION ${CMAKE_BINARY_DIR} FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

add_executable(InflateDemo1
  Examples/InflateDemo/InflateDemo1.cpp
  Utils/clipper.svg.cpp
  Utils/ClipFileLoad.cpp
  Utils/ClipFileSave.cpp
)
target_include_directories(InflateDemo1 PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)
target_link_libraries(InflateDemo1 PRIVATE
  -lm
  Clipper2
)
set_target_properties(InflateDemo1 PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(InflateDemo1 PROPERTIES LINK_FLAGS "-Wl,-rpath,${CMAKE_SOURCE_DIR}/lib")

