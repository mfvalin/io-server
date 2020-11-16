# This must be repeated in each CMake file; having it in an included file is not
# sufficient
cmake_minimum_required(VERSION 3.16)

include(cmake_shared/git_version.cmake)
include(cmake_shared/dump_cmake_variables.cmake)

# Given our wacky compiler setups, languages must be activated after the
# compieler has been configured
project(io-server VERSION ${VERSION} DESCRIPTION "Library for efficiently writing data generated by a large cluster" LANGUAGES NONE)

include(cmake_shared/doxygen.cmake)

set(COMPILER_SUITE "GNU" CACHE STRING "Compiler suite to use for the build.  Can be one of the following: GNU, Intel, XL")
option(BUILD_SHARED_LIBS "Build shared libraries instead of static ones." FALSE)

set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (Debug, Release, RelWithDebInfo, MinSizeRel)")

message(STATUS "CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}")
message(STATUS "COMPILER_SUITE=${COMPILER_SUITE}")

string(TOLOWER ${COMPILER_SUITE} COMPILER_SUITE)

#dump_cmake_variables()

# enable_language can not be called here since the compiler must be configured
# first.  We therefore make a list of languages that will be enabled by the
# compiler_presets module
set(LANGUAGES C Fortran)

find_package(MPI)
# Set compiler flags based on the OS/Distro and requested compiler
include(cmake_shared/compiler_presets.cmake)

set(CMAKE_Fortran_COMPILER ${MPI_Fortran_COMPILER})
set(CMAKE_C_COMPILER ${MPI_C_COMPILER})

set(BUILD_ARCH "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Build architecture: ${BUILD_ARCH}")


# BUILD CONFIGURATION

set(CMAKE_VERBOSE_MAKEFILE TRUE)

# WHAT TO COMPILE

file(GLOB PROJECT_C_FILES
  *.c
)

file(GLOB PROJECT_F_FILES
  *.F90
  *.F
  *.f
  *.f90
)

file(GLOB PROJECT_INCLUDE_FILES
  *.h
  *.inc
)

set(C_TO_INC_FILES
  memory_arena.c
  circular_buffer.c
  remote_circular_buffer.c
)

set(C_TO_H_FILES ${C_TO_INC_FILES})

add_library(${PROJECT_NAME} ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_INCLUDE_FILES})
target_include_directories(${PROJECT_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/.)

set_target_properties(${PROJECT_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(${PROJECT_NAME} PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

include(generate_inc.cmake)
include(generate_h.cmake)

install(
  TARGETS ${PROJECT_NAME}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  PUBLIC_HEADER DESTINATION include
)