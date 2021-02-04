cmake_minimum_required(VERSION 3.16)

include(cmake_shared/git_version.cmake)
include(cmake_shared/dump_cmake_variables.cmake)

project(io-server-test VERSION ${VERSION} DESCRIPTION "Tests for the IO-server library" LANGUAGES NONE)

set(COMPILER_SUITE "GNU" CACHE STRING "Compiler suite to use for the build.  Can be one of the following: GNU, Intel, XL")

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

include(gen_test_target.cmake)


# WHAT TO COMPILE

gen_test_target(NAME cb_plus_dcb               INPUT_FILES cb_plus_dcb.F90)
gen_test_target(NAME circ_buffer               INPUT_FILES circular_buffer.F90)
gen_test_target(NAME circ_buffer_fill          INPUT_FILES circular_buffer_fill.c)
gen_test_target(NAME circ_buffer_single_thread INPUT_FILES circular_buffer_single_thread.F90)
gen_test_target(NAME distributed_circ_buffer   INPUT_FILES distributed_circular_buffer.F90)
gen_test_target(NAME small_dcb_test            INPUT_FILES small_dcb_test.c)

gen_test_target(NAME f_memory_arena            INPUT_FILES f_memory_arena.F90)
#gen_test_target(NAME memory_arena              INPUT_FILES memory_arena.F90)
gen_test_target(NAME pseudo-model              INPUT_FILES pseudo-model.F90)
gen_test_target(NAME pseudo-server             INPUT_FILES pseudo-server.F90)
gen_test_target(NAME shmem_heap                INPUT_FILES shmen_heap.F90)
