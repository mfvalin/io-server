cmake_minimum_required(VERSION 3.16)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

include(ec_init)
include(ec_parse_manifest)

ec_parse_manifest()
project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")

enable_testing()

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
   set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/install CACHE PATH "..." FORCE)
endif()

enable_language(Fortran)
enable_language(C)

include(ec_compiler_presets)

find_package(MPI)
if (MPI_FOUND)
   set(CMAKE_Fortran_COMPILER ${MPI_Fortran_COMPILER})
   set(CMAKE_C_COMPILER ${MPI_C_COMPILER})
else()
   find_program(RPN_MPICC mpicc)
   find_program(RPN_MPIF90 mpif90)
   if (${RPN_MPICC} STREQUAL "RPN_MPICC-NOTFOUND" OR ${RPN_MPIF90} STREQUAL "RPN_MPIF90-NOTFOUND")
      # message(FATAL_ERROR "Did not find a compiler for mpi. (Just looking for mpicc and mpif90)")
      find_program(RPN_MPICC cc)
      find_program(RPN_MPIF90 ftn)
   endif()

   message(STATUS "RPN_MPICC = ${RPN_MPICC}")
   message(STATUS "RPN_MPIF90 = ${RPN_MPIF90}")

   set(CMAKE_Fortran_COMPILER ${RPN_MPIF90})
   set(CMAKE_C_COMPILER ${RPN_MPICC})
endif()

option(BUILD_DOC "Build the Doxygen documentation" FALSE)
if (BUILD_DOC)
  include(doxygen)
endif()

add_subdirectory(src)
add_subdirectory(tests)

