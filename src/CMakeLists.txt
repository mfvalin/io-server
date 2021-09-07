cmake_minimum_required(VERSION 3.16)

# What to compile
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.F90 *.F *.f *.f90)

# Headers to generate
set(C_TO_INC_FILES
  circular_buffer.c
  distributed_circular_buffer.c
  memory_arena.c
  rpn_extra.c
  timer.c
)

set(C_TO_H_FILES
  circular_buffer.c
  distributed_circular_buffer.c
  memory_arena.c
  rpn_extra.c
  shmem_heap.c
  timer.c
)

set(F_ALLOC_FILE ${CMAKE_CURRENT_SOURCE_DIR}/io-server/f_alloc.inc)

file(GLOB PROJECT_INCLUDE_FILES
  io-server/*.h
  io-server/*.inc
  io-server/*.hf
)

# Fortran headers
set(EXTRACT_FORTRAN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_fortran_interfaces.sh)
foreach(C_FILE ${C_TO_INC_FILES})
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/io-server/${FILE_NAME}.inc")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_FORTRAN_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_FORTRAN_SCRIPT})
   list(APPEND PROJECT_INCLUDE_FILES ${OUT_FILE})
endforeach(C_FILE)

# C headers
set(EXTRACT_C_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_c_interfaces.sh)
foreach(C_FILE ${C_TO_H_FILES})
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/io-server/${FILE_NAME}.h")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_C_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_C_SCRIPT})
   list(APPEND PROJECT_INCLUDE_FILES ${OUT_FILE})
endforeach(C_FILE)

add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/io-server/ioserver.inc
   COMMAND ${EXTRACT_FORTRAN_SCRIPT} ${CMAKE_CURRENT_SOURCE_DIR}/ioserver_init.F90 > ${CMAKE_CURRENT_SOURCE_DIR}/io-server/ioserver.inc
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ioserver_init.F90 ${EXTRACT_FORTRAN_SCRIPT}
   )
list(APPEND PROJECT_INCLUDE_FILES io-server/ioserver.inc)

# A bunch of fortran function signatures
set(F_ALLOC_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/shape_f_pointer.sh)
add_custom_command(OUTPUT ${F_ALLOC_FILE} COMMAND ${F_ALLOC_SCRIPT} > ${F_ALLOC_FILE} DEPENDS ${F_ALLOC_SCRIPT})
list(APPEND PROJECT_INCLUDE_FILES ${F_ALLOC_FILE})

# The library itself
add_library(${PROJECT_NAME} ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_INCLUDE_FILES})
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Where to put modules
get_target_property(LIBRARY_DIR ${PROJECT_NAME} BINARY_DIR)
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${LIBRARY_DIR}/mod)
target_include_directories(${PROJECT_NAME} INTERFACE ${LIBRARY_DIR}/mod)

set_target_properties(${PROJECT_NAME} PROPERTIES VERSION ${PROJECT_VERSION})

target_compile_definitions(${PROJECT_NAME} PRIVATE _POSIX_C_SOURCE=199309L)

target_compile_options(
   ${PROJECT_NAME}
   PUBLIC
   $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall -Wextra -Werror -Wno-error=pedantic -Wno-error=unused-function>
   $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wall -Wextra -Werror>
   $<$<COMPILE_LANG_AND_ID:C,Intel>:-Wall -Werror>
   $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-warn all -diag-disable 5268,7025,7416>
   #-g -O0
)

install(TARGETS ${PROJECT_NAME})
