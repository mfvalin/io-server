cmake_minimum_required(VERSION 3.16)

# What to compile
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.F90 *.F *.f *.f90)

# Headers to generate
set(C_TO_INC_FILES
   circular_buffer.c
   distributed_circular_buffer.c
   rpn_extra.c
   shmem_arena.c
   shmem_heap.c
   timer.c
)

set(C_TO_H_FILES
   circular_buffer.c
   distributed_circular_buffer.c
   rpn_extra.c
   shmem_arena.c
   shmem_heap.c
   timer.c
)

set(F_ALLOC_FILE ${CMAKE_CURRENT_SOURCE_DIR}/io-server/f_alloc.inc)

file(GLOB PROJECT_INCLUDE_FILES
   io-server/*.h
   io-server/*.inc
   io-server/*.hf
)

set(COPYRIGHT_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/common_header.txt)

# Fortran headers
set(EXTRACT_FORTRAN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_fortran_interfaces.sh)
foreach(C_FILE ${C_TO_INC_FILES})
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/io-server/${FILE_NAME}.inc")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_FORTRAN_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_FORTRAN_SCRIPT} ${COPYRIGHT_HEADER})
   list(APPEND PROJECT_INCLUDE_FILES ${OUT_FILE})
endforeach(C_FILE)

# C headers
set(EXTRACT_C_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_c_interfaces.sh)
foreach(C_FILE ${C_TO_H_FILES})
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/io-server/${FILE_NAME}.h")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_C_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_C_SCRIPT} ${COPYRIGHT_HEADER})
   list(APPEND PROJECT_INCLUDE_FILES ${OUT_FILE})
endforeach(C_FILE)

# A bunch of fortran function signatures
set(F_ALLOC_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/shape_f_pointer.sh)
add_custom_command(OUTPUT ${F_ALLOC_FILE} COMMAND ${F_ALLOC_SCRIPT} > ${F_ALLOC_FILE} DEPENDS ${F_ALLOC_SCRIPT})
list(APPEND PROJECT_INCLUDE_FILES ${F_ALLOC_FILE})

# The library itself
add_library(${PROJECT_NAME} ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_INCLUDE_FILES})
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Where to put modules
get_target_property(LIBRARY_DIR ${PROJECT_NAME} BINARY_DIR)
set(MODULE_DIR ${LIBRARY_DIR}/include)
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_include_directories(${PROJECT_NAME} INTERFACE ${MODULE_DIR})

# Specify which modules to include in the public package
set(PUBLIC_MODULES
   ${MODULE_DIR}/heap_module.mod
   ${MODULE_DIR}/ioserver_context_module.mod
   ${MODULE_DIR}/ioserver_message_module.mod
   ${MODULE_DIR}/ioserver_run_module.mod
   ${MODULE_DIR}/jar_module.mod
   ${MODULE_DIR}/model_stream_module.mod
)

set_target_properties(${PROJECT_NAME} PROPERTIES VERSION ${PROJECT_VERSION})

target_compile_definitions(${PROJECT_NAME} PRIVATE _POSIX_C_SOURCE=199309L)

target_compile_options(
   ${PROJECT_NAME}
   PUBLIC
   $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall -Wextra>
   $<$<COMPILE_LANG_AND_ID:C,GNU>:-Werror -Wno-error=pedantic -Wno-error=unused-function>
   $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wall -Wextra -fmax-errors=8>
   $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Werror -Wno-error=unused-function -Wno-error=maybe-uninitialized -Wno-error=unused-variable -Wno-error=unused-dummy-argument>
   $<$<COMPILE_LANG_AND_ID:C,Intel>:-Wall -Werror>
   $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-error-limit 8 -warn all -diag-disable 5268,7025,7416>
   # -g -O0
)

install(TARGETS ${PROJECT_NAME}
   PUBLIC_HEADER DESTINATION include/$ENV{EC_ARCH}
   LIBRARY       DESTINATION lib/$ENV{EC_ARCH}
   ARCHIVE       DESTINATION lib/$ENV{EC_ARCH})
install(FILES ${PUBLIC_MODULES} DESTINATION include/$ENV{EC_ARCH})
