cmake_minimum_required(VERSION 3.16)

set(TEST_SOURCES
   atomic.F90
   cb_unaligned_input.c
   circular_buffer.F90
   circular_buffer_concurrent.F90
   circular_buffer_fill.c
   circular_buffer_single_thread.F90
   circular_buffer_timeout.F90
   dcb_edge_case_1.F90
   dcb_timeout.F90
   dcb_unaligned_input.c
   distributed_circular_buffer.F90
   grid_meta.F90
   put_error.F90
   shmem_heap_basic.F90
   simple_mutex.F90
   test_memory_arena.F90
   test_serializer.F90
   test_string.F90
   )

set(TEST_HELPER_FILES
   test_helper.c
   test_helper_mod.F90
)

# Fortran headers
set(EXTRACT_FORTRAN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_fortran_interfaces.sh)
foreach(C_FILE test_helper.c)
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${FILE_NAME}.inc")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_FORTRAN_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_FORTRAN_SCRIPT})
   list(APPEND TEST_HELPER_FILES ${OUT_FILE})
endforeach(C_FILE)

# C headers
set(EXTRACT_C_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_c_interfaces.sh)
foreach(C_FILE test_helper.c)
   get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
   set(IN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE}")
   set(OUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${FILE_NAME}.h")
   add_custom_command(OUTPUT ${OUT_FILE} COMMAND ${EXTRACT_C_SCRIPT} ${IN_FILE} > ${OUT_FILE} DEPENDS ${C_FILE} ${EXTRACT_C_SCRIPT})
   list(APPEND TEST_HELPER_FILES ${OUT_FILE})
endforeach(C_FILE)


function(get_test_name SOURCE_FILE TEST_NAME)
   get_filename_component(BASE_NAME ${SOURCE_FILE} NAME_WLE)
   set(${TEST_NAME} ${PROJECT_NAME}_${BASE_NAME} PARENT_SCOPE)
endfunction(get_test_name)

function(gen_test_target)
#   set(options )
#   set(oneValueArgs )
   set(multiValueArgs SOURCES LIBRARIES)
   cmake_parse_arguments(GEN_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

   if (NOT DEFINED GEN_TEST_SOURCES)
     message(WARNING "Missing sources")
   endif()

   list (GET GEN_TEST_SOURCES 0 FIRST_SOURCE)
   get_test_name(${FIRST_SOURCE} TEST_TARGET)

   # The target
   add_executable(${TEST_TARGET} ${GEN_TEST_SOURCES})
   target_compile_options(
      ${TEST_TARGET}
      PRIVATE
      $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wno-integer-division>
      $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-diag-disable 5268 -error-limit 8>
      )

   # Modules
   get_target_property(LIBRARY_DIR ${TEST_TARGET} BINARY_DIR)
   set_target_properties(${TEST_TARGET} PROPERTIES Fortran_MODULE_DIRECTORY ${LIBRARY_DIR}/mod)
   target_include_directories(${TEST_TARGET} INTERFACE ${LIBRARY_DIR}/mod)
   target_compile_definitions(${TEST_TARGET} PRIVATE _POSIX_C_SOURCE=199309L)

   # Libraries
   foreach(LIB ${GEN_TEST_LIBRARIES})
      target_link_libraries(${TEST_TARGET} ${LIB})
   endforeach()
endfunction()

# Create the test library
set(TEST_HELPER_LIB test_helper)
add_library(${TEST_HELPER_LIB} STATIC ${TEST_HELPER_FILES})
target_include_directories(${TEST_HELPER_LIB} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_compile_definitions(${TEST_HELPER_LIB} PRIVATE _POSIX_C_SOURCE=199309L)

# Where to put test library modules
get_target_property(LIBRARY_DIR ${TEST_HELPER_LIB} BINARY_DIR)
set_target_properties(${TEST_HELPER_LIB} PROPERTIES Fortran_MODULE_DIRECTORY ${LIBRARY_DIR}/mod)
target_include_directories(${TEST_HELPER_LIB} INTERFACE ${LIBRARY_DIR}/mod)

# Build the tests
foreach(TEST_SOURCE ${TEST_SOURCES})
   gen_test_target(SOURCES ${TEST_SOURCE} LIBRARIES ${PROJECT_NAME})
endforeach(TEST_SOURCE)

gen_test_target(SOURCES benchmark_mpi_bandwidth.F90   LIBRARIES ${PROJECT_NAME} ${TEST_HELPER_LIB})
gen_test_target(SOURCES benchmark_disk_bandwidth.F90  LIBRARIES ${PROJECT_NAME} ${TEST_HELPER_LIB})
gen_test_target(SOURCES default_model.F90             LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES empty_model.F90               LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES init_comms.F90                LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES launch_pseudo_model.F90       LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES many_streams.F90              LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES model_integration.F90         LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)
gen_test_target(SOURCES model_write.F90               LIBRARIES ${PROJECT_NAME} ioserver_pseudo_model)

# Check MPI implementation to be able to give the proper flags when running tests
find_program(RPN_MPIRUN mpirun)
if (${RPN_MPIRUN} STREQUAL "RPN_MPIRUN-NOTFOUND")
   message(FATAL_ERROR "mpirun not found. Won't be able to configure the version.")
else()
   execute_process(
      COMMAND ${RPN_MPIRUN} --version 2>&1
      COMMAND grep -E -o "Open MPI"
      OUTPUT_VARIABLE MPI_IMPLEMENTATION
      OUTPUT_STRIP_TRAILING_WHITESPACE)
   if ("x${MPI_IMPLEMENTATION}" STREQUAL "xOpen MPI")
      set(OVERSUBSCRIBE_FLAG "-oversubscribe")
   endif()
   # message(WARNING "Oversubscribe flag: ${OVERSUBSCRIBE_FLAG}")
endif()

# Targets to run basic tests
get_test_name(circular_buffer_single_thread.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${TEST_TARGET})

get_test_name(cb_unaligned_input.c TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${TEST_TARGET})

get_test_name(test_serializer.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${TEST_TARGET})

get_test_name(grid_meta.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${TEST_TARGET})

get_test_name(circular_buffer.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(circular_buffer_concurrent.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(circular_buffer_fill.c TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(circular_buffer_timeout.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(distributed_circular_buffer.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 20 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(shmem_heap_basic.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(simple_mutex.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(atomic.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(test_memory_arena.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(default_model.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(model_write.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET}_debug COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS} 1 2 2 2 2)
add_test(NAME ${TEST_TARGET}_nodebug COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS} 0 2 2 2 2)

get_test_name(init_comms.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 12 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(dcb_edge_case_1.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 3 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(dcb_timeout.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 3 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(dcb_unaligned_input.c TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 3 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

set(LAUNCH_SERVER_TARGET ${CMAKE_BINARY_DIR}/src/${PROJECT_NAME}_launch_server)

get_test_name(launch_pseudo_model.F90 LAUNCH_MODEL_TARGET)
add_test(NAME launch_server_model COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS} :
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${LAUNCH_MODEL_TARGET} ${MPIEXEC_POSTFLAGS})
add_test(NAME launch_model_server COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${LAUNCH_MODEL_TARGET} ${MPIEXEC_POSTFLAGS} :
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(model_integration.F90 LAUNCH_MODEL_TARGET)
add_test(NAME server_model_integration COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 6 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS} :
         ${MPIEXEC_NUMPROC_FLAG} 10 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${LAUNCH_MODEL_TARGET} ${MPIEXEC_POSTFLAGS})
add_test(NAME model_server_integration COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 10 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${LAUNCH_MODEL_TARGET} ${MPIEXEC_POSTFLAGS} :
         ${MPIEXEC_NUMPROC_FLAG} 6 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(empty_model.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS} :
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(many_streams.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET}_10 COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS} --max-streams 10 --put-cmd-timeout 10 :
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})
add_test(NAME ${TEST_TARGET}_100 COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG}
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${LAUNCH_SERVER_TARGET} ${MPIEXEC_POSTFLAGS} --max-streams 100 --put-cmd-timeout 2 :
         ${MPIEXEC_NUMPROC_FLAG} 8 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(put_error.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${OVERSUBSCRIBE_FLAG} ${MPIEXEC_NUMPROC_FLAG} 40 ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET} ${MPIEXEC_POSTFLAGS})
