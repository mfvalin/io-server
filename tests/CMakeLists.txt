cmake_minimum_required(VERSION 3.16)

set(TEST_SOURCES
   cb_plus_dcb.F90
   circular_buffer.F90
   # circular_buffer_fill.c
   circular_buffer_single_thread.F90
   distributed_circular_buffer.F90
#   memory_arena.F90
   mpi_bandwidth.F90
   pseudo-model.F90
#   pseudo-server.F90
#   small_dcb_test.c
   test_memory_arena.F90
   test_shmem_heap.F90
   )

function(get_test_name SOURCE_FILE TEST_NAME)
   get_filename_component(BASE_NAME ${SOURCE_FILE} NAME_WLE)
   set(${TEST_NAME} ${PROJECT_NAME}_${BASE_NAME} PARENT_SCOPE)
endfunction(get_test_name)

# Build the tests
foreach(TEST_SOURCE ${TEST_SOURCES})
   get_test_name(${TEST_SOURCE} TEST_TARGET)
   add_executable(${TEST_TARGET} ${TEST_SOURCE})
   target_link_libraries(${TEST_TARGET} ${PROJECT_NAME})
endforeach(TEST_SOURCE)


# Targets to run basic tests
get_test_name(circular_buffer_single_thread.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${TEST_TARGET})

get_test_name(circular_buffer.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(circular_buffer_fill.c TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(distributed_circular_buffer.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(test_memory_arena.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS})

get_test_name(cb_plus_dcb.F90 TEST_TARGET)
add_test(NAME ${TEST_TARGET} COMMAND ${MPIEXEC_EXECUTABLE} -oversubscribe ${MPIEXEC_NUMPROC_FLAG} 16 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS} 1 2 2 4 0)

#get_test_name(test_shmem_heap.F90 TEST_TARGET)
#add_test(NAME ${TEST_TARGET}_base COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS} 0)
#add_test(NAME ${TEST_TARGET}_relay COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 6 ${MPIEXEC_PREFLAGS} ${TEST_TARGET} ${MPIEXEC_POSTFLAGS} 1)
