cmake_minimum_required(VERSION 3.16)

set(TEST_SOURCES
#   cb_plus_dcb.F90
   circular_buffer.F90
   circular_buffer_fill.c
   circular_buffer_single_thread.F90
   distributed_circular_buffer.F90
#   memory_arena.F90
   pseudo-model.F90
#   pseudo-server.F90
   small_dcb_test.c
   test_memory_arena.F90
   test_shmem_heap.F90
   )

foreach(TEST_SOURCE ${TEST_SOURCES})
   get_filename_component(BASE_NAME ${TEST_SOURCE} NAME_WLE)
   set(TEST_TARGET ${PROJECT_NAME}_${BASE_NAME})
   add_executable(${TEST_TARGET} ${TEST_SOURCE})
   target_link_libraries(${TEST_TARGET} ${PROJECT_NAME})
endforeach(TEST_SOURCE)
