${BUILD_PATH}/tests/io_server_circular_buffer_single_thread
${BUILD_PATH}/tests/io_server_cb_unaligned_input
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_circular_buffer
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_circular_buffer_fill
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_distributed_circular_buffer
${MPI_COMMAND} -n 12          ${BUILD_PATH}/tests/io_server_simple_mutex
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_test_memory_arena
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write                 1 2 2 2
${MPI_COMMAND} -n 3           ${BUILD_PATH}/tests/io_server_dcb_edge_case_1
${MPI_COMMAND} -n 3           ${BUILD_PATH}/tests/io_server_dcb_unaligned_input
${MPI_COMMAND} -n 2           ${BUILD_PATH}/tests/io_server_shmem_heap_by_offset
#${MPI_COMMAND} -n 20         ${BUILD_PATH}/tests/io_server_put_error