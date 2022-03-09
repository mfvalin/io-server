${BUILD_PATH}/tests/io_server_circular_buffer_single_thread                                     || exit -1
${BUILD_PATH}/tests/io_server_cb_unaligned_input                                                || exit -1
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_circular_buffer                     || exit -1
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_circular_buffer_fill                || exit -1
${MPI_COMMAND} -n 8           ${BUILD_PATH}/tests/io_server_circular_buffer_concurrent          || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_distributed_circular_buffer         || exit -1
${MPI_COMMAND} -n 8           ${BUILD_PATH}/tests/io_server_shmem_heap_basic                    || exit -1
${MPI_COMMAND} -n 12          ${BUILD_PATH}/tests/io_server_simple_mutex                        || exit -1
${MPI_COMMAND} -n 4           ${BUILD_PATH}/tests/io_server_test_memory_arena                   || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_default_model                       || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write           1 2 2 2 4     || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_init_comms                          || exit -1
${MPI_COMMAND} -n 3           ${BUILD_PATH}/tests/io_server_dcb_edge_case_1                     || exit -1
${MPI_COMMAND} -n 3           ${BUILD_PATH}/tests/io_server_dcb_unaligned_input                 || exit -1
#${MPI_COMMAND} -n 20         ${BUILD_PATH}/tests/io_server_put_error
