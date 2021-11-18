${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write  1 2 2 1
# ${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_put_error  # This test currently fails on XC50