${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_default_model               || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write  1 2 2 2 2      || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_custom_functions            || exit -1
${MPI_COMMAND} -n ${NUM_SERVER_CPUS} ${BUILD_PATH}/tests/io_server_launch_server : -n ${NUM_MODEL_CPUS} ${BUILD_PATH}/tests/io_server_launch_pseudo_model || exit -1
# ${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_put_error  # This test currently fails on XC50