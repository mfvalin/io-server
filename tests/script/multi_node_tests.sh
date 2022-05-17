set -x
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_default_model               || exit -1
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write  1 2 2 2 2      || exit -1
${MPI_COMMAND} -n $(expr ${NUM_CPUS} - 1) ${BUILD_PATH}/tests/io_server_model_write  1 2 2 2 2      || exit -1      # Asymmetric nodes
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_model_write  0 2 2 2 2      || exit -1                  # Without debug mode
${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_init_comms                  || exit -1
${MPI_COMMAND} -n ${NUM_SERVER_CPUS} ${BUILD_PATH}/src/io_server_launch_server        : -n ${NUM_MODEL_CPUS} ${BUILD_PATH}/tests/io_server_launch_pseudo_model || exit -1
${MPI_COMMAND} -n ${NUM_MODEL_CPUS} ${BUILD_PATH}/tests/io_server_launch_pseudo_model : -n ${NUM_SERVER_CPUS} ${BUILD_PATH}/src/io_server_launch_server        || exit -1
${MPI_COMMAND} -n ${NUM_SERVER_CPUS} ${BUILD_PATH}/src/io_server_launch_server        : -n ${NUM_MODEL_CPUS} ${BUILD_PATH}/tests/io_server_model_integration   || exit -1
${MPI_COMMAND} -n ${NUM_MODEL_CPUS} ${BUILD_PATH}/tests/io_server_model_integration   : -n ${NUM_SERVER_CPUS} ${BUILD_PATH}/src/io_server_launch_server        || exit -1
# ${MPI_COMMAND} -n ${NUM_CPUS} ${BUILD_PATH}/tests/io_server_put_error  # This test currently fails on XC50
set +x