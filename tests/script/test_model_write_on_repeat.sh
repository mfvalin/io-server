#! /usr/bin/env bash

cp ./io_server_model_write model_write
TEST_COMMAND="mpirun -n 80 ./model_write 0 2 2 2 4"
TEST_OUTPUT_FILE="model_write_out"

for i in {1..2000}; do
    echo $i > test_count
    ${TEST_COMMAND} > ${TEST_OUTPUT_FILE}
    if [ $? -ne 0 ]; then
        mv ${TEST_OUTPUT_FILE} "${TEST_OUTPUT_FILE}_${i}.txt"
        echo "Test ${i} failed"
    fi
done

rm -f ${TEST_OUTPUT_FILE}

echo "$(ls ${TEST_OUTPUT_FILE}* | wc -w) test(s) failed"
