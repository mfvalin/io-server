#include <stdio.h>
#include <string.h>

#include <mpi.h>

#include "io-server/distributed_circular_buffer.h"

const int NUM_ELEMENTS = 200;
const int NUM_CHARS = 60 * sizeof(data_element);

void channel_process(distributed_circular_buffer_p dcb)
{
    const int result = DCB_channel_start_listening(dcb);
    if (result < 0) { printf("error in channel!\n"); exit(-1); }
}

void consumer_process(distributed_circular_buffer_p dcb)
{
    char message[NUM_CHARS + 4];

    int result;

    result = DCB_get(dcb, 0, message, NUM_CHARS / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    printf("received: %s (len = %d)\n", message, strlen(message));
    if (strlen(message) != NUM_CHARS - 1)
    {
        printf("Received wrong length! Got %d, should be %d\n", strlen(message), NUM_CHARS - 1);
        exit(-1);
    }

    result = DCB_get(dcb, 0, message, (NUM_CHARS - 1) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    printf("received: %s (len = %d)\n", message, strlen(message));
    if (strlen(message) != NUM_CHARS - 2)
    {
        printf("Received wrong length! Got %d, should be %d\n", strlen(message), NUM_CHARS - 2);
        exit(-1);
    }

    result = DCB_get(dcb, 0, message, (NUM_CHARS - 2) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    printf("received: %s (len = %d)\n", message, strlen(message));
    if (strlen(message) != NUM_CHARS - 3)
    {
        printf("Received wrong length! Got %d, should be %d\n", strlen(message), NUM_CHARS - 3);
        exit(-1);
    }

    result = DCB_get(dcb, 0, message, (NUM_CHARS - 3) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    printf("received: %s (len = %d)\n", message, strlen(message));
    if (strlen(message) != NUM_CHARS - 4)
    {
        printf("Received wrong length! Got %d, should be %d\n", strlen(message), NUM_CHARS - 4);
        exit(-1);
    }
}

void producer_process(distributed_circular_buffer_p dcb)
{
    char message[NUM_CHARS + 4];
    for (int i = 0; i < NUM_CHARS - 1; ++i)
        message[i] = 'a' + i % 26;
    message[NUM_CHARS - 1] = '\0';

    int result;
    result = DCB_put(dcb, message, NUM_CHARS / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    result = DCB_put(dcb, message + 1, (NUM_CHARS - 1) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    result = DCB_put(dcb, message + 2, (NUM_CHARS - 2) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
    result = DCB_put(dcb, message + 3, (NUM_CHARS - 3) / sizeof(data_element) + 1, CB_COMMIT);
    if (result < 0) exit(-1);
}

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    int global_size, global_rank;
    MPI_Comm_size(MPI_COMM_WORLD, &global_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &global_rank);

    if (global_size != 3) {
        printf("Need exactly 3 processes for this test\n");
        return 1;
    }

    distributed_circular_buffer_p dcb;

    if (global_rank == 0 || global_rank == 1) {
        MPI_Comm server_comm;
        MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, &server_comm);
        dcb = DCB_create(MPI_COMM_WORLD, server_comm, 1, 1, NUM_ELEMENTS);
    }
    else {
        MPI_Comm producer_comm;
        MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, &producer_comm);
        dcb = DCB_create(MPI_COMM_WORLD, MPI_COMM_NULL, 0, 0, 0);
    }

    if (dcb == NULL) {
        printf("Couldn't create the DCB!\n");
        return -1;
    }

    if (global_rank == 0)
        consumer_process(dcb);
    else if (global_rank == 1)
        channel_process(dcb);
    else if (global_rank == 2)
        producer_process(dcb);
    else
        return -1;

    DCB_delete(dcb);

    return 0;
}
