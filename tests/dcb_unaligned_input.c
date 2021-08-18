#include <stdio.h>
#include <string.h>

#include <mpi.h>

#include "io-server/distributed_circular_buffer.h"

// These numbers 
const size_t NUM_BUFFER_BYTES = 180 * 4;
const size_t NUM_CHARS = 60 * 4;

void check_msg_length(const char* msg, const size_t length)
{
    if (strlen(msg) != length) {
        printf("Message has wrong length! %ld, expected %ld\n", strlen(msg), length);
        exit(-1);
    }
}

void channel_process(distributed_circular_buffer_p dcb)
{
    const int result = DCB_channel_start_listening(dcb);
    if (result < 0) { printf("error in channel!\n"); exit(-1); }
}

void consumer_process(distributed_circular_buffer_p dcb)
{
    char message[NUM_CHARS];
    char peeked[NUM_CHARS];

    int result;

    for (int i = 0; i < 8; ++i)
    {
        result = DCB_get_bytes(dcb, 0, peeked, NUM_CHARS - i, CB_PEEK);
        if (result != 0) exit(-1);
        result = DCB_get_bytes(dcb, 0, message, NUM_CHARS, CB_COMMIT);
        if (result != 0) exit(-1);
        printf("received: %s (len = %ld)\n", message, strlen(message));
        check_msg_length(message, NUM_CHARS - i - 1);

        if (strcmp(message, peeked) != 0) exit(-1);
    }
}

void producer_process(distributed_circular_buffer_p dcb)
{
    char message[NUM_CHARS];
    for (int i = 0; i < NUM_CHARS - 1; ++i)
        message[i] = 'a' + i % 26;
    message[NUM_CHARS - 1] = '\0';

    size_t total_sent = 0;
    const int64_t capacity = DCB_get_capacity_local_bytes(dcb);

    if (capacity != NUM_BUFFER_BYTES)
    {
        printf("AAAHHHHH wrong capacity!\n");
        exit(-1);
    }

    for (int i = 0; i < 8; ++i)
    {
        const size_t count = NUM_CHARS - i;

        const int result = DCB_put_bytes(dcb, message + i, count, CB_COMMIT);
        if (result < 0) exit(-1);

        total_sent += (count + sizeof(data_element) - 1) / sizeof(data_element) * sizeof(data_element); 
    }
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
        dcb = DCB_create_bytes(MPI_COMM_WORLD, server_comm, 1, 1, NUM_BUFFER_BYTES);
    }
    else {
        MPI_Comm producer_comm;
        MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, &producer_comm);
        dcb = DCB_create_bytes(MPI_COMM_WORLD, MPI_COMM_NULL, 0, 0, 0);
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
