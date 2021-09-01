#include <stdio.h>
#include <string.h>

#include <mpi.h>

#include "io-server/distributed_circular_buffer.h"

// These numbers 
const size_t NUM_BUFFER_BYTES = 180 * 4;
const size_t NUM_CHARS = 60 * 4;
const int NUM_MESSAGES = 12; // Need a number that will go through every fraction of an 8-byte element size, and a little bit more

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

    for (int i = 0; i < NUM_MESSAGES; ++i)
    {
        for (int j = 0; j < NUM_MESSAGES; ++j)
            peeked[j] = '\0';

        // Check read of a partial element (that it copies exactly the number of bytes asked)
        result = DCB_get_bytes(dcb, 0, peeked, i, CB_PEEK);
        if (result != 0) exit(-1);
        check_msg_length(peeked, i);

        // Peek exact message size
        result = DCB_get_bytes(dcb, 0, peeked, NUM_CHARS - i, CB_PEEK);
        if (result != 0) exit(-1);
        check_msg_length(peeked, NUM_CHARS - i - 1);

        // Extract message, rounding the number of bytes up to the size of an element (this should NOT affect the next message)
        const size_t round_num = ((NUM_CHARS - i + sizeof(data_element) - 1) / sizeof(data_element)) * sizeof(data_element);
        result = DCB_get_bytes(dcb, 0, message, round_num, CB_COMMIT);
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

    if (capacity < NUM_BUFFER_BYTES || capacity > NUM_BUFFER_BYTES + 8)
    {
        printf("AAAHHHHH wrong capacity! Asked for %ld, got %ld\n", NUM_BUFFER_BYTES, capacity);
        exit(-1);
    }

    for (int i = 0; i < NUM_MESSAGES; ++i)
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
    MPI_Finalize();

    return 0;
}
