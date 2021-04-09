#include <stdio.h>

#include <mpi.h>

#include "io-server/distributed_circular_buffer.h"

void test_function(MPI_Comm all_comm, MPI_Comm server_comm)
{
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    const int NUM_ELEMENTS = 100000;
    int local_buffer[NUM_ELEMENTS];

    for (int i = 0; i < NUM_ELEMENTS; i++)
    {
      local_buffer[i] = rank * 10000 + i;
    }

    const MPI_Aint shared_win_size = (rank == 0) ? NUM_ELEMENTS * sizeof(int) : 0;
    MPI_Win shared_window;
    int*    shared_mem = NULL;
    if (rank != 2)
    {
        MPI_Win_allocate_shared(shared_win_size, sizeof(int), MPI_INFO_NULL, server_comm, &shared_mem, &shared_window);
    }

    printf("%d: Allocated shared memory window\n", rank);

    if (rank == 1)
    {
        MPI_Aint size;
        int      disp_unit;
        MPI_Win_shared_query(shared_window, 0, &size, &disp_unit, &shared_mem);
    }

    MPI_Win window;
    const int win_size = rank == 2 ? 0 : NUM_ELEMENTS * sizeof(int);
    MPI_Win_create(shared_mem, win_size, sizeof(int), MPI_INFO_NULL, all_comm, &window);


    if (rank == 2)
    {
        //MPI_Win_lock(MPI_LOCK_SHARED, 1, MPI_MODE_NOCHECK, window); // This hangs when using OpenMPI, but not MPICH
        MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, window);
        MPI_Accumulate(local_buffer, NUM_ELEMENTS, MPI_INTEGER, 1, 0, NUM_ELEMENTS, MPI_INTEGER, MPI_REPLACE, window);
        MPI_Win_unlock(1, window);
    }

    // -------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    // -------------------------

    if (rank == 0)
    {
//        printf("shared_buffer:");
        int num_errors = 0;
        for (int i = 0; i < NUM_ELEMENTS; ++i)
        {
            if (shared_mem[i] != 2 * 10000 + i) num_errors++;
//            if (i % 30 == 0) printf("\n");
//            printf("%5d ", shared_mem[i]);
        }
//        printf("\n");
        printf("%d error(s) detected (without DCB)\n", num_errors);
    }

    // -------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    // -------------------------

    MPI_Win_free(&window);
    if (rank != 2) MPI_Win_free(&shared_window);
}

void test_function_dcb(MPI_Comm all_comm, MPI_Comm server_comm)
{
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    const int NUM_ELEMENTS = 3040;
    //const int NUM_ELEMENTS = 200;
    int local_buffer[NUM_ELEMENTS];

    for (int i = 0; i < NUM_ELEMENTS; i++)
    {
      local_buffer[i] = rank * 10000 + i;
    }

    distributed_circular_buffer_p dcb = DCB_create(all_comm, server_comm, 1, 1, NUM_ELEMENTS);


    if (rank == 1)
    {
        if (DCB_channel_start_listening(dcb) < 0)
            printf("ERROR in channel process\n");
        else
            DCB_delete(dcb);
        return;
    }

    if (rank == 2)
    {
        DCB_put(dcb, local_buffer, 1, CB_COMMIT);
        DCB_put(dcb, local_buffer + 1, NUM_ELEMENTS - 1, CB_COMMIT);
    }

    printf("%d: Before full barrier\n", rank);
    DCB_full_barrier(dcb);
    printf("%d: After full barrier\n", rank);

    if (rank == 0)
    {
        DCB_get(dcb, 0, local_buffer, NUM_ELEMENTS, CB_COMMIT);
        int num_errors = 0;
        for (int i = 0; i < NUM_ELEMENTS; ++i)
        {
            if (local_buffer[i] != 2 * 10000 + i)
            {
                num_errors++;
                //printf("local: %d, expected: %d\n", local_buffer[i], 2 * 10000 + i);
            }
        }
        printf("%d error(s) detected (with DCB)\n", num_errors);
    }

    DCB_delete(dcb);
}

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size != 3)
    {
        printf("Need exactly 2 MPI processes for this test!\n");
        return 1;
    }

    printf("Hi from rank %d/%d\n", rank, size);

    MPI_Comm server_comm;
    MPI_Comm_split(MPI_COMM_WORLD, rank == 2, 0, &server_comm);

    if (rank == 2) server_comm = MPI_COMM_NULL;
    test_function(MPI_COMM_WORLD, server_comm);
    test_function_dcb(MPI_COMM_WORLD, server_comm);

    MPI_Finalize();

    return 0;
}

