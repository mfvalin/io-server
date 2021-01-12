#include <stdio.h>

#include "io-server/distributed_circular_buffer.h"

int main(
    int argc, char** argv //!<
) {
  int rank, size;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  const int NUM_PRODUCERS = 1;
  int       val           = -1;

  distributed_circular_buffer_p buffer;

  MPI_Group world_group, server_group;
  MPI_Comm  server_comm;

  MPI_Comm_group(MPI_COMM_WORLD, &world_group);
  int ranges[1][3] = {{0, NUM_PRODUCERS - 1, 1}};
  MPI_Group_range_excl(world_group, 1, ranges, &server_group);
  MPI_Comm_create_group(MPI_COMM_WORLD, server_group, 0, &server_comm);

  buffer = DCB_create(MPI_COMM_WORLD, server_comm, NUM_PRODUCERS, 1, 200);

  int data[10];
  for (int i = 0; i < 10; ++i)
    data[i] = i;

  if (rank != 0) {
    buffer->raw_data[0] = -2;
    //    int num = DCB_get_num_elements(buffer, 0);
    //    printf("rank %d: num = %d\n", rank, num);
    printf("rank %d: data = %d, val = %d\n", rank, buffer->raw_data[0], val);
  }

  MPI_Barrier(MPI_COMM_WORLD);

  if (rank == 0) {
    //      DCB_put(buffer, data, 0);

    MPI_Win_lock(MPI_LOCK_SHARED, 2, MPI_MODE_NOCHECK, buffer->window);
    MPI_Accumulate(data + 2, 1, MPI_INT, 2, 0, 1, MPI_INT, MPI_REPLACE, buffer->window);
    //    MPI_Win_flush(2, buffer->window);
    //    MPI_Win_sync(buffer->window);
    MPI_Win_unlock(2, buffer->window);

    //    MPI_Win_lock(MPI_LOCK_SHARED, 2, MPI_MODE_NOCHECK, buffer->window_mem_dummy);
    //    MPI_Win_flush(2, buffer->window_mem_dummy);
    //    MPI_Win_sync(buffer->window_mem_dummy);
    //    MPI_Win_unlock(2, buffer->window_mem_dummy);

    //    MPI_Send(data + 5, 1, MPI_INT, 2, 0, MPI_COMM_WORLD);
  }

  //  MPI_Win_sync(buffer->window);
  //  MPI_Win_sync(buffer->window_mem_dummy);

  MPI_Status s;
  //  if (rank == 2) MPI_Recv(&val, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &s);

  MPI_Barrier(MPI_COMM_WORLD);

  //  MPI_Win_sync(buffer->window_mem_dummy);
  //  MPI_Win_sync(buffer->window);
  if (rank != 0) {
    //    buffer->raw_data[0] = -2;
    //    int num = DCB_get_num_elements(buffer, 0);
    //    printf("rank %d: num = %d\n", rank, num);
    printf("rank %d: data = %d, val = %d\n", rank, buffer->raw_data[0], val);
  }

  DCB_delete(buffer);

  MPI_Finalize();
  return 0;
}
