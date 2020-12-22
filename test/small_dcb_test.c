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

  distributed_circular_buffer_p buffer;

  buffer = DCB_create(MPI_COMM_WORLD, NUM_PRODUCERS, 200);

  int data[10];
  for (int i = 0; i < 10; ++i)
    data[i] = i;

  if (rank == 0) {
      DCB_put(buffer, data, 2);
  }

  MPI_Barrier(MPI_COMM_WORLD);

  if (rank != 0)
  {
    int num = DCB_get_num_elements(buffer, 0);
    printf("rank %d: num = %d\n", rank, num);
  }

  DCB_delete(buffer);

  MPI_Finalize();
  return 0;
}
