#include <stdio.h>

#include <mpi.h>

#include "io-server/circular_buffer.h"
#include "io-server/timer.h"

void init_array(data_element* array, const int num_elements, const int rank) {
  for (int i = 0; i < num_elements; i++)
    array[i] = (rank + 1) * 10000 + i;
}

void fill_test(int argc, char** argv) {
  const int NUM_BUFFER_ELEMENTS = 128;
  const int NPTEST              = NUM_BUFFER_ELEMENTS * 2;
  const int READ_DELAY_US       = 1000;
  const int WRITE_DELAY_US      = 1000;

  data_element* local_data    = (data_element*)malloc(NPTEST * sizeof(data_element));
  data_element* received_data = (data_element*)malloc(NPTEST * sizeof(data_element));

  int num_errors = 0;

  // Init MPI
  int num_procs;
  int my_rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  // Allocate MPI window in shared memory
  MPI_Win        window;
  data_element*  base_mem_ptr;
  const int      disp_unit   = sizeof(data_element);
  const MPI_Aint window_size = NUM_BUFFER_ELEMENTS * disp_unit;
  MPI_Win_allocate_shared(window_size, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, &base_mem_ptr, &window);

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  circular_buffer_p  local_buffer = circular_buffer_from_pointer(base_mem_ptr, NUM_BUFFER_ELEMENTS);
  circular_buffer_p* all_buffers  = NULL;
  const int          success      = (local_buffer != NULL);
  if (my_rank == 0) {
    all_buffers = (circular_buffer_p*)malloc((size_t)num_procs * sizeof(circular_buffer_p));
    for (int i = 1; i < num_procs; ++i) {
      MPI_Aint target_size;
      int      target_disp_unit;
      MPI_Win_shared_query(window, i, &target_size, &target_disp_unit, &all_buffers[i]);
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  if (!success)
    num_errors++;

  const int max_num_elements = circular_buffer_get_available_space(local_buffer);
  init_array(local_data, NPTEST, my_rank);

  if (my_rank != 0) {
    const int num_free = circular_buffer_atomic_put(local_buffer, local_data, max_num_elements);
    if (num_free != 0)
      num_errors++;
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  {
    io_timer_t put_time = {0, 0};
    io_timer_start(&put_time);

    //---------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    //---------------------------

    if (my_rank == 0) {
      for (int i = 1; i < num_procs; ++i) {
        sleep_us(READ_DELAY_US);
        circular_buffer_atomic_get(all_buffers[i], received_data, NUM_BUFFER_ELEMENTS / 2);
      }
    }
    else {
      circular_buffer_atomic_put(local_buffer, local_data + NUM_BUFFER_ELEMENTS, 1);
      io_timer_stop(&put_time);

      const double t = io_time_ms(&put_time);
      //      printf("Put data after %f ms (rank %d)\n", t, my_rank);

      if (t * 1000 < READ_DELAY_US * my_rank)
        num_errors++;
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  {
    io_timer_t read_time = {0, 0};
    io_timer_start(&read_time);

    //---------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    //---------------------------

    if (my_rank == 0) {
      for (int i = 1; i < num_procs; ++i) {
        circular_buffer_atomic_get(all_buffers[i], received_data, max_num_elements - NUM_BUFFER_ELEMENTS / 2 + 2);
        io_timer_stop(&read_time);
        io_timer_start(&read_time);
        const double t = io_time_ms(&read_time);
        if (t * 1000 < WRITE_DELAY_US * i)
          num_errors++;

        //        printf("Read in %f ms\n", t);
      }
    }
    else {
      sleep_us(WRITE_DELAY_US * my_rank);
      circular_buffer_atomic_put(local_buffer, local_data, 1);
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  if (my_rank != 0) {
    if (circular_buffer_get_available_data(local_buffer) != 0)
      num_errors++;
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  const int tmp_errors = num_errors;
  MPI_Reduce(&tmp_errors, &num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD);

  if (my_rank == 0) {
    free(all_buffers);

    if (num_errors > 0) {
      printf("THERE WERE %d ERRORS\n", num_errors);
    }
    else {
      printf("Circular buffer fill and wait test successful\n");
    }
  }

  free(local_data);
  free(received_data);

  MPI_Win_free(&window);
  MPI_Finalize();
}

int main(int argc, char** argv) {
  fill_test(argc, argv);
  return 0;
}
