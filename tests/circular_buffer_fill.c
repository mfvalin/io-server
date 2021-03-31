/*
 * Copyright (C) 2021  Environnement Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Authors:
 *     M. Valin,   Recherche en Prevision Numerique, 2020/2021
 *     V. Magnoux, Recherche en Prevision Numerique, 2020/2021
 */

#include <stdio.h>

#include <mpi.h>

#include "io-server/circular_buffer.h"
#include "io-server/timer.h"

void init_array(data_element* array, const int num_elements, const int rank) {
  for (int i = 0; i < num_elements; i++)
    array[i] = (rank + 1) * 10000 + i;
}

int fill_test(int argc, char** argv) {
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

  circular_buffer_p  local_buffer = CB_from_pointer(base_mem_ptr, NUM_BUFFER_ELEMENTS);
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

  const int max_num_elements = CB_get_available_space(local_buffer);
  const int capacity         = CB_get_capacity(local_buffer);
  init_array(local_data, NPTEST, my_rank);

  if (my_rank != 0) {
    if (capacity != max_num_elements) {
      printf("Ahhhh inconsistency between free space (%d) and capacity (%d)!\n", max_num_elements, capacity);
      num_errors++;
    }

    const int expected_error = CB_atomic_put(local_buffer, local_data, capacity + 1, CB_COMMIT);
    if (expected_error != -1) {
      printf("Wrong return value after trying to put more than max into the buffer! %d\n", expected_error);
      num_errors++;
    }

    const int num_free = CB_atomic_put(local_buffer, local_data, max_num_elements, CB_COMMIT);
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
        CB_atomic_get(all_buffers[i], received_data, NUM_BUFFER_ELEMENTS / 2, CB_COMMIT);
      }
    }
    else {
      CB_atomic_put(local_buffer, local_data + NUM_BUFFER_ELEMENTS, 1, CB_COMMIT);
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
        CB_atomic_get(all_buffers[i], received_data, max_num_elements - NUM_BUFFER_ELEMENTS / 2 + 2, CB_COMMIT);
        io_timer_stop(&read_time);
        io_timer_start(&read_time);
        const double t = io_time_ms(&read_time);
        if (t * 1000 < WRITE_DELAY_US * i)
          num_errors++;

        //        printf("Read in %f ms\n", t);

        const int expected_error = CB_atomic_get(all_buffers[i], received_data, capacity + 1, CB_COMMIT);
        if (expected_error != -1) {
          printf("Wrong return code after trying to read more than max buffer size! (%d)\n", expected_error);
          num_errors++;
        }
      }
    }
    else {
      sleep_us(WRITE_DELAY_US * my_rank);
      CB_atomic_put(local_buffer, local_data, 1, CB_COMMIT);
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  if (my_rank != 0) {
    if (CB_get_available_data(local_buffer) != 0)
      num_errors++;
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  const int tmp_errors = num_errors;
  MPI_Reduce(&tmp_errors, &num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD);

  if (my_rank == 0) {
    for (int i = 1; i < num_procs; ++i) {
      CB_print_stats(all_buffers[i], i, i == 1);
    }

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

  return num_errors;
}

int main(int argc, char** argv) {
  return fill_test(argc, argv);
}
