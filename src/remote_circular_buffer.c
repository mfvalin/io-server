/*
 * Copyright (C) 2020  Environnement Canada
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
 */

#include <stdio.h>
#include <time.h>

#include <mpi.h>

#include "circular_buffer.h"

typedef struct {
  MPI_Comm communicator;
  MPI_Win window;
  int32_t root;
  int32_t rank;
  int32_t comm_size;
  int32_t window_offset;
  int32_t num_bytes;
  union {
    circular_buffer* queue;
    int8_t* raw_data;
  };
} remote_circular_buffer;

typedef remote_circular_buffer *remote_circular_buffer_p;

static int available_space(const int in, const int out, const int limit)
{
//    printf("available_space: in = %d, out = %d, limit = %d\n", in, out, limit);
  return (in < out) ? out - in - 1 : limit - in + out - 1;
}

static int available_data(const int in, const int out, const int limit)
{
  return (in >= out) ? in - out : limit - out + in - 1;
}

static int get_available_space(const circular_buffer_p buffer)
{
    volatile int32_t *in = &buffer->m.in;
    volatile int32_t *out = &buffer->m.out;
    return available_space(*in, *out, buffer->m.limit);
}

static int get_available_data(const circular_buffer_p buffer)
{
    volatile int32_t *in = &buffer->m.in;
    volatile int32_t *out = &buffer->m.out;
    return available_data(*in, *out, buffer->m.limit);
}

static void sleep_us(const int num_us)
{
  struct timespec ts = {0, num_us * 1000};
  nanosleep(&ts, NULL);
}

static circular_buffer_p get_circular_buffer(remote_circular_buffer_p remote, const int buffer_id)
{
  return (circular_buffer_p)(remote->raw_data + buffer_id * remote->num_bytes);
}

static void print_buf(circular_buffer_p b, int rank)
{
  printf("version %d, first %d, in %d, out %d, limit %d, rank %d\n",
         b->m.version, b->m.first, b->m.in, b->m.out, b->m.limit, rank);
}



//F_StArT
//    function remote_circular_buffer_create(f_communicator, root, rank, comm_size, num_words) result(p) BIND(C, name = 'remote_circular_buffer_create')
//      import :: C_PTR, C_INT
//      implicit none
//      integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the remote buffer is shared
//      integer(C_INT), intent(IN), value :: root           !< Process rank on which buffer data is located
//      integer(C_INT), intent(IN), value :: rank           !< Rank of the current process
//      integer(C_INT), intent(IN), value :: comm_size      !< Number of processes in the communicator
//      integer(C_INT), intent(IN), value :: num_words      !< Number of 32-bit elements in the circular buffer
//      type(C_PTR) :: p                                    !< Pointer to created remote circular buffer
//   end function remote_circular_buffer_create
//F_EnD
remote_circular_buffer_p remote_circular_buffer_create(
    int32_t f_communicator,     //!< [in]  Communicator on which the remote buffer is shared (in Fortran)
    int32_t root,               //!< [in]  Process rank on which buffer data is located
    int32_t rank,               //!< [in]  Rank of the current process
    int32_t comm_size,          //!< [in]  Number of processes in the communicator
    int32_t num_words           //!< [in]  Number of 32-bit elements in the buffer
  ) {
  remote_circular_buffer_p buffer = (remote_circular_buffer*) malloc(sizeof(remote_circular_buffer));

  buffer->communicator = MPI_Comm_f2c(f_communicator);
  buffer->root         = root;
  buffer->rank         = rank;
  buffer->comm_size    = comm_size;
  buffer->num_bytes    = num_words * sizeof(int32_t);

  // Allocate space asymmetrically and set it as a window. The root gets all the space, everyone else gets nothing
  const int      num_buffers = (rank == root) ? comm_size - 1 : 1;
  const MPI_Aint win_size    = (rank == root) ? num_buffers * buffer->num_bytes : 0;
  MPI_Win_allocate(win_size, sizeof(int32_t), MPI_INFO_NULL, buffer->communicator, &buffer->raw_data, &buffer->window);


  if (rank == root)
  {
    // The root initializes every circular buffer, then sends to the corresponding node the offset where
    // that buffer is located in the window. The offset is in number of elements (int32_t).
    int current_index  = 0;
    for (int i = 0; i < comm_size; i++)
    {
      if (i != root)
      {
        circular_buffer_p buffer_address = get_circular_buffer(buffer, current_index);
        circular_buffer_init(buffer_address, num_words);

        const int current_offset = current_index * buffer->num_bytes / sizeof(int32_t);
        MPI_Send(&current_offset, 1, MPI_INTEGER, i, 0, buffer->communicator);

        current_index++;
      }
    }
  }
  else
  {
    // Allocate space for the circular buffer header (and the data pointer, but there is no data)
    buffer->queue = (circular_buffer *) malloc(sizeof(circular_buffer) * num_buffers);
    MPI_Status status;
    MPI_Recv(&buffer->window_offset, 1, MPI_INTEGER, buffer->root, 0, buffer->communicator, &status);
  }

  MPI_Win_fence(0, buffer->window);
  if (rank != root)
  {
    const int num_int = sizeof(circular_buffer) / sizeof(int32_t);
    MPI_Win_lock(MPI_LOCK_SHARED, buffer->root, 0, buffer->window);
    MPI_Get(buffer->queue, num_int, MPI_INTEGER, buffer->root, buffer->window_offset, num_int, MPI_INTEGER,
            buffer->window);
    MPI_Win_unlock(buffer->root, buffer->window);
  }

  return buffer;
}

//F_StArT
//  subroutine remote_circular_buffer_print(buffer) BIND(C, name = 'remote_circular_buffer_print')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
//  end subroutine remote_circular_buffer_print
//F_EnD
void remote_circular_buffer_print(
  remote_circular_buffer_p buffer //!< [in] Buffer for which to print data
  ) {
  printf("Comm: %ld, window: %ld, root: %d, rank: %d, queue %ld\n",
         (long)buffer->communicator, (long)buffer->window, buffer->root, buffer->rank, (long)buffer->queue);
  if (buffer->rank != buffer->root)
  {
    printf("Num elems: %d (rank %d)\n", get_available_data(buffer->queue), buffer->rank);
  }
  else
  {
    for (int i = 0; i < buffer->comm_size - 1; i++)
    {
      const circular_buffer_p b = get_circular_buffer(buffer, i);
      printf("From root: buffer %d has %d data in it\n", i, get_available_data(b));
    }
  }
}

//F_StArT
//  subroutine remote_circular_buffer_delete(buffer) BIND(C, name = 'remote_circular_buffer_delete')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
//  end subroutine remote_circular_buffer_delete
//F_EnD
void remote_circular_buffer_delete(
    remote_circular_buffer_p buffer //!< [in,out] Buffer to delete
  ) {
  MPI_Win_free(&buffer->window);
  if (buffer->rank != buffer->root)
    free(buffer->queue);
  free(buffer);
}


static int get_available_space_from_remote(const remote_circular_buffer_p buffer)
{
  MPI_Win_lock(MPI_LOCK_SHARED, buffer->root, 0, buffer->window);

  // TODO only get the "in" pointer rather than the entire structure?
  const int num_int = sizeof(circular_buffer) / sizeof(int32_t);
  MPI_Get(buffer->queue, num_int, MPI_INTEGER, buffer->root, buffer->window_offset, num_int, MPI_INTEGER,
          buffer->window);

  MPI_Win_unlock(buffer->root, buffer->window);

  return get_available_space(buffer->queue);
}


static int remote_circular_buffer_wait_space_available(
  remote_circular_buffer_p buffer,  //!< [in]  Pointer to a circular buffer
  int num_requested                 //!< [in]  Needed number of available slots
  ) {
  if (buffer == NULL || buffer->rank == buffer->root) return -1;

  const circular_buffer_p queue = buffer->queue;
  if (num_requested < 0 || queue->m.version != FIOL_VERSION) return -1;

  // First check locally
  int num_available = get_available_space(queue);
  if (num_available >= num_requested) return num_available;

  // Then get info from remote location, until there is enough
  while(num_available = get_available_space_from_remote(buffer), num_available < num_requested)
    sleep_us(100);

  return num_available;
}

//F_StArT
//  function remote_circular_buffer_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'remote_circular_buffer_put')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value           :: buffer
//    integer(C_INT), dimension(*), intent(in) :: src_data
//    integer(C_INT), intent(in), value        :: num_elements
//    integer(C_INT) :: num_available
//  end function remote_circular_buffer_put
//F_EnD
//! @brief Insert data into the given buffer, once there is enough space (will wait)
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the remote node (otherwise it might try to read data that has not yet arrived).
int remote_circular_buffer_put(
  remote_circular_buffer_p buffer,
  int32_t * const src_data,
  const int num_elements
  ) {
  remote_circular_buffer_wait_space_available(buffer, num_elements);

  MPI_Win_lock(MPI_LOCK_SHARED, buffer->root, 0, buffer->window);

  int32_t in_index        = buffer->queue->m.in;
  const int32_t out_index = buffer->queue->m.out;
  const int32_t limit     = buffer->queue->m.limit;

  const int32_t window_offset_base = buffer->window_offset + sizeof(fiol_management) / sizeof(int32_t);
  if (in_index < out_index) {
    // 1 segment
    const int32_t window_offset = window_offset_base + in_index;
    MPI_Accumulate(src_data, num_elements, MPI_INTEGER, buffer->root, window_offset, num_elements, MPI_INTEGER,
                   MPI_REPLACE, buffer->window);
    in_index += num_elements;
  }
  else {
    // First segment
    const int num_elem_segment_1 = num_elements > (limit - in_index) ? (limit - in_index) : num_elements;
    const int32_t window_offset_1 = window_offset_base + in_index;
    MPI_Accumulate(src_data, num_elem_segment_1, MPI_INTEGER, buffer->root, window_offset_1, num_elem_segment_1,
                   MPI_INTEGER, MPI_REPLACE, buffer->window);

    // Update insertion pointer
    in_index += num_elem_segment_1;
    if (in_index >= limit) in_index = 0;

    // Second segment
    const int num_elem_segment_2 = num_elements - num_elem_segment_1;
    const int32_t window_offset_2 = window_offset_base + in_index;
    MPI_Accumulate(src_data + num_elem_segment_1, num_elem_segment_2, MPI_INTEGER, buffer->root, window_offset_2,
                   num_elem_segment_2, MPI_INTEGER, MPI_REPLACE, buffer->window);

    // Update insertion pointer
    in_index += num_elem_segment_2;
  }

  // Update insertion index remotely and locally
  const int32_t window_offset = buffer->window_offset + offsetof(fiol_management, in) / sizeof(int32_t); //TODO IS THIS OK??
  MPI_Accumulate(&in_index, 1, MPI_INTEGER, buffer->root, window_offset, 1, MPI_INTEGER, MPI_REPLACE, buffer->window);
  buffer->queue->m.in = in_index;

  MPI_Win_unlock(buffer->root, buffer->window);

  return get_available_space(buffer->queue);
}

//F_StArT
//  subroutine buffer_write_test(buffer) BIND(C, name = 'buffer_write_test')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//  end subroutine buffer_write_test
//F_EnD
void buffer_write_test(remote_circular_buffer_p buffer)
{
  if (buffer->rank == buffer->root)
  {
    MPI_Barrier(buffer->communicator);
    const int num_bufs = buffer->comm_size - 1;
    for (int i = 0; i < num_bufs; i++)
    {
      printf("printing buf root -- ");
      print_buf(get_circular_buffer(buffer, i), buffer->rank);
    }
    MPI_Barrier(buffer->communicator);

    MPI_Barrier(buffer->communicator);
    for (int i = 0; i < num_bufs; i++)
    {
      printf("printing buf root after -- ");
      print_buf(get_circular_buffer(buffer, i), buffer->rank);
    }
  }
  else
  {
    print_buf(buffer->queue, buffer->rank);
    MPI_Barrier(buffer->communicator);
    MPI_Barrier(buffer->communicator);
    MPI_Win_lock(MPI_LOCK_SHARED, buffer->root, 0, buffer->window);

    int val = -1;
    MPI_Accumulate(&val, 1, MPI_INTEGER, buffer->root, buffer->window_offset, 1, MPI_INTEGER, MPI_REPLACE, buffer->window);


    MPI_Win_unlock(buffer->root, buffer->window);
    MPI_Barrier(buffer->communicator);
  }
}
