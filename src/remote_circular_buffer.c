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
#include <string.h>
#include <time.h>

#include <mpi.h>

#include "circular_buffer.h"

/**
  * @brief All information necessary to manage a set of circuler buffers accessed remotely, whose entire data is
  * located in a single process.
  *
  * This struct is a "collective" object, in the sense that every process that wants a remote circular buffer must have
  * an instance of it, including the "root" process. The root process is also called the "consumer" and holds the data
  * of all the collectively created circular buffers. The other processes are the "producers" and each only hold a copy
  * of the header of its circular buffer instance. The data on the producer process also forms an MPI window, which is
  * accessible by every other process that collectively own this remote buffer. In every communication, the consumer
  * process is always passive; this removes any need for explicit synchronization related to data transfer.
  */
typedef struct {
  MPI_Comm communicator;  //!< Communicator through which the processes sharing the remote buffer can communicate
  MPI_Win window;         //!< MPI window into the buffers, on the process which holds all data
  int32_t root;           //!< Rank of the process which actually stores the data
  int32_t rank;           //!< Rank of the process that initialized this particular instance of the remote buffer
  int32_t comm_size;      //!< How many processes share this remote buffer
  int32_t window_offset;  //!< Offset into the MPI window at which this producer's circular buffer is located
  int32_t num_bytes;      //!< How many bytes of data form a single circular buffer instance in this remote buffer
  union {
    circular_buffer* queue; //!< Pointer to the header of the circular buffer (only valid for producers)
    int8_t* raw_data;       //!< Pointer to the data holding the entire set of circular buffers (only valid for the consumer)
  };
} remote_circular_buffer;

typedef remote_circular_buffer *remote_circular_buffer_p;

const int SPACE_CHECK_WAIT_TIME_US = 100;
const int DATA_CHECK_WAIT_TIME_US  = 100;

//! @{ \name Helper functions

//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline int available_space(
    const int in,     //!< [in] Index of insertion location in the buffer
    const int out,    //!< [in] Index of extraction location in the buffer
    const int limit   //!< [in] Number of elements that the buffer can hold
  ) {
  return (in < out) ? out - in - 1 : limit - in + out - 1;
}

//! @brief Compute how much data is stored in a circular buffer, given of set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline int available_data(
    const int in,     //!< [in] Index of insertion location in the buffer
    const int out,    //!< [in] Index of extraction location in the buffer
    const int limit   //!< [in] Number of elements that the buffer can hold
  ) {
  return (in >= out) ? in - out : limit - out + in;
}

//! Compute how much space is available in a given circular buffer
static inline int get_available_space(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
  ) {
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile int32_t *in = &buffer->m.in;
  volatile int32_t *out = &buffer->m.out;
  return available_space(*in, *out, buffer->m.limit);
}

//! Compute how much data is stored in a given circular buffer
static inline int get_available_data(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
  ) {
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile int32_t *in = &buffer->m.in;
  volatile int32_t *out = &buffer->m.out;
  return available_data(*in, *out, buffer->m.limit);
}

//F_StArT
//  subroutine sleep_us(num_us) BIND(C, name = 'sleep_us')
//    import :: C_INT
//    implicit none
//    integer(C_INT), intent(in), value :: num_us    !< How many microseconds to sleep
//  end subroutine sleep_us
//F_EnD
//! Do nothing for a certain number of microseconds
void sleep_us(
    const int num_us  //!< [in] How many microseconds we want to wait
  ) {
  struct timespec ts; ts.tv_sec = num_us / 1000000; ts.tv_nsec = num_us % 1000000 * 1000;
  nanosleep(&ts, NULL);
}

//! Retrieve a pointer to a certain circular buffer managed by the given "remote" circular buffer
static inline circular_buffer_p get_circular_buffer(
    remote_circular_buffer_p remote,  //!< [in] The remote buffer from which we want a circular buffer
    const int buffer_id               //!< [in] ID of the circular buffer within the remote buffer
  ) {
  return (circular_buffer_p)(remote->raw_data + buffer_id * remote->num_bytes);
}

//! Print info about a circular buffer (for debugging)
static inline void print_buf(circular_buffer_p b, int rank)
{
  printf("version %d, first %d, in %d, out %d, limit %d, rank %d\n",
         b->m.version, b->m.first, b->m.in, b->m.out, b->m.limit, rank);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance and compute how much
//! space is available
//! @return The number of 4-byte elements that can still be store in the before
static inline int get_available_space_from_remote(
    const remote_circular_buffer_p buffer   //!< [in] The buffer we want to query
  ) {
  MPI_Win_lock(MPI_LOCK_SHARED, buffer->root, 0, buffer->window);

  // TODO only get the "out" pointer rather than the entire structure?
  const int num_int = sizeof(circular_buffer) / sizeof(int32_t);
  MPI_Get(buffer->queue, num_int, MPI_INTEGER, buffer->root, buffer->window_offset, num_int, MPI_INTEGER,
          buffer->window);

  MPI_Win_unlock(buffer->root, buffer->window);

  return get_available_space(buffer->queue);
}

//! Stop and wait until there is enough space in this circular buffer instance.
//! If the latest copy of the header shows enough space, return immediately. Otherwise, copy metadata from the consumer
//! process and check, until there is enough space.
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated.
static int remote_circular_buffer_wait_space_available(
    remote_circular_buffer_p buffer,  //!< [in]  Pointer to a circular buffer
    const int num_requested           //!< [in]  Needed number of available slots
  ) {
  if (buffer == NULL || buffer->rank == buffer->root) return -1;

  const circular_buffer_p queue = buffer->queue;
  if (num_requested < 0 || queue->m.version != FIOL_VERSION) return -1;

  // First check locally
  int num_available = get_available_space(queue);
  if (num_available >= num_requested) return num_available;

  // Then get info from remote location, until there is enough
  while(num_available = get_available_space_from_remote(buffer), num_available < num_requested)
    sleep_us(SPACE_CHECK_WAIT_TIME_US);

  return num_available;
}

static int remote_circular_buffer_wait_data_available(
     const remote_circular_buffer_p buffer, //!< [in] Buffer we are querying
     const int buffer_id,                   //!< [in] Which specific circular buffer we want to query (there are multiple ones)
     const int num_requested                //!< [in] Number of elements we want to read
  ) {
  if (buffer == NULL ||
      buffer->rank != buffer->root ||
      num_requested < 0)
    return -1;

  const circular_buffer_p queue = get_circular_buffer(buffer, buffer_id);
  if (queue->m.version != FIOL_VERSION)
    return -1;

  // Only check locally, waiting a bit between each check
  int num_available = 0;
  while (num_available = get_available_data(queue), num_available < num_requested)
    sleep_us(DATA_CHECK_WAIT_TIME_US);

  return num_available;
}

//! @}

//! @{ \name Remote circular buffer public interface

//! @brief Create a remote circular buffer on a set of processes.
//!
//! One of the processes is the root (consumer) and holds the
//! data for a circular buffer for each other process. The other processes only get a copy of the header of their
//! circular buffer, as well as an offset that points to the location of their data on the root process.
//! @return A pointer to a newly-allocated remote circular buffer struct that contains all the relevant info
remote_circular_buffer_p remote_circular_buffer_create(
    MPI_Comm communicator,      //!< [in]  Communicator on which the remote buffer is shared
    int32_t root,               //!< [in]  Process rank on which buffer data is located
    int32_t rank,               //!< [in]  Rank of the current process
    int32_t comm_size,          //!< [in]  Number of processes in the communicator
    int32_t num_words           //!< [in]  Number of 32-bit elements in the buffer
  ) {
  remote_circular_buffer_p buffer = (remote_circular_buffer*) malloc(sizeof(remote_circular_buffer));

  buffer->communicator = communicator;
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

  MPI_Barrier(buffer->communicator);
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
//    function remote_circular_buffer_create(f_communicator, root, rank, comm_size, num_words) result(p) BIND(C, name = 'remote_circular_buffer_create_f')
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
remote_circular_buffer_p remote_circular_buffer_create_f(
    int32_t f_communicator,     //!< [in]  Communicator on which the remote buffer is shared (in Fortran)
    int32_t root,               //!< [in]  Process rank on which buffer data is located
    int32_t rank,               //!< [in]  Rank of the current process
    int32_t comm_size,          //!< [in]  Number of processes in the communicator
    int32_t num_words           //!< [in]  Number of 32-bit elements in the buffer
  ) {
    return remote_circular_buffer_create(MPI_Comm_f2c(f_communicator), root, rank, comm_size, num_words);
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
//! Release all data used by the given remote circular buffer
void remote_circular_buffer_delete(
    remote_circular_buffer_p buffer //!< [in,out] Buffer to delete
  ) {
  MPI_Win_free(&buffer->window);
  if (buffer->rank != buffer->root)
    free(buffer->queue);
  free(buffer);
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
//! @brief Insert data into the given buffer, once there is enough space (will wait if there isn't enough initially)
//!
//! The data will be inserted into the buffer instance associated with the calling process.
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the remote node (otherwise it might try to read data that has not yet arrived).
//!
//! @return How many elements can still fit after the insertion, if everything went smoothly, -1 otherwise
int remote_circular_buffer_put(
    remote_circular_buffer_p buffer,  //!< Remote buffer in which we want to put data
    int32_t * const src_data,         //!< Pointer to the data we want to insert
    const int num_elements            //!< How many 4-byte elements we want to insert
  ) {
  if (remote_circular_buffer_wait_space_available(buffer, num_elements) < 0) return -1;

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

    // Update temporary insertion pointer
    in_index += num_elem_segment_1;
    if (in_index >= limit) in_index = 0;

    // Second segment (if there is one)
    const int num_elem_segment_2 = num_elements - num_elem_segment_1;
    const int32_t window_offset_2 = window_offset_base + in_index;
    MPI_Accumulate(src_data + num_elem_segment_1, num_elem_segment_2, MPI_INTEGER, buffer->root, window_offset_2,
                   num_elem_segment_2, MPI_INTEGER, MPI_REPLACE, buffer->window);

    // Update temporary insertion pointer
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
//  function remote_circular_buffer_get(buffer, buffer_id, dest_data, num_elements) result(num_available) BIND(C, name = 'remote_circular_buffer_get')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value              :: buffer
//    integer(C_INT), intent(in), value           :: buffer_id
//    integer(C_INT), dimension(*), intent(inout) :: dest_data
//    integer(C_INT), intent(in), value           :: num_elements
//    integer(C_INT) :: num_available
//  end function remote_circular_buffer_get
//F_EnD
int remote_circular_buffer_get(
    remote_circular_buffer_p buffer,
    const int buffer_id,
    int32_t* dest_data,
    const int num_elements
  ) {
  if (remote_circular_buffer_wait_data_available(buffer, buffer_id, num_elements) < 0) return -1;

  circular_buffer_p queue = get_circular_buffer(buffer, buffer_id);

  const int32_t in_index = queue->m.in;
  int32_t out_index      = queue->m.out;
  const int32_t limit    = queue->m.limit;

  const int32_t * const buffer_data = queue->data;

  if (out_index < in_index) {
    // 1 segment
    memcpy(dest_data, buffer_data + out_index, num_elements * sizeof(int32_t));
    out_index += num_elements;
  }
  else {
    // 1st segment
    const int num_elements_1 = num_elements > (limit - out_index) ? (limit - out_index) : num_elements;
    memcpy(dest_data, buffer_data + out_index, num_elements_1);

    // Update temporary extraction pointer
    out_index += num_elements_1;
    if(out_index >= limit) out_index = 0;

    // 2nd segment (if there is one)
    const int num_elements_2 = num_elements - num_elements_1;
    memcpy(dest_data + num_elements_1, buffer_data + out_index, num_elements_2 * sizeof(int32_t));

    // Update temporary extraction pointer
    out_index += num_elements_2;
  }

  // Update actual extraction pointer
  M_FENCE // Make sure everything has been read, and the temp pointer actually updated
  queue->m.out = out_index;

  return get_available_data(queue);
}

//! @}

//! @{ \name Testing stuff

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

//! @}
