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

typedef struct {
  int             target_rank;
  circular_buffer buf;
} circular_buffer_instance;

typedef circular_buffer_instance* circular_buffer_instance_p;

/**
 * @brief All information necessary to manage a set of circuler buffers accessed remotely, whose entire data is
 * located in a single process.
 *
 * This struct is a "collective" object, in the sense that every process that wants a remote circular buffer must have
 * an instance of the entire set description, including the "root" process.
 * The root processes are also called the "consumers" and share ownership of the data of all the collectively created
 * circular buffers in the distributed set. They must be located on the same physical node.
 * The other processes are the "producers" and each only hold a copy of the header of its circular buffer instance.
 * The data on the consumer processes also forms an MPI window into shared memory, which is
 * accessible remotely by every producer process that collectively own this distributed buffer set.
 * In every communication, the consumer processes are always passive; this removes any need for explicit
 * synchronization related to data transfer.
 */
typedef struct {
  MPI_Comm communicator;  //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win  window;        //!< MPI window into the circular buffers themselves, on the process which holds all data
  int32_t  rank;          //!< Rank of the process that initialized this instance of the distributed buffer description
  int32_t  comm_size;     //!< How many processes share this distributed buffer set
  int32_t  num_producers; //!< How many producer processes share this distributed buffer set
  int32_t  window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located
  int32_t  num_bytes_per_instance; //!< How many bytes of data form a single circular buffer instance in this buffer set
  union {
    circular_buffer_instance local_header; //!< Header of the circular buffer instance (only valid for producers)
    int8_t* raw_data; //!< Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  };
} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;

const int SPACE_CHECK_WAIT_TIME_US = 100; //!< How long to wait between checks for free space in a buffer (microseconds)
const int DATA_CHECK_WAIT_TIME_US  = 100; //!< How long to wait between checks for data in a buffer (microseconds)

//! @{ \name Helper functions

//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline int available_space(
    const int in,   //!< [in] Index of insertion location in the buffer
    const int out,  //!< [in] Index of extraction location in the buffer
    const int limit //!< [in] Number of elements that the buffer can hold
) {
  return (in < out) ? out - in - 1 : limit - in + out - 1;
}

//! @brief Compute how much data is stored in a circular buffer, given of set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline int available_data(
    const int in,   //!< [in] Index of insertion location in the buffer
    const int out,  //!< [in] Index of extraction location in the buffer
    const int limit //!< [in] Number of elements that the buffer can hold
) {
  return (in >= out) ? in - out : limit - out + in;
}

//! Compute how much space (in number of #cb_element) is available in a given circular buffer
static inline int get_available_space(const circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile int32_t* in  = &buffer->m.in;
  volatile int32_t* out = &buffer->m.out;
  return available_space(*in, *out, buffer->m.limit);
}

//! Compute how much data (in number of #cb_element) is stored in a given circular buffer
static inline int get_available_data(const circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile int32_t* in  = &buffer->m.in;
  volatile int32_t* out = &buffer->m.out;
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
void sleep_us(const int num_us //!< [in] How many microseconds we want to wait
) {
  struct timespec ts;
  ts.tv_sec  = num_us / 1000000;
  ts.tv_nsec = num_us % 1000000 * 1000;
  nanosleep(&ts, NULL);
}

//! Check if the caller is a consumer process (according to its distributed_circular_buffer)
static inline int is_consumer(distributed_circular_buffer_p buffer) {
  return (buffer->rank >= buffer->num_producers);
}

//! Check if the caller is a producer process (according to its distributed_circular_buffer)
static inline int is_producer(distributed_circular_buffer_p buffer) {
  return !is_consumer(buffer);
}

static inline circular_buffer_instance_p get_circular_buffer_instance(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want the circ buffer instance
    const int                     buffer_id   //!< [in] ID of the circ buffer instance within the buffer set
) {
  return (circular_buffer_instance_p)(buffer_set->raw_data + buffer_id * buffer_set->num_bytes_per_instance);
}

//! Retrieve a pointer to a certain circular buffer managed by the given distributed circular buffer set
static inline circular_buffer_p get_circular_buffer(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want a circular buffer
    const int                     buffer_id   //!< [in] ID of the circular buffer within the buffer set
) {
  return &get_circular_buffer_instance(buffer_set, buffer_id)->buf;
}

//! Print info about a circular buffer (for debugging)
static inline void print_buf(
    circular_buffer_p b,   //!< [in] Pointer to the buffer to print
    int               rank //!< [in] Rank of the caller
) {
  printf(
      "version %d, first %d, in %d, out %d, limit %d, rank %d\n", b->m.version, b->m.first, b->m.in, b->m.out,
      b->m.limit, rank);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance
static inline void update_local_header_from_remote(
    distributed_circular_buffer_p buffer //!< [in] Buffer set from which we want to update a single instance
) {
  MPI_Win_lock(MPI_LOCK_SHARED, buffer->num_producers, 0, buffer->window);

  const int num_int = sizeof(circular_buffer_instance) / sizeof(int);
  MPI_Get(
      &buffer->local_header, num_int, MPI_INTEGER, buffer->num_producers, buffer->window_offset, num_int, MPI_INTEGER,
      buffer->window);

  MPI_Win_unlock(buffer->num_producers, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance and compute how much
//! space is available
//! @return The number of #cb_element tokens that can still be stored in the buffer
static inline int get_available_space_from_remote(
    const distributed_circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  update_local_header_from_remote(buffer);
  return get_available_space(&buffer->local_header.buf);
}

//! Stop and wait until there is enough space in this circular buffer instance.
//!
//! If the latest copy of the header shows enough space, return immediately. Otherwise, copy metadata from the consumer
//! process and check, until there is enough space.
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated.
static int distributed_circular_buffer_wait_space_available(
    distributed_circular_buffer_p buffer,       //!< [in] Pointer to the distributed buffer we're waiting for
    const int                     num_requested //!< [in] Needed number of available #cb_element slots
) {
  if (buffer == NULL || is_consumer(buffer))
    return -1;

  const circular_buffer_p instance = &buffer->local_header.buf;
  if (num_requested < 0 || instance->m.version != FIOL_VERSION)
    return -1;

  // First check locally
  int num_available = get_available_space(instance);
  if (num_available >= num_requested)
    return num_available;

  // Then get info from remote location, until there is enough
  while (num_available = get_available_space_from_remote(buffer), num_available < num_requested)
    sleep_us(SPACE_CHECK_WAIT_TIME_US);

  return num_available;
}

//! Stop and wait until there is enough data in this circular buffer instance
//!
//! @return The number of data elements in the buffer, if everything goes smoothly, -1 otherwise.
static int distributed_circular_buffer_wait_data_available(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer we are querying
    const int buffer_id,    //!< [in] Which specific circular buffer we want to query (there are multiple ones)
    const int num_requested //!< [in] Number of elements we want to read
) {
  if (buffer == NULL || is_producer(buffer) || num_requested < 0)
    return -1;

  const circular_buffer_p instance = get_circular_buffer(buffer, buffer_id);
  if (instance->m.version != FIOL_VERSION)
    return -1;

  // Only check locally, waiting a bit between each check
  int num_available = 0;
  while (num_available = get_available_data(instance), num_available < num_requested) {
    sleep_us(DATA_CHECK_WAIT_TIME_US);
  }

  return num_available;
}

//! @}

//! @{ \name Distributed circular buffer public interface

//! @brief Create a set of distributed circular buffers on a set of processes.
//!
//! Some of the processes are consumers and hold the data for a circular buffer instance for each other process,
//! in a shared memory window. _It is the caller's responsibility to ensure that all consumer processes are located on
//! the same physical node._
//! The other processes are producers and only get a copy of the header of their
//! circular buffer, as well as an offset that points to the location of their data within the shared window.
//!
//! @return A pointer to a newly-allocated distributed circular buffer struct that contains all the relevant info
distributed_circular_buffer_p distributed_circular_buffer_create(
    MPI_Comm communicator,  //!< [in] Communicator on which the distributed buffer is shared
    int32_t  rank,          //!< [in] Rank of the current process
    int32_t  comm_size,     //!< [in] Number of processes in the communicator
    int32_t  num_producers, //!< [in] Number of producer processes in the communicator
    int32_t  num_elements   //!< [in] Number of #cb_element tokens in the buffer
) {
  distributed_circular_buffer_p buffer = (distributed_circular_buffer*)malloc(sizeof(distributed_circular_buffer));

  buffer->communicator           = communicator;
  buffer->rank                   = rank;
  buffer->comm_size              = comm_size;
  buffer->num_producers          = num_producers;
  buffer->num_bytes_per_instance = num_elements * sizeof(cb_element);

  // Allocate space asymmetrically and set it as a window. The root gets all the space, everyone else gets nothing
  const int      num_buffers = (rank == num_producers) ? num_producers : 0;
  const MPI_Aint win_size    = num_buffers * buffer->num_bytes_per_instance;

  MPI_Win_allocate_shared(
      win_size, sizeof(cb_element), MPI_INFO_NULL, buffer->communicator, &buffer->raw_data, &buffer->window);

  if (rank == num_producers) {
    // The root initializes every circular buffer, then sends to the corresponding node the offset where
    // that buffer is located in the window. The offset is in number of elements (int32_t).
    const int size_diff = (sizeof(circular_buffer_instance) - sizeof(circular_buffer)) / sizeof(cb_element);
    for (int i = 0; i < num_producers; i++) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i);
      circular_buffer_p          buffer_address  = &buffer_instance->buf;

      circular_buffer_init(buffer_address, num_elements - size_diff);
      buffer_instance->target_rank = rank;

      const int current_offset = i * buffer->num_bytes_per_instance / sizeof(cb_element);
      MPI_Send(&current_offset, 1, MPI_INTEGER, i, 0, buffer->communicator);
    }
  }
  else if (is_consumer(buffer)) { // Consumer nodes that are _not_ the first one
    MPI_Aint size;
    int      disp_unit;
    MPI_Win_shared_query(buffer->window, num_producers, &size, &disp_unit, &buffer->raw_data);
  }
  else {
    // Allocate space for the circular buffer header (and the data pointer, but there is no data)
    //    buffer->queue = (circular_buffer*)malloc(sizeof(circular_buffer) * num_buffers);
    MPI_Status status;
    MPI_Recv(&buffer->window_offset, 1, MPI_INTEGER, buffer->num_producers, 0, buffer->communicator, &status);
  }

  // --------------------------------
  MPI_Barrier(buffer->communicator);
  // --------------------------------

  if (is_producer(buffer)) {
    update_local_header_from_remote(buffer);
  }

  return buffer;
}

//F_StArT
//  function distributed_circular_buffer_create(f_communicator, rank, comm_size, num_producers, num_elements) result(p) BIND(C, name = 'distributed_circular_buffer_create_f')
//    import :: C_PTR, C_INT
//    implicit none
//    integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
//    integer(C_INT), intent(IN), value :: rank           !< Rank of the calling process
//    integer(C_INT), intent(IN), value :: comm_size      !< Number of processes in the communicator
//    integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
//    integer(C_INT), intent(IN), value :: num_elements   !< Number of 32-bit elements in the circular buffer
//    type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
//   end function distributed_circular_buffer_create
//F_EnD
//! Wrapper function to call from Fortran code, with a Fortran MPI communicator
distributed_circular_buffer_p distributed_circular_buffer_create_f(
    int32_t f_communicator, //!< [in] Communicator on which the distributed buffer is shared (in Fortran)
    int32_t rank,           //!< [in] Rank of the current process
    int32_t comm_size,      //!< [in] Number of processes in the communicator
    int32_t num_producers,  //!< [in] Number or producers (circular buffer instances)
    int32_t num_elements    //!< [in] Number of #cb_element tokens in the buffer
) {
  return distributed_circular_buffer_create(MPI_Comm_f2c(f_communicator), rank, comm_size, num_producers, num_elements);
}

//F_StArT
//  subroutine distributed_circular_buffer_print(buffer) BIND(C, name = 'distributed_circular_buffer_print')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
//  end subroutine distributed_circular_buffer_print
//F_EnD
void distributed_circular_buffer_print(distributed_circular_buffer_p buffer //!< [in] Buffer for which to print data
) {
  printf("Printing distributed circ buf, rank: %d\n", buffer->rank);
  if (is_producer(buffer)) {
    printf("Num elems: %d (rank %d)\n", get_available_data(&buffer->local_header.buf), buffer->rank);
  }
  else if (buffer->rank == buffer->num_producers) {
    for (int i = 0; i < buffer->num_producers; i++) {
      const circular_buffer_p b = get_circular_buffer(buffer, i);
      printf("From root: buffer %d has %d data in it\n", i, get_available_data(b));
    }
  }
}

//F_StArT
//  subroutine distributed_circular_buffer_delete(buffer) BIND(C, name = 'distributed_circular_buffer_delete')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
//  end subroutine distributed_circular_buffer_delete
//F_EnD
//! Release all data used by the given distributed circular buffer
void distributed_circular_buffer_delete(distributed_circular_buffer_p buffer //!< [in,out] Buffer to delete
) {
  MPI_Win_free(&buffer->window);
  free(buffer);
}

//F_StArT
//  function distributed_circular_buffer_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'distributed_circular_buffer_put')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value           :: buffer
//    integer(C_INT), dimension(*), intent(in) :: src_data
//    integer(C_INT), intent(in), value        :: num_elements
//    integer(C_INT) :: num_available
//  end function distributed_circular_buffer_put
//F_EnD
//! @brief Insert data into the given buffer, once there is enough space (will wait if there isn't enough initially)
//!
//! The data will be inserted into the buffer instance associated with the calling process.
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the root node (otherwise it might try to read data that has not yet arrived).
//!
//! @return How many elements can still fit after the insertion, if everything went smoothly, -1 otherwise
int distributed_circular_buffer_put(
    distributed_circular_buffer_p buffer,      //!< Distributed buffer in which we want to put data
    cb_element* const             src_data,    //!< Pointer to the data we want to insert
    const int                     num_elements //!< How many 4-byte elements we want to insert
) {
  if (distributed_circular_buffer_wait_space_available(buffer, num_elements) < 0)
    return -1;

  const int target_rank = buffer->local_header.target_rank;

  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, 0, buffer->window);

  int32_t       in_index  = buffer->local_header.buf.m.in;
  const int32_t out_index = buffer->local_header.buf.m.out;
  const int32_t limit     = buffer->local_header.buf.m.limit;

  const int32_t window_offset_base = buffer->window_offset + sizeof(circular_buffer_instance) / sizeof(cb_element);
  if (in_index < out_index) {
    // 1 segment
    const int32_t window_offset = window_offset_base + in_index;
    MPI_Accumulate(
        src_data, num_elements, MPI_INTEGER, target_rank, window_offset, num_elements, MPI_INTEGER, MPI_REPLACE,
        buffer->window);
    in_index += num_elements;
  }
  else {
    // First segment
    const int     num_elem_segment_1 = num_elements > (limit - in_index) ? (limit - in_index) : num_elements;
    const int32_t window_offset_1    = window_offset_base + in_index;
    MPI_Accumulate(
        src_data, num_elem_segment_1, MPI_INTEGER, target_rank, window_offset_1, num_elem_segment_1, MPI_INTEGER,
        MPI_REPLACE, buffer->window);

    // Update temporary insertion pointer
    in_index += num_elem_segment_1;
    if (in_index >= limit)
      in_index = 0;

    // Second segment (if there is one)
    const int     num_elem_segment_2 = num_elements - num_elem_segment_1;
    const int32_t window_offset_2    = window_offset_base + in_index;
    MPI_Accumulate(
        src_data + num_elem_segment_1, num_elem_segment_2, MPI_INTEGER, target_rank, window_offset_2,
        num_elem_segment_2, MPI_INTEGER, MPI_REPLACE, buffer->window);

    // Update temporary insertion pointer
    in_index += num_elem_segment_2;
  }

  // Update insertion index remotely and locally
  const int     index_byte_offset = offsetof(circular_buffer_instance, buf) + offsetof(fiol_management, in);
  const int     index_elem_offset = index_byte_offset / sizeof(cb_element);
  const int32_t window_offset     = buffer->window_offset + index_elem_offset; // TODO IS THIS OK??
  MPI_Accumulate(&in_index, 1, MPI_INTEGER, target_rank, window_offset, 1, MPI_INTEGER, MPI_REPLACE, buffer->window);
  buffer->local_header.buf.m.in = in_index;

  MPI_Win_unlock(buffer->num_producers, buffer->window);

  return get_available_space(&buffer->local_header.buf);
}

//F_StArT
//  function distributed_circular_buffer_get(buffer, buffer_id, dest_data, num_elements) result(num_available) BIND(C, name = 'distributed_circular_buffer_get')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value              :: buffer
//    integer(C_INT), intent(in), value           :: buffer_id
//    integer(C_INT), dimension(*), intent(inout) :: dest_data
//    integer(C_INT), intent(in), value           :: num_elements
//    integer(C_INT) :: num_available
//  end function distributed_circular_buffer_get
//F_EnD
int distributed_circular_buffer_get(
    distributed_circular_buffer_p buffer,      //!<
    const int                     buffer_id,   //!<
    int32_t*                      dest_data,   //!<
    const int                     num_elements //!<
) {
  if (distributed_circular_buffer_wait_data_available(buffer, buffer_id, num_elements) < 0)
    return -1;

  circular_buffer_p queue = get_circular_buffer(buffer, buffer_id);

  const int32_t in_index  = queue->m.in;
  int32_t       out_index = queue->m.out;
  const int32_t limit     = queue->m.limit;

  const int32_t* const buffer_data = queue->data;

  if (out_index < in_index) {
    // 1 segment
    memcpy(dest_data, buffer_data + out_index, num_elements * sizeof(cb_element));
    out_index += num_elements;
  }
  else {
    // 1st segment
    const int num_elements_1 = num_elements > (limit - out_index) ? (limit - out_index) : num_elements;
    memcpy(dest_data, buffer_data + out_index, num_elements_1);

    // Update temporary extraction pointer
    out_index += num_elements_1;
    if (out_index >= limit)
      out_index = 0;

    // 2nd segment (if there is one)
    const int num_elements_2 = num_elements - num_elements_1;
    memcpy(dest_data + num_elements_1, buffer_data + out_index, num_elements_2 * sizeof(cb_element));

    // Update temporary extraction pointer
    out_index += num_elements_2;
  }

  M_FENCE; // Make sure everything has been read, and the temp pointer actually updated

  queue->m.out = out_index; // Update actual extraction pointer

  return get_available_data(queue);
}

//! @}
