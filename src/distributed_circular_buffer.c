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

#include "io-server/circular_buffer.h"

static const MPI_Datatype CB_MPI_ELEMENT_TYPE = sizeof(data_element) == sizeof(int32_t)   ? MPI_INTEGER
                                                : sizeof(data_element) == sizeof(int64_t) ? MPI_LONG_LONG_INT
                                                                                          : MPI_DATATYPE_NULL;

/**
 * @brief Wrapper struct around a regular circular buffer. It adds some information for management within a set of
 * distributed circular buffers.
 */
typedef struct {
  int             target_rank; //!< With which process this instance should communicate for data transfers
  circular_buffer buf;         //!< The buffer contained in this instance
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
 * This data is directly accessible (local load/store) by every consumer process. The only item that is ever modified
 * by a consumer process is the extraction index in a circular buffer instance, whenever that consumer has finished
 * extracting a bunch of data from that instance.
 * In every (remote) communication, the consumer processes are always passive; this removes any need for explicit
 * synchronization related to data transfer.
 */
typedef struct {
  MPI_Comm   communicator; //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win    window;       //!< MPI window into the circular buffers themselves, on the process which holds all data
  MPI_Win    window_mem_dummy; //!< MPI window used only to allocate and free shared memory
  int32_t    rank; //!< Rank of the process that initialized this instance of the distributed buffer description
  int32_t    num_producers; //!< How many producer processes share this distributed buffer set
  data_index window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located
  int32_t    num_element_per_instance; //!< How many elements form a single circular buffer instance in this buffer set

  //! Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  //! Will have some metadata at the beginning
  data_element* raw_data;

  //! Header of the circular buffer instance (only valid for producers)
  //! This is the local copy and will be synchronized with the remote one, located in the shared memory region of the
  //! consumer processes
  circular_buffer_instance local_header;

} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;

/**
 * @brief Small header located at the beginning of the shared memory region on consumer processes. Stores the offset
 * of every instance within that shared memory region.
 * It will be stored in the remotely-accessible MPI window, so everything in it must have a size that is a multiple of
 * the size of an element to be addressable individually
 */
typedef struct {
  data_element num_offsets;      //!< How many offsets there are ( = number of producer processes)
  data_element size;             //!< Size in number of #data_element
  data_index   window_offsets[]; //!< The offsets.
} offset_header;

//! How long to wait between checks for free space in a buffer (microseconds)
static const int SPACE_CHECK_WAIT_TIME_US = 100;
//! How long to wait between checks for data in a buffer (microseconds)
static const int DATA_CHECK_WAIT_TIME_US = 100;

//F_StArT
//  include 'io-server/circular_buffer.inc'
//  interface
//F_EnD

//! @{ \name Offset calculators

//! Size of the offset header, in number of #data_element tokens
static inline data_index compute_offset_header_size(const distributed_circular_buffer_p buffer_set) {
  const data_index num_offsets  = buffer_set->num_producers;
  const size_t     num_bytes    = sizeof(offset_header) + (size_t)num_offsets * sizeof(data_element);
  const size_t     addition     = num_bytes % sizeof(data_element) == 0 ? 0 : 1;
  const data_index num_elements = num_bytes / sizeof(data_element) + addition;

  return num_elements;
}

//! Get a pointer to the offset_header of the given distributed_circular_buffer
static inline offset_header* get_offset_header(
    distributed_circular_buffer_p buffer_set //!< Buffer set from which we want the offset_header
) {
  return (offset_header*)(buffer_set->raw_data);
}

//! Initialize the offset_header of the given distributed_circular_buffer. This computes the offset of every buffer
//! instance within the set.
static inline void init_offset_header(
    distributed_circular_buffer_p buffer_set //!< The distributed buffer set we want to initialize
) {
  offset_header* header = get_offset_header(buffer_set);
  header->num_offsets   = buffer_set->num_producers;
  header->size          = compute_offset_header_size(buffer_set); // Padded to align with sizeof(data_element)

  for (int i = 0; i < header->num_offsets; i++) {
    header->window_offsets[i] = header->size + i * buffer_set->num_element_per_instance;
  }
}

//! \return The displacement in the shared memory window where the window offset for the specified rank (producer ID) is located
static inline MPI_Aint window_offset_displacement(
    const int producer_id //!< [in] ID of the producer for which we want the window offset location (= its rank)
) {
  const MPI_Aint num_elem = (sizeof(offset_header) + (size_t)producer_id * sizeof(data_index)) / sizeof(data_element);
  return num_elem;
}

//! \return The displacement in the shared memory window where the given element index for a circular buffer is located
static inline MPI_Aint buffer_element_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer in which the element is located
    const data_index                    index   //!< [in] Index of the element within the buffer
) {
  // Start of the buffer element section within the window
  const MPI_Aint base_displacement =
      buffer->window_offset + (long)(sizeof(circular_buffer_instance) / sizeof(data_element));
  const MPI_Aint elem_displacement = base_displacement + index;
  return elem_displacement;
}

//! \return The displacement in the shared memory window where the insertion index of the given buffer is located
static inline MPI_Aint insertion_index_displacement(const distributed_circular_buffer_p buffer) {
  const int        index_byte_offset = offsetof(circular_buffer_instance, buf) + offsetof(fiol_management, in);
  const int        index_elem_offset = index_byte_offset / (long)sizeof(data_element);
  const data_index displacement      = buffer->window_offset + index_elem_offset;
  return displacement;
}

//! @}

//! @{ \name Helper functions

//! Check whether the given distributed buffer is located on a consumer process
static inline int is_consumer(distributed_circular_buffer_p buffer) {
  return (buffer->rank >= buffer->num_producers);
}

//! Check whether the given distributed buffer is located on a producer process
static inline int is_producer(distributed_circular_buffer_p buffer) {
  return !is_consumer(buffer);
}

//! Check whether the given distributed buffer is located on the DCB root process
static inline int is_root(distributed_circular_buffer_p buffer) {
  return (buffer->rank == buffer->num_producers);
}

//! Get a pointer to a certain circular_buffer_instance within the given distributed_circular_buffer
static inline circular_buffer_instance_p get_circular_buffer_instance(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want the circ buffer instance
    const int                     buffer_id   //!< [in] ID of the circ buffer instance within the buffer set
) {
  const offset_header* offsets = get_offset_header(buffer_set);
  return (circular_buffer_instance_p)(buffer_set->raw_data + offsets->window_offsets[buffer_id]);
}

//! Retrieve a pointer to a certain circular buffer managed by the given distributed circular buffer set
static inline circular_buffer_p get_circular_buffer(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want a circular buffer
    const int                     buffer_id   //!< [in] ID of the circular buffer within the buffer set
) {
  return &get_circular_buffer_instance(buffer_set, buffer_id)->buf;
}

//! Retrieve the window offset of the appropriate circular buffer instance within the given distributed buffer
//! (according to its rank)
static void retrieve_window_offset_from_remote(
    distributed_circular_buffer_p buffer //!< Buffer for which we want the window offset
) {
  // We communicate through the root process (rank == num_producers), because we don't yet know the rank of our target
  MPI_Win_lock(MPI_LOCK_SHARED, buffer->num_producers, MPI_MODE_NOCHECK, buffer->window);
  MPI_Get(
      &buffer->window_offset, 1, CB_MPI_ELEMENT_TYPE, buffer->num_producers, window_offset_displacement(buffer->rank),
      1, CB_MPI_ELEMENT_TYPE, buffer->window);
  MPI_Win_unlock(buffer->num_producers, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance
static inline void update_local_header_from_remote(
    distributed_circular_buffer_p buffer //!< [in] Buffer set from which we want to update a single instance
) {
  // Gotta load the target rank first, because it's going to be overwritten by the MPI_Get
  const int target_rank =
      buffer->local_header.target_rank >= 0 ? buffer->local_header.target_rank : buffer->num_producers;

  const int num_elem = sizeof(circular_buffer_instance) / sizeof(data_element);
  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);
  MPI_Get(
      &buffer->local_header, num_elem, CB_MPI_ELEMENT_TYPE, target_rank, buffer->window_offset, num_elem,
      CB_MPI_ELEMENT_TYPE, buffer->window);
  MPI_Win_unlock(target_rank, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance and compute how much
//! space is available
//! @return The number of #data_element tokens that can still be stored in the buffer
static inline data_index get_available_space_from_remote(
    const distributed_circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  update_local_header_from_remote(buffer);
  return circular_buffer_get_available_space(&buffer->local_header.buf);
}

//! @brief Stop and wait until there is enough space in this circular buffer instance.
//!
//! If the latest copy of the header shows enough space, return immediately. Otherwise, copy metadata from the consumer
//! process and check, until there is enough space.
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated.
static data_index distributed_circular_buffer_wait_space_available(
    distributed_circular_buffer_p buffer,       //!< [in] Pointer to the distributed buffer we're waiting for
    const int                     num_requested //!< [in] Needed number of available #data_element slots
) {
  if (buffer == NULL || is_consumer(buffer))
    return -1;

  const circular_buffer_p instance = &buffer->local_header.buf;
  if (num_requested < 0 || instance->m.version != FIOL_VERSION)
    return -1;

  // First check locally
  data_index num_available = circular_buffer_get_available_space(instance);
  if (num_available >= num_requested)
    return num_available;

  // Then get info from remote location, until there is enough
  while ((void)(num_available = get_available_space_from_remote(buffer)), num_available < num_requested)
    sleep_us(SPACE_CHECK_WAIT_TIME_US);

  return num_available;
}

//! Stop and wait until there is enough data in this circular buffer instance
//!
//! @return The number of data elements in the buffer, if everything goes smoothly, -1 otherwise.
static data_index distributed_circular_buffer_wait_data_available(
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
  data_index num_available = 0;
  while ((void)(num_available = circular_buffer_get_available_data(instance)), num_available < num_requested) {
    sleep_us(DATA_CHECK_WAIT_TIME_US);
  }

  return num_available;
}

//! @}

//! @{ \name Distributed circular buffer public interface

void distributed_circular_buffer_print(distributed_circular_buffer_p);

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
    MPI_Comm      communicator,  //!< [in] Communicator on which the distributed buffer is shared
    const int32_t num_producers, //!< [in] Number of producer processes in the communicator (number of buffer instances)
    const int32_t num_elements   //!< [in] Number of elems in a single circular buffer (only needed on the root process)
) {
  distributed_circular_buffer_p buffer = (distributed_circular_buffer*)malloc(sizeof(distributed_circular_buffer));

  buffer->communicator             = communicator;
  buffer->num_producers            = num_producers;
  buffer->num_element_per_instance = num_elements;
  MPI_Comm_rank(buffer->communicator, &buffer->rank);
  buffer->local_header.target_rank = -1;

  // Make sure everyone has the same number of elements
  MPI_Bcast(&buffer->num_element_per_instance, 1, MPI_INTEGER, buffer->num_producers, buffer->communicator);

  // We will be using two windows. The first one (created here) is simply to facilitate the allocation and management
  // of shared memory. Only the root allocates shared memory; the other consumers will use that same memory.
  const data_index offset_header_size = compute_offset_header_size(buffer);
  const MPI_Aint   win_total_size     = (num_producers * num_elements + offset_header_size) * sizeof(data_element);
  const MPI_Aint   shared_win_size    = is_root(buffer) ? win_total_size : 0; // Size used for shared memory allocation

  // Create the shared memory window (only for shared memory allocation)
  MPI_Win_allocate_shared(
      shared_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->communicator, &buffer->raw_data,
      &buffer->window_mem_dummy);

  if (is_root(buffer)) {
    // The root initializes every circular buffer, then sends to the corresponding node the offset where
    // that buffer is located in the window. The offset is in number of #data_element.

    init_offset_header(buffer);

    // Compute number of elements that fit in individual buffers (excludes the space taken by some headers)
    const int extra_elem = (sizeof(circular_buffer_instance) - sizeof(circular_buffer)) % sizeof(data_element);
    const int instance_overhead =
        (sizeof(circular_buffer_instance) - sizeof(circular_buffer)) / sizeof(data_element) + extra_elem;
    const int num_elem_in_circ_buffer = buffer->num_element_per_instance - instance_overhead;

    int num_procs;
    MPI_Comm_size(buffer->communicator, &num_procs);
    const int num_consumers = num_procs - num_producers;

    // Initialize the individual buffers
    for (int i = 0; i < num_producers; i++) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i);
      circular_buffer_p          buffer_address  = &buffer_instance->buf;

      circular_buffer_init(buffer_address, num_elem_in_circ_buffer);
      buffer_instance->target_rank = buffer->rank + i % num_consumers; // Assign a target consumer for the buffer
    }
  }
  else if (is_consumer(buffer)) {
    // Consumer nodes that are _not_ the first one (the "root") need to get the proper address in shared memory
    // in order to create the window with it
    MPI_Aint size;
    int      disp_unit;
    MPI_Win_shared_query(buffer->window_mem_dummy, num_producers, &size, &disp_unit, &buffer->raw_data);
  }

  // Create the actual window that will be used for data transmission
  const MPI_Aint final_win_size = is_producer(buffer) ? 0 : win_total_size; // Size used for actual window creation
  MPI_Win_create(
      buffer->raw_data, final_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->communicator, &buffer->window);

  if (is_producer(buffer)) {
    retrieve_window_offset_from_remote(buffer); // Find out where in the window this instance is located
    update_local_header_from_remote(buffer);    // Get the header to sync the instance locally
  }

  return buffer;
}

//F_StArT
//  function distributed_circular_buffer_create(f_communicator, num_producers, num_elements) result(p) BIND(C, name = 'distributed_circular_buffer_create_f')
//    import :: C_PTR, C_INT
//    implicit none
//    integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
//    integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
//    integer(C_INT), intent(IN), value :: num_elements   !< Number of desired #data_element in the circular buffer
//    type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
//   end function distributed_circular_buffer_create
//F_EnD
//! Wrapper function to call from Fortran code, with a Fortran MPI communicator
distributed_circular_buffer_p distributed_circular_buffer_create_f(
    int32_t f_communicator, //!< [in] Communicator on which the distributed buffer is shared (in Fortran)
    int32_t num_producers,  //!< [in] Number or producers (circular buffer instances)
    int32_t num_elements    //!< [in] Number of #data_element tokens in the buffer
) {
  return distributed_circular_buffer_create(MPI_Comm_f2c(f_communicator), num_producers, num_elements);
}

//F_StArT
//  subroutine distributed_circular_buffer_print(buffer) BIND(C, name = 'distributed_circular_buffer_print')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
//  end subroutine distributed_circular_buffer_print
//F_EnD
//! Print some debug info
void distributed_circular_buffer_print(distributed_circular_buffer_p buffer //!< [in] Buffer for which to print data
) {
  printf(
      "Printing distributed circ buf, rank: %d, num producers %d, buf sizes %d\n", buffer->rank, buffer->num_producers,
      buffer->num_element_per_instance);
  if (is_producer(buffer)) {
    printf(
        "Num elems: %ld (rank %d)\n", (int64_t)circular_buffer_get_available_data(&buffer->local_header.buf),
        buffer->rank);
  }
  else if (is_root(buffer)) {
    for (int i = 0; i < buffer->num_producers; i++) {
      const circular_buffer_p b = get_circular_buffer(buffer, i);
      printf("From root: buffer %d has %ld data in it\n", i, (int64_t)circular_buffer_get_available_data(b));
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
  MPI_Win_free(&buffer->window_mem_dummy);
  free(buffer);
}

//F_StArT
//  function distributed_circular_buffer_get_latest_num_elements(buffer) result(num_elements) BIND(C, name = 'distributed_circular_buffer_get_latest_num_elements')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_elements
//  end function distributed_circular_buffer_get_latest_num_elements
//F_EnD
//! Check how many elements are stored in the buffer, after updating from the remote.
//! _Can only be called from a producer process._
//! @return The (updated) number of elements in the buffer, or -1 if called from a consumer process
int32_t distributed_circular_buffer_get_latest_num_elements(distributed_circular_buffer_p buffer //!< [in]
) {
  if (is_producer(buffer)) {
    update_local_header_from_remote(buffer);
    return (int32_t)circular_buffer_get_available_data(&buffer->local_header.buf);
  }

  return -1;
}

//F_StArT
//  function distributed_circular_buffer_get_num_elements(buffer, buffer_id) result(num_elements) BIND(C, name = 'distributed_circular_buffer_get_num_elements')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value    :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT) :: num_elements
//  end function distributed_circular_buffer_get_num_elements
//F_EnD
//! Check how many elements are stored in the specified buffer.
//! _Can only be called from a consumer process._
//! @return The number of elements in the specified buffer, or -1 if called from a producer process
int32_t distributed_circular_buffer_get_num_elements(
    distributed_circular_buffer_p buffer,   //!< [in]
    const int                     buffer_id //!< [in]
) {
  if (is_consumer(buffer))
    return (int32_t)circular_buffer_get_available_data(get_circular_buffer(buffer, buffer_id));

  return -1;
}

//F_StArT
//  function distributed_circular_buffer_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'distributed_circular_buffer_put')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value           :: buffer
//    integer(DATA_ELEMENT), dimension(*), intent(in) :: src_data
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
data_index distributed_circular_buffer_put(
    distributed_circular_buffer_p buffer,      //!< Distributed buffer in which we want to put data
    data_element* const           src_data,    //!< Pointer to the data we want to insert
    const int                     num_elements //!< How many 4-byte elements we want to insert
) {
  if (distributed_circular_buffer_wait_space_available(buffer, num_elements) < 0)
    return -1;

  const int target_rank = buffer->local_header.target_rank;

  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);

  data_index       in_index  = buffer->local_header.buf.m.in;
  const data_index out_index = buffer->local_header.buf.m.out;
  const data_index limit     = buffer->local_header.buf.m.limit;

  if (in_index < out_index) {
    // 1 segment
    MPI_Accumulate(
        src_data, num_elements, CB_MPI_ELEMENT_TYPE, target_rank, buffer_element_displacement(buffer, in_index),
        num_elements, CB_MPI_ELEMENT_TYPE, MPI_REPLACE, buffer->window);
    in_index += num_elements;
  }
  else {
    // First segment
    const int num_elem_segment_1 = num_elements > (limit - in_index) ? (limit - in_index) : num_elements;
    MPI_Accumulate(
        src_data, num_elem_segment_1, CB_MPI_ELEMENT_TYPE, target_rank, buffer_element_displacement(buffer, in_index),
        num_elem_segment_1, CB_MPI_ELEMENT_TYPE, MPI_REPLACE, buffer->window);

    // Update temporary insertion pointer
    in_index += num_elem_segment_1;
    if (in_index >= limit)
      in_index = 0;

    // Second segment (if there is one)
    const int num_elem_segment_2 = num_elements - num_elem_segment_1;
    MPI_Accumulate(
        src_data + num_elem_segment_1, num_elem_segment_2, CB_MPI_ELEMENT_TYPE, target_rank,
        buffer_element_displacement(buffer, in_index), num_elem_segment_2, CB_MPI_ELEMENT_TYPE, MPI_REPLACE,
        buffer->window);

    // Update temporary insertion pointer
    in_index += num_elem_segment_2;
  }

  // Update insertion index remotely and locally
  MPI_Accumulate(
      &in_index, 1, CB_MPI_ELEMENT_TYPE, target_rank, insertion_index_displacement(buffer), 1, CB_MPI_ELEMENT_TYPE,
      MPI_REPLACE, buffer->window);
  buffer->local_header.buf.m.in = in_index;

  MPI_Win_unlock(target_rank, buffer->window);

  return circular_buffer_get_available_space(&buffer->local_header.buf);
}

//F_StArT
//  function distributed_circular_buffer_get(buffer, buffer_id, dest_data, num_elements) result(num_available) BIND(C, name = 'distributed_circular_buffer_get')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value              :: buffer
//    integer(C_INT), intent(in), value           :: buffer_id
//    integer(DATA_ELEMENT), dimension(*), intent(inout) :: dest_data
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

  const data_element* const buffer_data = queue->data;

  if (out_index < in_index) {
    // 1 segment
    copy_elements(dest_data, buffer_data + out_index, num_elements);
    out_index += num_elements;
  }
  else {
    // 1st segment
    const int num_elements_1 = num_elements > (limit - out_index) ? (limit - out_index) : num_elements;
    copy_elements(dest_data, buffer_data + out_index, num_elements_1);

    // Update temporary extraction pointer
    out_index += num_elements_1;
    if (out_index >= limit)
      out_index = 0;

    // 2nd segment (if there is one)
    const int num_elements_2 = num_elements - num_elements_1;
    copy_elements(dest_data + num_elements_1, buffer_data + out_index, num_elements_2);

    // Update temporary extraction pointer
    out_index += num_elements_2;
  }

  memory_fence(); // Make sure everything has been read, and the temp pointer actually updated

  queue->m.out = out_index; // Update actual extraction pointer

  return circular_buffer_get_available_data(queue);
}

//! @}
//F_StArT
//  end interface
//F_EnD
