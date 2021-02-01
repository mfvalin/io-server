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

#include <immintrin.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>

//C_StArT
#include <mpi.h>

#include "io-server/circular_buffer.h"
//C_EnD

static const MPI_Datatype CB_MPI_ELEMENT_TYPE = sizeof(data_element) == sizeof(int32_t)   ? MPI_INTEGER
                                                : sizeof(data_element) == sizeof(int64_t) ? MPI_LONG_LONG_INT
                                                                                          : MPI_DATATYPE_NULL;

//C_StArT
/**
 * @brief Needs to be aligned to size of #data_element
 */
typedef struct {
  uint64_t num_transfers;
  uint64_t num_elem;
  double   total_wait_time_ms;
} DCB_stats;

/**
 * @brief Wrapper struct around a regular circular buffer. It adds some information for management within a set of
 * distributed circular buffers.
 *
 * Please don't change the order of data members (unless you know what the consequences are).
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
 * an instance of the entire set description, including the "root" process. The set is composed of multiple circular
 * buffer "instances".
 * The processes that will read data from the buffer instance(s) are also called the "consumers" and share ownership
 * of the data of all the collectively created circular buffers in the distributed set.
 * _They must be located on the same physical node._
 * One of the consumer processes is considered the "root" process and is responsible for allocating/deallocating shared
 * memory and for initializing the buffer instances.
 * The processes that insert data into the buffer instances are the "producers" and each only hold a copy of the header
 * of its circular buffer instance. Each producer is associated with exactly one buffer instance (and vice versa).
 * The data on the consumer processes also forms an MPI window into shared memory, which is
 * accessible remotely by every producer process that collectively own this distributed buffer set.
 * _However, each producer will only ever access the portion of that window that contains its associated buffer
 * instance._ This data is directly accessible (local load/store) by every consumer process. The only item that is ever
 * modified by a consumer process is the extraction index in a circular buffer instance, whenever that consumer has
 * finished extracting a bunch of data from that instance. In every (remote) communication, the consumer processes are
 * always passive; this removes any need for explicit synchronization related to data transfer. To enable a larger
 * bandwidth for remote data transfers, producers are able to access the shared memory window through multiple channels
 * (one process per channel). To ensure that the data will be received as quickly as possible from the passive/target
 * side, the "receiver" process of each channel is constantly polling for updates by synchronizing the window.
 * _The receiver processes must be located on the same physical node as the consumers._
 */
typedef struct {
  int32_t    rank; //!< Rank of the process that initialized this instance of the distributed buffer description
  int32_t    num_producers; //!< How many producer processes share this distributed buffer set
  int32_t    num_channels;  //!< How many channels can be used for MPI 1-sided communication (1 PE per channel)
  int32_t    num_element_per_instance; //!< How many elements form a single circular buffer instance in this buffer set
  data_index window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located

  int32_t receiver_id;
  int32_t consumer_id;

  MPI_Comm communicator; //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win  window;       //!< MPI window into the circular buffers themselves, on the process which holds all data
  MPI_Win  window_mem_dummy;    //!< MPI window used only to allocate and free shared memory
  MPI_Comm server_communicator; //!< Communicator that groups processes located on the IO server

  //! Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  //! Will have some metadata at the beginning
  data_element* raw_data;

  DCB_stats  producer_stats;
  MPI_Win    consumer_stats_window;
  DCB_stats* consumer_stats;

  //! Header of the circular buffer instance (only valid for producers)
  //! This is the local copy and will be synchronized with the remote one, located in the shared memory region of the
  //! consumer processes
  circular_buffer_instance local_header;

} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;
//C_EnD

enum receiver_signal
{
  RSIG_NONE = 0,
  RSIG_SERVER_BARRIER,
  RSIG_FULL_BARRIER,
  RSIG_STOP,
};

typedef enum receiver_signal receiver_signal_t;

/**
 * @brief Small header located at the beginning of the shared memory region on consumer processes. Stores the offset
 * of every instance within that shared memory region.
 * It will be stored in the remotely-accessible MPI window, so everything in it must have a size that is a multiple of
 * the size of an element to be addressable individually. So basically, just data_elements
 */
typedef struct {
  data_element num_offsets; //!< How many offsets there are ( = number of producer processes)
  data_element size;        //!< Size in number of #data_element
  data_element num_signals;
  data_element window_offsets[]; //!< The offsets.
  // After the offsets, there are the signals, but we can't name them explicitly. We really need a better solution
} offset_header;

typedef offset_header* offset_header_p;

//! How long to wait between checks for free space in a buffer (microseconds)
static const int SPACE_CHECK_DELAY_US = 100;
//! How long to wait between checks for data in a buffer (microseconds)
static const int DATA_CHECK_DELAY_US = 20;
//! How long to wait between each window sync from a receiver process
static const int WINDOW_SYNC_DELAY_US = 10;

//F_StArT
//  include 'io-server/circular_buffer.inc'
//  interface
//F_EnD

//! @{ @name Offset calculators

//! Compute the number of #data_element taken by the given number of bytes (rounded up)
static inline data_index num_elem_from_bytes(const size_t num_bytes) {
  return num_bytes / sizeof(data_element) + (num_bytes % sizeof(data_element) > 0);
}

//! Compute the number of #data_element taken by the circular_buffer_instance struct
static inline data_index instance_header_size() {
  return num_elem_from_bytes(sizeof(circular_buffer_instance));
}

//! Compute the number of #data_element taken by the circular_buffer struct
static inline data_index circular_buffer_header_size() {
  return num_elem_from_bytes(sizeof(circular_buffer));
}

//! Size of the offset header, in number of #data_element tokens
static inline data_index offset_header_size(
    const int num_buffers, //!< Number of circular buffers in the set
    const int num_channels //!< Number of communication channels (PEs) used for MPI 1-sided calls
) {
  const size_t num_bytes = sizeof(offset_header) + (size_t)(num_buffers) * sizeof(data_element) +
                           (size_t)num_channels * sizeof(receiver_signal_t);
  return num_elem_from_bytes(num_bytes);
}

static inline data_index total_circular_buffer_size(const int num_desired_elem) {
  return num_desired_elem + circular_buffer_header_size() + 1;
}

//! Size of an entire circular buffer instance, based on the number of
static inline data_index total_circular_buffer_instance_size(const int num_desired_elem) {
  return instance_header_size() + total_circular_buffer_size(num_desired_elem);
}

//! Compute the total size needed by the shared memory window to fit all circular buffers and metadata, in number of
//! #data_element tokens
static inline data_index total_window_num_elem(
    const int num_buffers,                //!< How many circular buffers the set will hold
    const int num_channels,               //!< How many communication channels with the IO server will be used
    const int num_desired_elem_per_buffer //!< How many #data_element tokens we want to be able to store in each buffer
) {
  return num_buffers * (total_circular_buffer_instance_size(num_desired_elem_per_buffer)) +
         offset_header_size(num_buffers, num_channels);
}

//! Get a pointer to the offset_header of the given distributed_circular_buffer
static inline offset_header_p get_offset_header(
    distributed_circular_buffer_p buffer_set //!< Buffer set from which we want the offset_header
) {
  return (offset_header_p)(buffer_set->raw_data);
}

//! Get a pointer to the signal location of the given receiver process (ID)
static inline receiver_signal_t* get_receiver_signal_ptr(
    distributed_circular_buffer_p buffer,     //!< [in] Buffer whose receiver signal we want
    const int32_t                 receiver_id //!< [in] ID of the receiver we're looking for
) {
  return ((receiver_signal_t*)&get_offset_header(buffer)->window_offsets[buffer->num_producers]) + receiver_id;
}

//! Initialize the offset_header of the given distributed_circular_buffer. This computes the offset of every buffer
//! instance within the set.
static inline void init_offset_header(
    offset_header_p header,                   //!< [in] Pointer to the header that needs to be initialized
    const int       num_buffers,              //!< [in] How many circular buffer instances there are in the set
    const int       num_channels,             //!< [in]
    const int       num_elements_per_instance //!< [in] How many elements each instance takes
) {
  header->num_offsets = num_buffers;
  header->size        = offset_header_size(num_buffers, num_channels);
  header->num_signals = num_channels;

  for (int i = 0; i < header->num_offsets; ++i) {
    header->window_offsets[i] = header->size + i * (total_circular_buffer_instance_size(num_elements_per_instance));
  }

  receiver_signal_t* signals = (receiver_signal_t*)(header->window_offsets + header->num_offsets);
  for (int i = 0; i < num_channels; ++i) {
    signals[i] = RSIG_NONE;
  }
}

static inline void print_offset_header(offset_header_p header) {
  printf(
      "Num buffers %d, size %d bytes, num channels %d\n"
      "Address %ld\n",
      header->num_offsets, header->size, header->num_signals, (long)header);
  printf("Offsets: ");
  for (int i = 0; i < header->num_offsets; ++i) {
    printf("%ld ", (long)header->window_offsets[i]);
  }
  printf("\nSignals: ");
  receiver_signal_t* signals = (receiver_signal_t*)(header->window_offsets + header->num_offsets);
  for (int i = 0; i < header->num_signals; ++i) {
    printf("%ld ", (long)signals[i]);
  }
  printf("\n");
}

//! Compute the displacement in the shared memory window where the window offset for the specified rank (producer ID) is
//! located That displacement should land in the appropriate location in the offset_header struct, which is itself at
//! the start of the window
//! @return The window displacement where a producer can find the offset (displacement) of its buffer within that window
static inline MPI_Aint window_offset_displacement(
    const int producer_id //!< [in] ID of the producer for which we want the window offset location (= its rank)
) {
  const MPI_Aint num_elem = (sizeof(offset_header) + (size_t)producer_id * sizeof(data_element)) / sizeof(data_element);
  return num_elem;
}

//! @return The displacement in the shared memory window where the given element index for a circular buffer is located
static inline MPI_Aint buffer_element_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer in which the element is located
    const data_index                    index   //!< [in] Index of the element within the buffer
) {
  // Start of the buffer element section within the window
  const MPI_Aint base_displacement = buffer->window_offset + instance_header_size();
  const MPI_Aint elem_displacement = base_displacement + index;
  return elem_displacement;
}

//! @return The displacement in the shared memory window where the insertion index of the given buffer is located
static inline MPI_Aint insertion_index_displacement(const distributed_circular_buffer_p buffer) {
  // TODO We'd better wish the offset is aligned to the size of a data_element
  const int        index_byte_offset = offsetof(circular_buffer_instance, buf) + offsetof(fiol_management, in);
  const int        index_elem_offset = num_elem_from_bytes(index_byte_offset);
  const data_index displacement      = buffer->window_offset + index_elem_offset;
  return displacement;
}

//! Get the displacement (in number of #data_element) in the shared memory window where the start of the buffer is
//! located. Must be called from a producer process
static inline MPI_Aint remote_header_displacement(const distributed_circular_buffer_p buffer) {
  return buffer->window_offset;
}

//! @}

//! @{ @name Helper functions

//! Rank of the "root" process, responsible for managing the buffer's shared memory
static inline int get_root_id(const distributed_circular_buffer_p buffer) {
  return buffer->num_producers + buffer->num_channels;
}

static inline int get_server_comm_root_id(const distributed_circular_buffer_p buffer) {
  return buffer->num_channels;
}

//! Check whether the given distributed buffer is located on a consumer process
static inline int is_consumer(const distributed_circular_buffer_p buffer) {
  return (buffer->consumer_id >= 0);
}

//! Check whether the given distributed buffer is located on a producer process
static inline int is_producer(const distributed_circular_buffer_p buffer) {
  return (buffer->rank < buffer->num_producers);
}

//! Check whether the given distributed buffer is located on the DCB root process
static inline int is_root(const distributed_circular_buffer_p buffer) {
  return (buffer->rank == get_root_id(buffer));
}

//! Check whether the given distributed buffer is located on a receiver process
static inline int is_receiver(const distributed_circular_buffer_p buffer) {
  return (buffer->receiver_id >= 0);
}

//! Get a pointer to a certain circular_buffer_instance within the given distributed_circular_buffer
static inline circular_buffer_instance_p get_circular_buffer_instance(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want the circ buffer instance
    const int                     buffer_id   //!< [in] ID of the circ buffer instance within the buffer set
) {
  const offset_header_p offsets = get_offset_header(buffer_set);
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
  // We communicate through the root process, because we don't yet know the rank of our target
  const int root_rank = get_root_id(buffer);
  MPI_Win_lock(MPI_LOCK_SHARED, root_rank, MPI_MODE_NOCHECK, buffer->window);
  MPI_Get(
      &buffer->window_offset, 1, CB_MPI_ELEMENT_TYPE, root_rank, window_offset_displacement(buffer->rank), 1,
      CB_MPI_ELEMENT_TYPE, buffer->window);
  MPI_Win_unlock(root_rank, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance
static inline void update_local_header_from_remote(
    distributed_circular_buffer_p buffer //!< [in] Buffer set from which we want to update a single instance
) {
  // Gotta load the target rank first, because it's going to be overwritten by the MPI_Get
  const int target_rank =
      buffer->local_header.target_rank >= 0 ? buffer->local_header.target_rank : get_root_id(buffer);
  const int num_elem = instance_header_size();
  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);
  MPI_Get(
      &buffer->local_header, num_elem, CB_MPI_ELEMENT_TYPE, target_rank, remote_header_displacement(buffer), num_elem,
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
  return CB_get_available_space(&buffer->local_header.buf);
}

//! @brief Stop and wait until there is enough space in this circular buffer instance.
//!
//! If the latest copy of the header shows enough space, return immediately. Otherwise, copy metadata from the consumer
//! process and check, until there is enough space.
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated.
static data_index DCB_wait_space_available(
    distributed_circular_buffer_p buffer,       //!< [in] Pointer to the distributed buffer we're waiting for
    const int                     num_requested //!< [in] Needed number of available #data_element slots
) {
  if (buffer == NULL || is_consumer(buffer))
    return -1;

  const circular_buffer_p instance = &buffer->local_header.buf;
  if (num_requested < 0 || instance->m.version != FIOL_VERSION)
    return -1;

  // First check locally
  data_index num_available = CB_get_available_space(instance);
  if (num_available >= num_requested)
    return num_available;

  int num_waits = 0;
  // Then get info from remote location, until there is enough
  while ((void)(num_available = get_available_space_from_remote(buffer)), num_available < num_requested) {
    num_waits++;
    sleep_us(SPACE_CHECK_DELAY_US);
  }

  buffer->producer_stats.total_wait_time_ms += num_waits * SPACE_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//! Stop and wait until there is enough data in this circular buffer instance
//!
//! @return The number of data elements in the buffer, if everything goes smoothly, -1 otherwise.
static data_index DCB_wait_data_available(
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
  int        num_waits     = 0;
  while ((void)(num_available = CB_get_available_data(instance)), num_available < num_requested) {
    num_waits++;
    sleep_us(DATA_CHECK_DELAY_US);
  }

  buffer->consumer_stats[buffer_id].total_wait_time_ms += num_waits * DATA_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//! Initialize the given circular buffer instance, including the circular_buffer it contains
static circular_buffer_p init_circular_buffer_instance(
    circular_buffer_instance_p instance, //!< Buffer instance we want to init
    const int                  num_elem  //!< How many #data_element are taken by the circular buffer
) {
  instance->target_rank = -1;
  return CB_init(&instance->buf, num_elem);
}

//! Initialize the DCB_stats struct to all 0
static void DCB_init_stats(DCB_stats* stats) {
  stats->num_transfers      = 0;
  stats->num_elem           = 0;
  stats->total_wait_time_ms = 0.0;
}

//! Print the collected stats for a single buffer instance
static void print_instance_stats(
    const DCB_stats* producer_stats, //!< Stats from the producer side of the buffer
    const DCB_stats* consumer_stats, //!< Stats from the consumer side of the buffer
    const int        id,             //!< ID of the buffer (to prefix the stats)
    const int        with_header     //!< Whether to print a header to name the columns
) {
  const uint64_t num_puts = producer_stats->num_transfers;
  const uint64_t num_gets = consumer_stats->num_transfers;

  const double avg_in     = num_puts > 0 ? (double)producer_stats->num_elem / num_puts : 0.0;
  const double avg_out    = num_gets > 0 ? (double)consumer_stats->num_elem / num_gets : 0.0;
  const double avg_wait_w = num_puts > 0 ? (double)producer_stats->total_wait_time_ms / num_puts : 0.0;
  const double avg_wait_r = num_gets > 0 ? (double)consumer_stats->total_wait_time_ms / num_gets : 0.0;

  if (with_header) {
    printf("rank: #elem put (avg/call) -- #elem got (avg/call) -- write wait (avg/call) --  read wait (avg/call)\n");
  }

  printf(
      " %03d:  %8ld (%8.1f) --  %8ld (%8.1f) -- %7.3f ms (%8.5f) -- %7.3f ms (%8.5f)\n", id, producer_stats->num_elem,
      avg_in, consumer_stats->num_elem, avg_out, producer_stats->total_wait_time_ms, avg_wait_w,
      consumer_stats->total_wait_time_ms, avg_wait_r);
}

//! Set the variable at a given address to a certain value, atomically, as soon as the old value is RSIG_NONE
static inline void set_receiver_signal(receiver_signal_t* signal_ptr, const receiver_signal_t value) {
  while (__sync_val_compare_and_swap(signal_ptr, RSIG_NONE, value) != 0)
    ;
}

//! Set the signal of every receiver associated with a buffer set to the given value.
//! Should only be called from a single process at a time, located on the IO server (so basically a consumer process,
//! because the receivers are normally busy)
static inline void send_receiver_signal(distributed_circular_buffer_p buffer, const receiver_signal_t value) {
  for (int i = 0; i < buffer->num_channels; ++i)
    set_receiver_signal(get_receiver_signal_ptr(buffer, i), value);
}

//! @}

//! @{ \name Distributed circular buffer public interface

//C_StArT
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p);
void DCB_full_barrier(distributed_circular_buffer_p buffer);
//C_EnD

//F_StArT
//  subroutine DCB_sync_window(buffer) BIND(C, name = 'DCB_sync_window')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//   end subroutine DCB_sync_window
//F_EnD
void DCB_sync_window(distributed_circular_buffer_p buffer) {
  MPI_Win_sync(buffer->window);
}

//! @brief Create a set of distributed circular buffers on a set of processes.
//!
//! Some of the processes are consumers and hold the data for a circular buffer instance for each other process,
//! in a shared memory window. _It is the caller's responsibility to ensure that all consumer processes are located on
//! the same physical node._
//! The other processes are producers and only get a copy of the header of their
//! circular buffer, as well as an offset that points to the location of their data within the shared window.
//!
//! @return A pointer to a newly-allocated distributed circular buffer struct that contains all the relevant info
//C_StArT
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,        //!< [in] Communicator on which the distributed buffer is shared
    MPI_Comm      server_communicator, //!< [in] Communicator that groups server processes
    const int32_t num_producers, //!< [in] Number of producer processes in the communicator (number of buffer instances)
    const int32_t num_channels,  //!< [in] Number of processes that can be the target of MPI 1-sided comm (receivers)
    const int32_t num_elements   //!< [in] Number of elems in a single circular buffer (only needed on the root process)
    )
//C_EnD
{
  distributed_circular_buffer_p buffer = (distributed_circular_buffer*)malloc(sizeof(distributed_circular_buffer));

  buffer->communicator             = communicator;
  buffer->server_communicator      = MPI_COMM_NULL;
  buffer->num_producers            = num_producers;
  buffer->num_channels             = num_channels;
  buffer->num_element_per_instance = num_elements;
  buffer->local_header.target_rank = -1;
  MPI_Comm_rank(buffer->communicator, &buffer->rank);

  // Set internal ID for each type of process participating in the buffer set
  {
    buffer->receiver_id = -1;
    buffer->consumer_id = -1;

    // Either a receiver or a consumer
    if (buffer->rank >= buffer->num_producers) {
      if (buffer->rank < buffer->num_producers + buffer->num_channels) {
        buffer->receiver_id = buffer->rank - buffer->num_producers;
      }
      else {
        buffer->consumer_id = buffer->rank - buffer->num_producers - buffer->num_channels;
      }
    }
  }

  // Make sure everyone has the same number of elements
  MPI_Bcast(&buffer->num_element_per_instance, 1, MPI_INTEGER, get_root_id(buffer), buffer->communicator);

  const MPI_Aint win_total_size =
      total_window_num_elem(buffer->num_producers, buffer->num_channels, buffer->num_element_per_instance) *
      (MPI_Aint)sizeof(data_element);
  const MPI_Aint shared_win_size = is_root(buffer) ? win_total_size : 0; // Size used for shared memory allocation

  if (is_consumer(buffer) || is_receiver(buffer)) {
    buffer->server_communicator = server_communicator;

    // Create the shared memory window (only for shared memory allocation). Only the root is allocating a non-zero amount
    MPI_Win_allocate_shared(
        shared_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->server_communicator, &buffer->raw_data,
        &buffer->window_mem_dummy);

    // This window is only used to allocate shared memory, so that everyone on the server can update stats
    const MPI_Aint consumer_stats_size = is_root(buffer) ? buffer->num_producers * sizeof(DCB_stats) : 0;
    MPI_Win_allocate_shared(
        consumer_stats_size, 1, MPI_INFO_NULL, buffer->server_communicator, &buffer->consumer_stats,
        &buffer->consumer_stats_window);
  }

  int init_result = 1;

  if (is_root(buffer)) {
    // The root initializes every circular buffer, then sends to the corresponding node the offset where
    // that buffer is located in the window. The offset is in number of #data_element.

    init_offset_header(
        get_offset_header(buffer), buffer->num_producers, buffer->num_channels, buffer->num_element_per_instance);

    // Compute number of elements that fit in individual buffers (excludes the space taken by some headers)
    const int num_elem_in_circ_buffer = total_circular_buffer_size(buffer->num_element_per_instance);

    int num_procs;
    MPI_Comm_size(buffer->communicator, &num_procs);

    // Initialize the individual buffers
    for (int i = 0; i < num_producers; i++) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i);
      init_result = init_result && (init_circular_buffer_instance(buffer_instance, num_elem_in_circ_buffer) != NULL);
      // Assign a target consumer for the buffer
      buffer_instance->target_rank = buffer->num_producers + i % num_channels;

      DCB_init_stats(&buffer->consumer_stats[i]);
    }
  }
  else if (is_consumer(buffer) || is_receiver(buffer)) {
    // Consumer nodes that are _not_ the first one (the "root") need to get the proper address in shared memory
    // in order to create the window with it
    MPI_Aint size;
    int      disp_unit;
    MPI_Win_shared_query(
        buffer->window_mem_dummy, get_server_comm_root_id(buffer), &size, &disp_unit, &buffer->raw_data);
    MPI_Win_shared_query(
        buffer->consumer_stats_window, get_server_comm_root_id(buffer), &size, &disp_unit, &buffer->consumer_stats);
  }

  // Create the actual window that will be used for data transmission, using a pointer to the same shared memory on the server
  const MPI_Aint final_win_size = is_producer(buffer) ? 0 : win_total_size; // Size used for actual window creation
  MPI_Win_create(
      buffer->raw_data, final_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->communicator, &buffer->window);

  MPI_Bcast(&init_result, 1, MPI_INTEGER, get_root_id(buffer), buffer->communicator);

  if (!init_result) {
    DCB_delete(buffer);
    return NULL;
  }

  if (is_producer(buffer)) {
    DCB_init_stats(&buffer->producer_stats);

    retrieve_window_offset_from_remote(buffer); // Find out where in the window this instance is located
    update_local_header_from_remote(buffer);    // Get the header to sync the instance locally
  }

  // Wait until everyone (especially the producers) is properly initialized before allowing the use of the buffer
  MPI_Barrier(buffer->communicator);

  return buffer;
}

//F_StArT
//  function DCB_create(f_communicator, f_server_communicator, num_producers, num_channels, num_elements) result(p) BIND(C, name = 'DCB_create_f')
//    import :: C_PTR, C_INT
//    implicit none
//    integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
//    integer(C_INT), intent(IN), value :: f_server_communicator !< Communicator that groups the server processes
//    integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
//    integer(C_INT), intent(IN), value :: num_channels   !< Number of receivers (PEs used for communication only)
//    integer(C_INT), intent(IN), value :: num_elements   !< Number of desired #data_element in the circular buffer
//    type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
//   end function DCB_create
//F_EnD
//! Wrapper function to call from Fortran code, with a Fortran MPI communicator
distributed_circular_buffer_p DCB_create_f(
    int32_t f_communicator,        //!< [in] Communicator on which the distributed buffer is shared (in Fortran)
    int32_t f_server_communicator, //!< [in] Communicator that groups server processes (in Fortran)
    int32_t num_producers,         //!< [in] Number or producers (circular buffer instances)
    int32_t num_channels,          //!< [in] Number of processes that can be the target of MPI 1-sided comm (receivers)
    int32_t num_elements           //!< [in] Number of #data_element tokens in the buffer
) {
  return DCB_create(
      MPI_Comm_f2c(f_communicator), MPI_Comm_f2c(f_server_communicator), num_producers, num_channels, num_elements);
}

//F_StArT
//  subroutine DCB_print(buffer) BIND(C, name = 'DCB_print')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
//  end subroutine DCB_print
//F_EnD
//! Print some debug info
void DCB_print(distributed_circular_buffer_p buffer //!< [in] Buffer for which to print data
) {
  printf(
      "Printing distributed circ buf, rank: %d, num producers %d, num channels %d, buf sizes %d, window offset = %d\n",
      buffer->rank, buffer->num_producers, buffer->num_channels, buffer->num_element_per_instance,
      buffer->window_offset);
  if (is_producer(buffer)) {
    printf(
        "Num elems: %ld, num spaces: %ld (rank %d, might be outdated)\n"
        "Target rank: %ld\n"
        "version: %d, first %d, in %d, out %d, limit %d\n",
        (int64_t)CB_get_available_data(&buffer->local_header.buf),
        (int64_t)CB_get_available_space(&buffer->local_header.buf), buffer->rank,
        (int64_t)buffer->local_header.target_rank, buffer->local_header.buf.m.version, buffer->local_header.buf.m.first,
        buffer->local_header.buf.m.in, buffer->local_header.buf.m.out, buffer->local_header.buf.m.limit);
  }
  else if (is_root(buffer)) {
    for (int i = 0; i < buffer->num_producers; ++i) {
      const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, i);
      printf("From root: buffer %d has %ld data in it\n", i, (int64_t)CB_get_available_data(&instance->buf));
    }
  }
}

//F_StArT
//  subroutine DCB_delete(buffer) BIND(C, name = 'DCB_delete')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
//  end subroutine DCB_delete
//F_EnD
//! Release all data used by the given distributed circular buffer
void DCB_delete(distributed_circular_buffer_p buffer //!< [in,out] Buffer to delete
) {
  DCB_full_barrier(buffer); // Make sure all transfers are done before we start the deletion process

  if (is_producer(buffer)) {
    MPI_Send(
        &buffer->producer_stats, sizeof(DCB_stats), MPI_BYTE, get_root_id(buffer), buffer->rank, buffer->communicator);
  }

  if (is_root(buffer)) {
    send_receiver_signal(buffer, RSIG_STOP);

    for (int i = 0; i < buffer->num_producers; ++i) {
      DCB_stats  producer_stats;
      MPI_Status status;
      MPI_Recv(&producer_stats, sizeof(DCB_stats), MPI_BYTE, i, i, buffer->communicator, &status);
      print_instance_stats(&producer_stats, &buffer->consumer_stats[i], i, i == 0);
    }
  }

  MPI_Win_free(&buffer->window);

  if (is_consumer(buffer) || is_receiver(buffer)) {
    MPI_Win_free(&buffer->window_mem_dummy);
    MPI_Win_free(&buffer->consumer_stats_window);
  }

  free(buffer);
}

//F_StArT
//  function DCB_get_latest_num_elements(buffer) result(num_elements) BIND(C, name = 'DCB_get_latest_num_elements')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_elements
//  end function DCB_get_latest_num_elements
//F_EnD
//! Check how many elements are stored in the buffer, after updating from the remote.
//! _Can only be called from a producer process._
//! @return The (updated) number of elements in the buffer, or -1 if called from a consumer process
int32_t DCB_get_latest_num_elements(distributed_circular_buffer_p buffer //!< [in]
) {
  if (is_producer(buffer)) {
    update_local_header_from_remote(buffer);
    return (int32_t)CB_get_available_data(&buffer->local_header.buf);
  }

  return -1;
}

//F_StArT
//  function DCB_get_num_elements(buffer, buffer_id) result(num_elements) BIND(C, name = 'DCB_get_num_elements')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value    :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT) :: num_elements
//  end function DCB_get_num_elements
//F_EnD
//! Check how many elements are stored in the specified buffer.
//! _Can only be called from a consumer process._
//! @return The number of elements in the specified buffer, or -1 if called from a producer process
//C_StArT
int32_t DCB_get_num_elements(
    distributed_circular_buffer_p buffer,   //!< [in]
    const int                     buffer_id //!< [in]
    )
//C_EnD
{
  if (is_consumer(buffer))
    return (int32_t)CB_get_available_data(get_circular_buffer(buffer, buffer_id));
  return -1;
}

//F_StArT
// function DCB_get_producer_id(buffer) result(producer_id) BIND(C, name = 'DCB_get_producer_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: producer_id
// end function DCB_get_producer_id
//F_EnD
int32_t DCB_get_producer_id(const distributed_circular_buffer_p buffer) {
  if (is_producer(buffer))
    return buffer->rank;
  return -1;
}

//F_StArT
// function DCB_get_receiver_id(buffer) result(receiver_id) BIND(C, name = 'DCB_get_receiver_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: receiver_id
// end function DCB_get_receiver_id
//F_EnD
int32_t DCB_get_receiver_id(const distributed_circular_buffer_p buffer) {
  return buffer->receiver_id;
}

//F_StArT
// function DCB_get_consumer_id(buffer) result(consumer_id) BIND(C, name = 'DCB_get_consumer_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: consumer_id
// end function DCB_get_consumer_id
//F_EnD
int32_t DCB_get_consumer_id(const distributed_circular_buffer_p buffer) {
  return buffer->consumer_id;
}

//F_StArT
//  function DCB_get_num_producers(buffer) result(num_producers) BIND(C, name = 'DCB_get_num_producers')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_producers
//  end function DCB_get_num_producers
//F_EnD
int32_t DCB_get_num_producers(const distributed_circular_buffer_p buffer) {
  return buffer->num_producers;
}

//F_StArT
// function DCB_start_receiving(buffer) result(return_value) BIND(C, name = 'DCB_start_receiving')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: return_value
// end function DCB_start_receiving
//F_EnD
//! Start the main loop of a receiver process.
//!
//! It's basically an infinite loop that keeps synchronizing the public/private MPI window so that the "passively"
//! received data is actually seen (yeah, that's not passive for real...)
//! It also checks at every iteration for a signal indicating that it needs to do something else, like returning, or
//! calling MPI_Barrier()
int32_t DCB_start_receiving(distributed_circular_buffer_p buffer) {
  if (!is_receiver(buffer))
    return -1;

  volatile receiver_signal_t* signal = get_receiver_signal_ptr(buffer, buffer->receiver_id);

  while (1) {
    sleep_us(WINDOW_SYNC_DELAY_US);
    MPI_Win_sync(buffer->window);

    switch (*signal) {
    case RSIG_SERVER_BARRIER:
      MPI_Barrier(buffer->server_communicator);
      *signal = RSIG_NONE;
      break;
    case RSIG_FULL_BARRIER:
      MPI_Barrier(buffer->communicator);
      *signal = RSIG_NONE;
      break;
    case RSIG_STOP: return 0;
    case RSIG_NONE: break;
    }
  }
}

//F_StArT
// subroutine DCB_full_barrier(buffer) BIND(C, name = 'DCB_full_barrier')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
// end subroutine DCB_full_barrier
//F_EnD
//! MPI barrier that applies to every process that participated in the creating of the given buffer.
//! It _must_ be called by every consumer and producer. Calling it from a receiver process has no effect (but your
//! receiver should be busy receiving, not doing barriers)
void DCB_full_barrier(distributed_circular_buffer_p buffer) {
  if (is_root(buffer))
    send_receiver_signal(buffer, RSIG_FULL_BARRIER);

  if (!is_receiver(buffer))
    MPI_Barrier(buffer->communicator);
}

//F_StArT
// subroutine DCB_server_barrier(buffer) BIND(C, name = 'DCB_server_barrier')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
// end subroutine DCB_server_barrier
//F_EnD
//! MPI barrier that only applies to processes located on the server. It must be called by every consumer. Calling it
//! from a producer or a receiver has no effect.
void DCB_server_barrier(distributed_circular_buffer_p buffer) {
  if (is_root(buffer))
    send_receiver_signal(buffer, RSIG_SERVER_BARRIER);

  if (is_consumer(buffer))
    MPI_Barrier(buffer->server_communicator);
}

//F_StArT
//  function DCB_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'DCB_put')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value           :: buffer
//    integer(DATA_ELEMENT), dimension(*), intent(in) :: src_data
//    integer(C_INT), intent(in), value        :: num_elements
//    integer(C_INT) :: num_available
//  end function DCB_put
//F_EnD
//! @brief Insert data into the given buffer, once there is enough space (will wait if there isn't enough initially)
//!
//! The data will be inserted into the buffer instance associated with the calling process.
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the root node (otherwise it might try to read data that has not yet arrived).
//!
//! @return How many elements can still fit after the insertion, if everything went smoothly, -1 otherwise
//C_StArT
data_index DCB_put(
    distributed_circular_buffer_p buffer,      //!< Distributed buffer in which we want to put data
    data_element* const           src_data,    //!< Pointer to the data we want to insert
    const int                     num_elements //!< How many #data_element tokens we want to insert
    )
//C_EnD
{
  if (DCB_wait_space_available(buffer, num_elements) < 0)
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

  buffer->producer_stats.num_elem += num_elements;
  buffer->producer_stats.num_transfers++;

  return CB_get_available_space(&buffer->local_header.buf);
}

//F_StArT
//  function DCB_get(buffer, buffer_id, dest_data, num_elements) result(num_available) BIND(C, name = 'DCB_get')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value              :: buffer
//    integer(C_INT), intent(in), value           :: buffer_id
//    integer(DATA_ELEMENT), dimension(*), intent(inout) :: dest_data
//    integer(C_INT), intent(in), value           :: num_elements
//    integer(C_INT) :: num_available
//  end function DCB_get
//F_EnD
int DCB_get(
    distributed_circular_buffer_p buffer,      //!<
    const int                     buffer_id,   //!<
    int32_t*                      dest_data,   //!<
    const int                     num_elements //!<
) {
  if (DCB_wait_data_available(buffer, buffer_id, num_elements) < 0)
    return -1;

  circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id);
  circular_buffer_p          queue    = get_circular_buffer(buffer, buffer_id);

  const int32_t in_index  = instance->buf.m.in;
  int32_t       out_index = instance->buf.m.out;
  const int32_t limit     = instance->buf.m.limit;

  const data_element* const buffer_data = instance->buf.data;

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

  buffer->consumer_stats[buffer_id].num_elem += num_elements;
  buffer->consumer_stats[buffer_id].num_transfers++;

  memory_fence(); // Make sure everything has been read, and the temp pointer actually updated

  volatile data_index *d_out = &instance->buf.m.out;
  *d_out = out_index; // Update actual extraction pointer

  return CB_get_available_data(queue);
}

/**
 * @brief Check the integrity of a single CB instance within the given DCB _(only valid for consumers/receivers)_
 * @return 0 if the check is successful, a negative value otherwise
 */
int DCB_check_instance_integrity(
    const distributed_circular_buffer_p buffer,   //!< [in] The DCB we want to check
    const int                           buffer_id //!< [in] The ID of the CB we want to check within the DCB
) {
  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id);
  if (CB_check_integrity(&instance->buf) != 0)
    return -1;

  return 0;
}

//F_StArT
//  function DCB_check_integrity(buffer, verbose) result(result) BIND(C, name = 'DCB_check_integrity')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value    :: buffer
//    integer(C_INT), intent(in), value :: verbose
//    integer(C_INT)                    :: result
//  end function DCB_check_integrity
//F_EnD
/**
 * @brief Check whether the given DCB is consistent. For producers, check the CB header. For consumers, check
 * every CB header.
 * @return 0 if everything is consistent, a negative number otherwise
 */
int DCB_check_integrity(
    const distributed_circular_buffer_p buffer, //!< [in] The buffer we want to check
    int                                 verbose //!< [in] Whether to display certain information in case of failure
) {
  if (buffer == NULL)
    return -1;

  if (is_producer(buffer)) {
    const int               total_num_elem = buffer->num_element_per_instance;
    const circular_buffer_p circ_buf       = &buffer->local_header.buf;

    if (total_num_elem != circ_buf->m.limit - 1) {
      if (verbose) {
        printf(
            "(rank %d) Limit seems to be wrong! It's %d, but it should be %d\n", buffer->rank, circ_buf->m.limit,
            total_num_elem);
      }
      return -1;
    }

    if (CB_check_integrity(circ_buf) != 0)
      return -1;
  }
  else if (is_consumer(buffer)) {
    for (int i = 0; i < buffer->num_producers; ++i) {
      if (DCB_check_instance_integrity(buffer, i) != 0)
        return -1;
    }
  }

  return 0;
}

//! @}
//F_StArT
//  end interface
//F_EnD
