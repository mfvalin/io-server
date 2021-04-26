/*
 * Copyright (C) 2021  Environnement et Changement climatique Canada
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

#include "io-server/memory_arena.h"
#include "io-server/timer.h"

static const MPI_Datatype CB_MPI_ELEMENT_TYPE = sizeof(data_element) == sizeof(int32_t)   ? MPI_INT
                                                : sizeof(data_element) == sizeof(int64_t) ? MPI_LONG_LONG_INT
                                                                                          : MPI_DATATYPE_NULL;

//! How long to wait between checks for free space in a buffer (microseconds)
static const int DCB_SPACE_CHECK_DELAY_US = 100;
//! How long to wait between checks for data in a buffer (microseconds)
static const int DCB_DATA_CHECK_DELAY_US = 20;
//! How long to wait between each window sync from a channel process
static const int DCB_WINDOW_SYNC_DELAY_US = 10;

//! ID of the server process that is considered the root of a DCB
static const int DCB_ROOT_ID = 0;

//C_StArT
/**
 * @brief Wrapper struct around a regular circular buffer. It adds some information for management within a set of
 * distributed circular buffers.
 *
 * Please don't change the order of data members (unless you know what the consequences are).
 */
typedef struct {
  int             target_rank; //!< With which process this instance should communicate for data transfers
  int             id;
  int64_t         capacity;    //!< How many elements can fit in this instance
  void*           dummy;       //!< Force 64-bit alignment of the rest of the struct
  circular_buffer circ_buffer; //!< The buffer contained in this instance
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
 * side, each channel process is constantly polling for updates by synchronizing the window.
 * _The channel processes must be located on the same physical node as the consumers._
 */
typedef struct {
  int32_t      num_producers; //!< How many producer processes share this distributed buffer set
  int32_t      num_channels;  //!< How many channels can be used for MPI 1-sided communication (1 PE per channel)
  int32_t      num_consumers; //!< How many server processes will read from the individual buffers
  data_element window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located

  int32_t channel_id;
  int32_t consumer_id;
  int32_t producer_id;

  int32_t server_rank;

  MPI_Comm communicator; //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win  window;       //!< MPI window into the circular buffers themselves, on the process which holds all data
  MPI_Comm server_communicator; //!< Communicator that groups processes located on the IO server

  io_timer_t existence_timer; //!< To keep track of how long ago the buffer was created

  //! Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  //! Will have some metadata at the beginning
  data_element* raw_data;

  //! Header of the circular buffer instance (only valid for producers)
  //! This is the local copy and will be synchronized with the remote one, located in the shared memory region of the
  //! consumer processes
  circular_buffer_instance local_header;

} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;
//C_EnD

enum channel_signal
{
  RSIG_NONE = 0,
  RSIG_SERVER_BARRIER,
  RSIG_FULL_BARRIER,
  RSIG_STOP,
};

typedef enum channel_signal channel_signal_t;

/**
 * @brief Small header located at the beginning of the shared memory region on server processes. Stores the offset
 * of every instance within that shared memory region, the ranks of producer and channel processes on the DCB
 * communicator and locations that can be used to send signals to channel processes.
 * It will be stored in the remotely-accessible MPI window, so everything in it must have a size that is a multiple of
 * the size of an element to be addressable individually. So basically, just data_elements
 */
typedef struct {
  data_element num_offsets; //!< How many offsets there are ( = number of producer processes)
  data_element size;        //!< Size in number of #data_element
  data_element num_signals; //!< How many communication channels there are in the buffer set (1 signal per channel)
  data_element data[];      //!< The data available to everyone. Organized as follows:
  // First, the window offsets
  // After the offsets, there are the producer ranks, but we can't name them explicitly. We really need a better solution
  // After the producer ranks, there are the channel ranks.
  // After the ranks, there are the signals. This is starting to get messy
} offset_header;

typedef offset_header* offset_header_p;

//F_StArT
//  interface
//F_EnD

// Forward declarations
static inline void print_instance(const circular_buffer_instance_p instance);

//! @{ @name Various size calculators

//! Compute the number of #data_element taken by the given number of bytes (rounded up)
static inline data_element num_elem_from_bytes(const size_t num_bytes) {
  return num_bytes / sizeof(data_element) + (num_bytes % sizeof(data_element) > 0);
}

//! Compute the _even_ number of #data_element needed to contain the given number of bytes. This guarantees that the
//! final amount will have a size of a multiple of 64 bits (since we use either 32- or 64-bit elements)
static inline data_element num_elem_from_bytes_aligned64(const size_t num_bytes) {
  const data_element num_elem_init = num_elem_from_bytes(num_bytes);
  return num_elem_init % 2 == 0 ? num_elem_init : num_elem_init + 1;
}

static inline data_element circular_buffer_header_size() {
  return num_elem_from_bytes_aligned64(sizeof(circular_buffer));
}

//! Compute the number of #data_element taken by the circular_buffer_instance struct
static inline data_element instance_header_size() {
  return num_elem_from_bytes_aligned64(sizeof(circular_buffer_instance));
}

//! Size of the offset header, in number of #data_element tokens
static inline data_element offset_header_size(
    const int num_buffers, //!< Number of circular buffers in the set
    const int num_channels //!< Number of communication channels (PEs) used for MPI 1-sided calls
) {
  const size_t num_bytes = sizeof(offset_header) +                          // base struct size
                           (size_t)num_buffers * sizeof(data_element) +     // offsets size
                           (size_t)num_buffers * sizeof(data_element) +     // producer ranks size
                           (size_t)num_channels * sizeof(data_element) +    // channel (ghost process) ranks size
                           (size_t)num_channels * sizeof(channel_signal_t); // signals size
  return num_elem_from_bytes_aligned64(num_bytes);
}

//! Size of an entire circular buffer instance, based on the number of
static inline data_element total_circular_buffer_instance_size(const int num_desired_elem) {
  // Add 2 instead of 1 so that all instances are aligned to 64 bits
  return instance_header_size() + num_desired_elem + 2;
}

//! Compute the total size needed by the shared memory window to fit all circular buffers and metadata, in number of
//! #data_element tokens
static inline data_element total_window_num_elem(
    const int num_buffers,                //!< How many circular buffers the set will hold
    const int num_channels,               //!< How many communication channels with the IO server will be used
    const int num_desired_elem_per_buffer //!< How many #data_element tokens we want to be able to store in each buffer
) {
  return num_buffers * (total_circular_buffer_instance_size(num_desired_elem_per_buffer)) +
         offset_header_size(num_buffers, num_channels);
}

//! @}

//! @{ \name Circular buffer instance management

static inline data_element get_available_space(
    const circular_buffer_instance_p buffer //!< [in] The buffer instance we want to query
) {
  return CB_get_available_space(&buffer->circ_buffer);
}

static inline data_element get_available_data(
    const circular_buffer_instance_p buffer //!< [in] The buffer instance we want to query
) {
  return CB_get_available_data(&buffer->circ_buffer);
}

static inline int check_instance_consistency(const circular_buffer_instance_p instance) {
  if (instance == NULL) {
    // printf("Invalid b/c NULL pointer\n");
    return -1;
  }

  if (instance->capacity != CB_get_capacity(&instance->circ_buffer))
    return -1;

  if (instance->target_rank < 0)
    return -1;

  if (instance->id < 0)
    return -1;

  if (CB_check_integrity(&instance->circ_buffer) < 0)
    return -1;

  return 0;
}

//! @}

//! @{ \name Shared server data management

//! Get a pointer to the list of window offsets, from a pointer to the header
static inline data_element* get_offsets_pointer_from_offset_header(offset_header_p header) {
  return header->data;
}

//! Get a pointer to the list of producer ranks, from a pointer to the header and the number of buffers in the DCB
static inline data_element* get_prod_ranks_pointer_from_offset_header(
    offset_header_p header,     //!< [in] The header from which we want the producer rank list
    const int       num_buffers //!< [in] How many buffers there are in the DCB that owns the given header
) {
  return get_offsets_pointer_from_offset_header(header) + num_buffers;
}

//! Get a pointer to the list of channel ranks, from a pointer to the header and the number of buffers in the DCB
static inline data_element* get_channel_ranks_pointer_from_offset_header(
    offset_header_p header,     //!< [in] The header from which we want the channel rank list
    const int       num_buffers //!< [in] How many buffers there are in the DCB that owns the given header
) {
  return get_prod_ranks_pointer_from_offset_header(header, num_buffers) + num_buffers;
}

//! Get a pointer to the list of channel signals, from a pointer to the header and the number of buffers and
//! channels in the DCB
static inline channel_signal_t* get_signals_pointer_from_offset_header(
    offset_header_p header,      //!< [in] The header from which we want the channel signal list
    const int       num_buffers, //!< [in] The number of buffers in the DCB that owns the given header
    const int       num_channels //!< [in] The number of channels in the DCB that owns the given header
) {
  return (channel_signal_t*)(get_channel_ranks_pointer_from_offset_header(header, num_buffers) + num_channels);
}

//! Get a pointer to the offset_header of the given DCB
static inline offset_header_p get_offset_header(
    distributed_circular_buffer_p buffer_set //!< Buffer set from which we want the offset_header
) {
  return (offset_header_p)(buffer_set->raw_data);
}

//! Get a pointer to the list of channel signals of the given DCB
static inline channel_signal_t* get_signals_pointer(distributed_circular_buffer_p buffer) {
  return get_signals_pointer_from_offset_header(get_offset_header(buffer), buffer->num_producers, buffer->num_channels);
}

//! Get a pointer to the list of producer ranks of the given DCB
static inline data_element* get_producer_ranks(distributed_circular_buffer_p buffer) {
  return get_prod_ranks_pointer_from_offset_header(get_offset_header(buffer), buffer->num_producers);
}

//! Get a pointer to the list of channel ranks of the given DCB
static inline data_element* get_channel_ranks(distributed_circular_buffer_p buffer) {
  return get_channel_ranks_pointer_from_offset_header(get_offset_header(buffer), buffer->num_producers);
}

//! Get a pointer to the signal location of the given channel process (ID, not rank)
static inline channel_signal_t* get_channel_signal_ptr(
    distributed_circular_buffer_p buffer,    //!< [in] Buffer whose channel signal we want
    const int32_t                 channel_id //!< [in] ID of the channel we're looking for
) {
  return get_signals_pointer(buffer) + channel_id;
}

//! Initialize the offset_header of the given distributed_circular_buffer. This computes the offset of every buffer
//! instance within the set, sets the value of all ranks to -1 and all signals to RSIG_NONE.
static inline void init_offset_header(
    offset_header_p header,                   //!< [in] Pointer to the header that needs to be initialized
    const int       num_buffers,              //!< [in] How many circular buffer instances there are in the set
    const int       num_channels,             //!< [in] How many channels can be used to transmit data
    const int       num_elements_per_instance //!< [in] How many elements each instance takes
) {
  header->num_offsets = num_buffers;
  header->size        = offset_header_size(num_buffers, num_channels);
  header->num_signals = num_channels;

  data_element* window_offsets = get_offsets_pointer_from_offset_header(header);
  for (int i = 0; i < header->num_offsets; ++i) {
    window_offsets[i] = header->size + i * (total_circular_buffer_instance_size(num_elements_per_instance));
  }

  data_element* producer_ranks = get_prod_ranks_pointer_from_offset_header(header, num_buffers);
  for (int i = 0; i < num_buffers; ++i) {
    producer_ranks[i] = -1;
  }

  data_element* channel_ranks = get_channel_ranks_pointer_from_offset_header(header, num_buffers);
  for (int i = 0; i < num_channels; ++i) {
    channel_ranks[i] = -1;
  }

  channel_signal_t* signals = get_signals_pointer_from_offset_header(header, num_buffers, num_channels);
  for (int i = 0; i < num_channels; ++i) {
    signals[i] = RSIG_NONE;
  }
}

//! Print the contents of the given header
static inline void print_offset_header(offset_header_p header) {
  printf(
      "Num buffers %d, size %d elements, num channels %d\n"
      "Address %ld",
      header->num_offsets, header->size, header->num_signals, (long)header);
  printf("\nOffsets:        ");
  const data_element* offsets = get_offsets_pointer_from_offset_header(header);
  for (int i = 0; i < header->num_offsets; ++i) {
    printf("%ld ", (long)offsets[i]);
  }
  printf("\nProducer ranks: ");
  const data_element* ranks = get_prod_ranks_pointer_from_offset_header(header, header->num_offsets);
  for (int i = 0; i < header->num_offsets; ++i) {
    printf("%ld ", (long)ranks[i]);
  }
  printf("\nChannel ranks:  ");
  const data_element* channel_ranks = get_channel_ranks_pointer_from_offset_header(header, header->num_offsets);
  for (int i = 0; i < header->num_signals; ++i) {
    printf("%ld ", (long)channel_ranks[i]);
  }
  printf("\nSignals:        ");
  channel_signal_t* signals = get_signals_pointer_from_offset_header(header, header->num_offsets, header->num_signals);
  for (int i = 0; i < header->num_signals; ++i) {
    printf("%ld ", (long)signals[i]);
  }
  printf("\n");
}

//! @}

//! @{ @name Offset calculators

//! Compute the displacement in the shared memory window where the window offset for the specified rank (producer ID) is
//! located. That displacement should land in the appropriate location in the offset_header struct, which is itself at
//! the start of the window
//! @return The window displacement where a producer can find the offset (displacement) of its buffer within that window
static inline MPI_Aint window_offset_displacement(
    const int producer_id //!< [in] ID of the producer for which we want the window offset location (= its rank)
) {
  data_element* ptr = get_offsets_pointer_from_offset_header(NULL) + producer_id;
  return (MPI_Aint)ptr / sizeof(data_element);
}

//! Compute the displacement in the shared memory window where the MPI rank of the given producer ID is located.
static inline MPI_Aint producer_rank_displacement(
    const int producer_id,  //!< [in] ID of the producer whose rank we seek
    const int num_producers //!< [in] How many producers there are in the DCB
) {
  // We find the displacement by taking a (fake) pointer to the rank, assuming the address of the header is NULL
  // and by dividing that pointer by the size of a window element
  data_element* ptr = get_prod_ranks_pointer_from_offset_header(NULL, num_producers) + producer_id;
  return (MPI_Aint)ptr / sizeof(data_element);
}

//! @return The displacement in the shared memory window where the given element index for a circular buffer is located
static inline MPI_Aint buffer_element_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer in which the element is located
    const data_element                  index   //!< [in] Index of the element within the buffer
) {
  // Start of the buffer element section within the window
  const ptrdiff_t ptr_offset =
      (data_element*)buffer->local_header.circ_buffer.data - (data_element*)&buffer->local_header;
  const MPI_Aint base_displacement = buffer->window_offset + ptr_offset;
  const MPI_Aint elem_displacement = base_displacement + index;
  return elem_displacement;
}

//! @return The displacement in the shared memory window where the insertion index of the given buffer is located
static inline MPI_Aint insertion_index_displacement(const distributed_circular_buffer_p buffer) {
  const ptrdiff_t ptr_offset =
      (data_element*)buffer->local_header.circ_buffer.m.in - (data_element*)&buffer->local_header;
  const data_element displacement = buffer->window_offset + ptr_offset;
  return displacement;
}

//! Get the displacement (in number of #data_element) in the shared memory window where the start of the buffer is
//! located. Must be called from a producer process
static inline MPI_Aint remote_header_displacement(const distributed_circular_buffer_p buffer) {
  return buffer->window_offset;
}

//! @}

//! @{ @name Helper functions

//! Check whether the given distributed buffer is located on a consumer process
static inline int is_consumer(const distributed_circular_buffer_p buffer) {
  return (buffer->consumer_id >= 0);
}

//! Check whether the given distributed buffer is located on a producer process
static inline int is_producer(const distributed_circular_buffer_p buffer) {
  return (buffer->producer_id >= 0);
}

//! Check whether the given distributed buffer is located on the DCB root process
static inline int is_root(const distributed_circular_buffer_p buffer) {
  return (buffer->server_rank == DCB_ROOT_ID);
}

//! Check whether the given distributed buffer is located on a channel process
static inline int is_channel(const distributed_circular_buffer_p buffer) {
  return (buffer->channel_id >= 0);
}

static inline int is_on_server(const distributed_circular_buffer_p buffer) {
  return (buffer->server_rank >= 0);
}

//! Set the process rank to be used for MPI communication for the given CB instance
static inline void assign_target_channel(
    const circular_buffer_instance_p    instance,   //!< [in, out] Buffer instance whose target we want to set
    const int                           channel_id, //!< [in] Which channel this instance should use
    const distributed_circular_buffer_p buffer      //!< [in] DCB to which this instance belongs
) {
  instance->target_rank = get_channel_ranks(buffer)[channel_id];
}

//! Get a pointer to a certain circular_buffer_instance within the given distributed_circular_buffer
static inline circular_buffer_instance_p get_circular_buffer_instance(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want the circ buffer instance
    const int                     buffer_id   //!< [in] ID of the circ buffer instance within the buffer set
) {
  const data_element* offsets = get_offsets_pointer_from_offset_header(get_offset_header(buffer_set));
  return (circular_buffer_instance_p)(buffer_set->raw_data + offsets[buffer_id]);
}

//! Retrieve the window offset of the appropriate circular buffer instance within the given distributed buffer
//! (according to its rank)
static void retrieve_window_offset_from_remote(
    distributed_circular_buffer_p buffer //!< Buffer for which we want the window offset
) {
  // We communicate through the root process, because we don't yet know the rank of our target
  MPI_Win_lock(MPI_LOCK_SHARED, DCB_ROOT_ID, MPI_MODE_NOCHECK, buffer->window);
  MPI_Get(
      &buffer->window_offset, 1, CB_MPI_ELEMENT_TYPE, DCB_ROOT_ID, window_offset_displacement(buffer->producer_id), 1,
      CB_MPI_ELEMENT_TYPE, buffer->window);
  MPI_Win_unlock(DCB_ROOT_ID, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance
static inline void update_local_header_from_remote(
    distributed_circular_buffer_p buffer, //!< [in] Buffer set from which we want to update a single instance
    const int                     full    //!< [in] Whether to perform a full update (if =1) or only a partial one
) {
  // Gotta load the target rank first, because it's going to be overwritten by the MPI_Get
  const int      target_rank  = buffer->local_header.target_rank >= 0 ? buffer->local_header.target_rank : DCB_ROOT_ID;
  const int      num_elem     = instance_header_size();
  const MPI_Aint displacement = remote_header_displacement(buffer);

  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);
  circular_buffer_instance header_copy;
  MPI_Get(
      &header_copy, num_elem, CB_MPI_ELEMENT_TYPE, target_rank, displacement, num_elem, CB_MPI_ELEMENT_TYPE,
      buffer->window);
  MPI_Win_unlock(target_rank, buffer->window);

  if (full == 1) // Keep everything
  {
    memcpy(&buffer->local_header, &header_copy, sizeof(header_copy));
  }
  else // Only take the necessary values
  {
    buffer->local_header.target_rank                   = header_copy.target_rank;
    buffer->local_header.circ_buffer.m.out[CB_FULL]    = header_copy.circ_buffer.m.out[CB_FULL];
    buffer->local_header.circ_buffer.m.out[CB_PARTIAL] = header_copy.circ_buffer.m.out[CB_PARTIAL];
  }
}

//! Set the value of this process' rank in the server shared data area, at the right location in the MPI window.
//! (We use the rank on the global DCB communicator)
static void set_producer_rank_on_remote(
    distributed_circular_buffer_p buffer,      //!< [in,out] Buffer we want to update
    const int                     producer_id, //!< [in] ID of the calling producer within the buffer
    const int producer_rank //!< [in] MPI rank that corresponds to the calling producer on the buffer communicator
) {
  const int          target_rank         = 0; // Target the root of the DCB, which we know to be on the server
  const int          num_elem            = 1;
  const data_element value               = producer_rank;
  const MPI_Aint     target_displacement = producer_rank_displacement(producer_id, buffer->num_producers);

  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);
  MPI_Put(
      &value, num_elem, CB_MPI_ELEMENT_TYPE, target_rank, target_displacement, num_elem, CB_MPI_ELEMENT_TYPE,
      buffer->window);
  MPI_Win_unlock(target_rank, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance and compute how much
//! space is available
//! @return The number of #data_element tokens that can still be stored in the buffer
static inline data_element get_available_space_from_remote(
    const distributed_circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  update_local_header_from_remote(buffer, 0);
  return get_available_space(&buffer->local_header);
}

//! @brief Stop and wait until there is enough space in this circular buffer instance.
//!
//! If the latest copy of the header shows enough space, return immediately. Otherwise, copy metadata from the consumer
//! process and check, until there is enough space.
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated. If there is an error, returns -1.
static data_element DCB_wait_space_available(
    distributed_circular_buffer_p buffer,       //!< [in] Pointer to the distributed buffer we're waiting for
    const int                     num_requested //!< [in] Needed number of available #data_element slots
) {
  if (buffer == NULL || !is_producer(buffer))
    return -1;

  const circular_buffer_instance_p instance = &buffer->local_header;
  if (num_requested < 0 || num_requested > CB_get_capacity(&instance->circ_buffer) ||
      check_instance_consistency(instance) < 0)
    return -1;

  // First check locally
  data_element num_available = get_available_space(instance);
  if (num_available >= num_requested)
    return num_available;

  int num_waits = 0;
  // Then get info from remote location, until there is enough
  while ((void)(num_available = get_available_space_from_remote(buffer)), num_available < num_requested) {
    num_waits++;
    sleep_us(DCB_SPACE_CHECK_DELAY_US);
  }

  buffer->local_header.circ_buffer.stats.total_write_wait_time_ms += num_waits * DCB_SPACE_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//! Stop and wait until there is enough data in this circular buffer instance
//!
//! @return The number of data elements in the buffer, if everything goes smoothly, -1 otherwise.
static data_element DCB_wait_data_available(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer we are querying
    const int buffer_id,    //!< [in] Which specific circular buffer we want to query (there are multiple ones)
    const int num_requested //!< [in] Number of elements we want to read
) {
  if (buffer == NULL || is_producer(buffer))
    return -1;

  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id);
  if (num_requested < 0 || num_requested > CB_get_capacity(&instance->circ_buffer) ||
      check_instance_consistency(instance) < 0)
    return -1;

  // Only check locally, waiting a bit between each check
  data_element num_available = 0;
  int          num_waits     = 0;
  while ((void)(num_available = get_available_data(instance)), num_available < num_requested) {
    num_waits++;
    sleep_us(DCB_DATA_CHECK_DELAY_US);
  }

  instance->circ_buffer.stats.total_read_wait_time_ms += num_waits * DCB_DATA_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//! Initialize the given circular buffer instance, including the circular_buffer it contains
static inline int init_circular_buffer_instance(
    circular_buffer_instance_p instance, //!< Buffer instance we want to init
    const int                  id,       //!< ID of the buffer instance
    const int                  num_elem  //!< How many #data_element are taken by the circular buffer
) {
  instance->target_rank = -1;
  instance->id          = id;
  instance->dummy       = NULL;
  instance->capacity    = -1;

  if (CB_init(&instance->circ_buffer, num_elem) == NULL)
    return -1;

  instance->capacity = CB_get_capacity(&instance->circ_buffer);

  return 0;
}

//! Print the collected stats for a single buffer instance
static void print_instance_stats(
    circular_buffer_instance_p instance,
    const int                  with_header //!< Whether to print a header to name the columns
) {
  CB_print_stats(&instance->circ_buffer, instance->id, with_header);
}

static inline void print_instance(const circular_buffer_instance_p instance) {
  printf(
      "Num elems: %ld, num spaces: %ld\n"
      "Target rank: %ld\n"
      "Capacity %ld\n",
      (int64_t)get_available_data(instance), (int64_t)get_available_space(instance), (int64_t)instance->target_rank,
      instance->capacity);
  CB_print_header(&instance->circ_buffer);
  for (uint64_t i = 0; i < sizeof(circular_buffer_instance); ++i) {
    if (i % 8 == 0)
      printf("\n");
    printf("%03d ", ((unsigned char*)instance)[i]);
  }
  printf("\n");
}

//! Set the variable at a given address to a certain value, atomically, as soon as the old value is RSIG_NONE
static inline void set_channel_signal(channel_signal_t* signal_ptr, const channel_signal_t value) {
  while (__sync_val_compare_and_swap(signal_ptr, RSIG_NONE, value) != 0)
    ;
}

//! Set the signal of every channel associated with a DCB to the given value.
//! Should only be called from a single process at a time, located on the IO server (so basically a consumer process,
//! because the channels are normally busy)
static inline void send_channel_signal(distributed_circular_buffer_p buffer, const channel_signal_t value) {
  for (int i = 0; i < buffer->num_channels; ++i)
    set_channel_signal(get_channel_signal_ptr(buffer, i), value);
}

//! @}

//! @{ \name Distributed circular buffer public interface

// Forward declarations
//C_StArT
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p);
void DCB_full_barrier(distributed_circular_buffer_p buffer);
int  DCB_check_integrity(const distributed_circular_buffer_p buffer, int verbose);
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
//! _The buffer with rank 0 on the given DCB communicator will be considered the root of the DCB and must be located on
//! the server node._
//!
//! @return A pointer to a newly-allocated distributed circular buffer struct that contains all the relevant info
//C_StArT
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,        //!< [in] Communicator on which the distributed buffer is shared
    MPI_Comm      server_communicator, //!< [in] Communicator that groups server processes
    const int32_t num_producers, //!< [in] Number of producer processes in the communicator (number of buffer instances)
    const int32_t num_channels,  //!< [in] Number of processes that can be the target of MPI 1-sided comm (channels)
    const int32_t num_elements   //!< [in] Number of elems in a single circular buffer (only needed on the root process)
    )
//C_EnD
{
  distributed_circular_buffer_p buffer = (distributed_circular_buffer*)malloc(sizeof(distributed_circular_buffer));

  buffer->communicator             = communicator;
  buffer->server_communicator      = server_communicator;
  buffer->num_producers            = num_producers;
  buffer->num_channels             = num_channels;
  buffer->local_header.target_rank = -1;

  buffer->channel_id  = -1;
  buffer->consumer_id = -1;
  buffer->producer_id = -1;
  buffer->server_rank = -1;

  buffer->existence_timer.start      = 0;
  buffer->existence_timer.total_time = 0.0;

  int dcb_rank;
  MPI_Comm_rank(buffer->communicator, &dcb_rank);

  // Determine which process will be the root of the DCB
  if (buffer->server_communicator != MPI_COMM_NULL) {
    MPI_Comm_rank(buffer->server_communicator, &buffer->server_rank);
    if (buffer->server_rank == DCB_ROOT_ID) {
      if (dcb_rank != DCB_ROOT_ID) {
        printf("ERROR during DCB_create. DCB rank %d is not on the server communicator...\n", DCB_ROOT_ID);
        return NULL;
      }

      int total_num_procs;
      MPI_Comm_size(buffer->communicator, &total_num_procs);
      buffer->num_consumers = total_num_procs - buffer->num_producers - buffer->num_channels;
    }
  }

  // We assume the rank 0 process is the root and contains the relevant info
  // Now make sure the other processes have the same info
  MPI_Bcast(&buffer->num_producers, 1, MPI_INT, DCB_ROOT_ID, buffer->communicator);
  MPI_Bcast(&buffer->num_channels, 1, MPI_INT, DCB_ROOT_ID, buffer->communicator);
  MPI_Bcast(&buffer->num_consumers, 1, MPI_INT, DCB_ROOT_ID, buffer->communicator);

  // Assign a "buffer rank" to every producer process. It will be their producer ID
  if (server_communicator == MPI_COMM_NULL) {
    MPI_Comm new_comm;
    MPI_Comm_split(buffer->communicator, 1, 0, &new_comm);
    MPI_Comm_rank(new_comm, &buffer->producer_id);
  }
  else {
    MPI_Comm dummy;
    MPI_Comm_split(buffer->communicator, 0, dcb_rank, &dummy);

    if (buffer->server_rank < 0) {
      printf("WE HAVE A PROBLEM!!!\n");
      return NULL;
    }

    if (buffer->server_rank < buffer->num_consumers)
      buffer->consumer_id = buffer->server_rank;
    else
      buffer->channel_id = buffer->server_rank - buffer->num_consumers;
  }

  const MPI_Aint win_total_size =
      total_window_num_elem(buffer->num_producers, buffer->num_channels, num_elements) * (MPI_Aint)sizeof(data_element);
  const MPI_Aint shared_win_size = is_root(buffer) ? win_total_size : 0; // Size used for shared memory allocation

  // Root only: allocate shared DCB memory on the server, initialize the common header and send memory info to other server processes
  if (is_root(buffer)) {
    int id = -1;
    buffer->raw_data = memory_allocate_shared(&id, shared_win_size);

    if (buffer->raw_data == NULL)
    {
      printf("Error when allocating shared memory for DCB\n");
      return NULL;
    }

    init_offset_header(get_offset_header(buffer), buffer->num_producers, buffer->num_channels, num_elements);

    MPI_Bcast(&id, 1, MPI_INT, DCB_ROOT_ID, buffer->server_communicator);
  }

  // Non-root server processes: retrieve shared memory info and set own rank (channels only) into common header
  if (is_on_server(buffer) && !is_root(buffer))
  {
    int id;
    MPI_Bcast(&id, 1, MPI_INT, DCB_ROOT_ID, buffer->server_communicator);
    buffer->raw_data = memory_address_from_id(id);

    // Set channel ranks in common header
    if (is_channel(buffer)) {
      int full_rank;
      MPI_Comm_rank(buffer->communicator, &full_rank);
      get_channel_ranks(buffer)[buffer->channel_id] = full_rank;
    }

    // Let the root know it can initialize the buffers
    MPI_Barrier(buffer->server_communicator);
  }

  // Root only: initialize the individual CB instances
  if (is_root(buffer)) {
    // Wait until everyone on the server has retrieved the shared mem info and the channels have set their rank
    MPI_Barrier(buffer->server_communicator);

    // Compute number of elements that fit in individual buffers (excludes the space taken by some headers)
    const int num_elem_in_circ_buffer =
        total_circular_buffer_instance_size(num_elements) - instance_header_size() + circular_buffer_header_size();

    // Initialize the individual buffers
    for (int i = 0; i < num_producers; i++) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i);
      init_circular_buffer_instance(buffer_instance, i, num_elem_in_circ_buffer);
      assign_target_channel(buffer_instance, i % buffer->num_channels, buffer);
    }
  }

  // Create the actual window that will be used for data transmission, using a pointer to the same shared memory on the server
  const MPI_Aint final_win_size = is_producer(buffer) ? 0 : win_total_size; // Size used for actual window creation
  MPI_Win_create(
      buffer->raw_data, final_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->communicator, &buffer->window);

  // Producers: retrieve their assigned initialized CB instance and send their rank to the server
  if (is_producer(buffer)) {
    retrieve_window_offset_from_remote(buffer); // Find out where in the window this instance is located
    update_local_header_from_remote(buffer, 1); // Get the header to sync the instance locally

    int producer_rank;
    MPI_Comm_rank(buffer->communicator, &producer_rank);
    set_producer_rank_on_remote(buffer, buffer->producer_id, producer_rank);
  }

  // Wait until everyone (especially the producers) is properly initialized before allowing the use of the buffer
  MPI_Barrier(buffer->communicator);

  if (is_root(buffer))
    print_offset_header(get_offset_header(buffer));

  if (DCB_check_integrity(buffer, 1) < 0) {
    printf("AAAHHHh just-created DCB is not consistent!!!!\n");
    return NULL;
  }

  io_timer_start(&buffer->existence_timer);

  return buffer;
}

//F_StArT
//  function DCB_create(f_communicator, f_server_communicator, num_producers, num_channels, num_elements) result(p) BIND(C, name = 'DCB_create_f')
//    import :: C_PTR, C_INT
//    implicit none
//    integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
//    integer(C_INT), intent(IN), value :: f_server_communicator !< Communicator that groups the server processes
//    integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
//    integer(C_INT), intent(IN), value :: num_channels   !< Number of channels (PEs used for communication only)
//    integer(C_INT), intent(IN), value :: num_elements   !< Number of desired #data_element in the circular buffer
//    type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
//   end function DCB_create
//F_EnD
//! Wrapper function to call from Fortran code, with a Fortran MPI communicator
distributed_circular_buffer_p DCB_create_f(
    int32_t f_communicator,        //!< [in] Communicator on which the distributed buffer is shared (in Fortran)
    int32_t f_server_communicator, //!< [in] Communicator that groups server processes (in Fortran)
    int32_t num_producers,         //!< [in] Number or producers (circular buffer instances)
    int32_t num_channels,          //!< [in] Number of processes that can be the target of MPI 1-sided comm (channels)
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
      "Printing distributed circ buf: num producers %d, num channels %d, window offset = %d\n"
      "Consumer ID: %d\n"
      "Producer ID: %d\n"
      "Channel ID:  %d\n"
      "Server rank: %d\n"
      "Current time: %.2f ms\n",
      buffer->num_producers, buffer->num_channels, buffer->window_offset, buffer->consumer_id, buffer->producer_id,
      buffer->channel_id, buffer->server_rank, io_time_since_start(&buffer->existence_timer));
  if (is_producer(buffer)) {
    print_instance(&buffer->local_header);
  }
  else if (is_root(buffer)) {
    for (int i = 0; i < buffer->num_producers; ++i) {
      const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, i);
      printf("From root: buffer %d has %ld data in it\n", i, (int64_t)get_available_data(instance));
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

  io_timer_stop(&buffer->existence_timer);

  if (is_producer(buffer)) {
    MPI_Send(
        &buffer->local_header.circ_buffer.stats, sizeof(cb_stats), MPI_BYTE, DCB_ROOT_ID, buffer->producer_id,
        buffer->communicator);
  }

  if (is_root(buffer)) {
    send_channel_signal(buffer, RSIG_STOP); // Tell channels to stop any activity
    const data_element* producer_ranks     = get_producer_ranks(buffer);
    uint64_t            total_data_written = 0;
    printf(
        "------------------------------------------------------------------------------------\n"
        "DCB STATS\n"
        "Num server processes: %d\n"
        "Num consumers: %d\n"
        "Num channels: %d\n"
        "Num producers (=buffers): %d\n",
        buffer->num_channels + buffer->num_consumers, buffer->num_consumers, buffer->num_channels,
        buffer->num_producers);
    for (int i = 0; i < buffer->num_producers; ++i) {
      const circular_buffer_instance_p instance   = get_circular_buffer_instance(buffer, i);
      cb_stats_p                       full_stats = &instance->circ_buffer.stats;

      // Combine the stats from remote and server
      MPI_Status status;
      cb_stats   remote_stats;
      MPI_Recv(&remote_stats, sizeof(cb_stats), MPI_BYTE, producer_ranks[i], i, buffer->communicator, &status);
      full_stats->num_writes               = remote_stats.num_writes;
      full_stats->num_write_elems          = remote_stats.num_write_elems;
      full_stats->total_write_time_ms      = remote_stats.total_write_time_ms;
      full_stats->total_write_wait_time_ms = remote_stats.total_write_wait_time_ms;

      print_instance_stats(instance, i == 0);

      total_data_written += remote_stats.num_write_elems;
    }

    const double existence_time = io_time_ms(&buffer->existence_timer);
    char         total_data_s[8], dps_s[8];
    readable_element_count((double)total_data_written * sizeof(data_element), total_data_s);
    readable_element_count(total_data_written / existence_time * 1000.0 * sizeof(data_element), dps_s);
    printf("Total data written: %sB (%sB/s)\n", total_data_s, dps_s);
    printf("------------------------------------------------------------------------------------\n");
  }

  MPI_Win_free(&buffer->window);
  free(buffer);
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
    distributed_circular_buffer_p buffer,   //!< [in] DCB we are querying
    const int                     buffer_id //!< [in] Which specific buffer in the DCB
    )
//C_EnD
{
  if (is_consumer(buffer))
    return (int32_t)get_available_data(get_circular_buffer_instance(buffer, buffer_id));
  return -1;
}

//F_StArT
//  function DCB_get_num_spaces(buffer, update_from_remote) result(num_spaces) BIND(C, name = 'DCB_get_num_spaces')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT), intent(in), value :: update_from_remote
//    integer(C_INT) :: num_spaces
//  end function DCB_get_num_spaces
//F_EnD
//C_StArT
int32_t DCB_get_num_spaces(
    distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    int update_from_remote                //!< [in] Whether to look at the server to get the absolute latest num spaces
    )
//C_EnD
{
  if (is_producer(buffer)) {
    if (update_from_remote == 1)
      return get_available_space_from_remote(buffer);
    else
      return get_available_space(&buffer->local_header);
  }
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
  return buffer->producer_id;
}

//F_StArT
// function DCB_get_channel_id(buffer) result(channel_id) BIND(C, name = 'DCB_get_channel_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: channel_id
// end function DCB_get_channel_id
//F_EnD
int32_t DCB_get_channel_id(const distributed_circular_buffer_p buffer) {
  return buffer->channel_id;
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
//  function DCB_get_num_consumers(buffer) result(num_consumers) BIND(C, name = 'DCB_get_num_consumers')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_consumers
//  end function DCB_get_num_consumers
//F_EnD
int32_t DCB_get_num_consumers(const distributed_circular_buffer_p buffer) {
  return buffer->num_consumers;
}

//F_StArT
//  function DCB_get_capacity_local(buffer) result(capacity) BIND(C, name = 'DCB_get_capacity_local')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: capacity
//  end function DCB_get_capacity_local
//F_EnD
int32_t DCB_get_capacity_local(const distributed_circular_buffer_p buffer) {
  if (is_producer(buffer))
    return buffer->local_header.capacity;
  return -1;
}

//F_StArT
//  function DCB_get_capacity_server(buffer, buffer_id) result(capacity) BIND(C, name = 'DCB_get_capacity_server')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR),    intent(in), value :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT) :: capacity
//  end function DCB_get_capacity_server
//F_EnD
int32_t DCB_get_capacity_server(const distributed_circular_buffer_p buffer, int buffer_id) {
  if (is_consumer(buffer))
    return get_circular_buffer_instance(buffer, buffer_id)->capacity;
  return -1;
}

//F_StArT
// function DCB_channel_start_listening(buffer) result(return_value) BIND(C, name = 'DCB_channel_start_listening')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: return_value
// end function DCB_channel_start_listening
//F_EnD
//! Start the main loop of a channel process.
//!
//! It's basically an infinite loop that keeps synchronizing the public/private MPI window so that the "passively"
//! received data is actually seen (yeah, that's not passive for real...)
//! It also checks at every iteration for a signal indicating that it needs to do something else, like returning, or
//! calling MPI_Barrier()
//C_StArT
int32_t DCB_channel_start_listening(distributed_circular_buffer_p buffer //!< [in]
                                    )
//C_EnD
{
  if (!is_channel(buffer))
    return -1;

  volatile channel_signal_t* signal = get_channel_signal_ptr(buffer, buffer->channel_id);

  while (1) {
    sleep_us(DCB_WINDOW_SYNC_DELAY_US);
    MPI_Win_lock(MPI_LOCK_SHARED, buffer->server_rank, 0, buffer->window);
    MPI_Win_unlock(buffer->server_rank, buffer->window);

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
//! It _must_ be called by every consumer and producer. Calling it from a channel process has no effect (but your
//! channel should be busy listening, not doing barriers)
void DCB_full_barrier(distributed_circular_buffer_p buffer) {
  if (is_root(buffer))
    send_channel_signal(buffer, RSIG_FULL_BARRIER);

  if (!is_channel(buffer))
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
//! from a producer or a channel has no effect.
void DCB_server_barrier(distributed_circular_buffer_p buffer) {
  if (is_root(buffer))
    send_channel_signal(buffer, RSIG_SERVER_BARRIER);

  if (is_consumer(buffer))
    MPI_Barrier(buffer->server_communicator);
}

//F_StArT
//  function DCB_put(buffer, src_data, num_elements, operation) result(num_available) BIND(C, name = 'DCB_put')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value           :: buffer          !< Buffer where we want to insert data
//    integer(DATA_ELEMENT), dimension(*), intent(in) :: src_data !< Data to insert
//    integer(C_INT), intent(in), value        :: num_elements    !< How many data elements we want to insert
//    integer(C_INT), intent(in), value        :: operation       !< Whether to commit the transaction or wait
//    integer(C_INT) :: num_available !< How many element can still fit in the buffer after this call
//  end function DCB_put
//F_EnD
//! @brief Insert data into the given buffer, once there is enough space (will wait if there isn't enough initially)
//!
//! The data will be inserted into the buffer instance associated with the calling process.
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the root node (otherwise it might try to read data that has not yet arrived).
//! An operation must be specified. The options for now are either COMMIT or NO_COMMIT. COMMIT will make the data
//! available for the consumer to read, whereas NO_COMMIT will _send_ the data but _not_ make it available.
//! _With NO_COMMIT, the data is still sent through MPI_.
//!
//! @return How many elements can still fit after the insertion, if everything went smoothly, -1 otherwise
//C_StArT
data_element DCB_put(
    distributed_circular_buffer_p buffer,       //!< [in,out] Distributed buffer in which we want to put data
    data_element* const           src_data,     //!< [in] Pointer to the data we want to insert
    const int                     num_elements, //!< [in] How many #data_element tokens we want to insert
    const int                     operation     //!< [in] What operation to perform (whether to commit the transaction)
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  io_timer_start(&timer);

  const int num_spaces = DCB_wait_space_available(buffer, num_elements);
  if (num_spaces < 0)
    return -1;

  const int target_rank = buffer->local_header.target_rank;

  // NOTE: We could in theory use the MPI_MODE_NOCHECK, but it does not work with OpenMPI 4.0.5
  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, 0, buffer->window);

  data_element       in_index = buffer->local_header.circ_buffer.m.in[CB_PARTIAL];
  const data_element capacity = buffer->local_header.capacity;

  // First segment
  const int num_elem_segment_1 = num_elements >= (capacity - in_index) ? (capacity - in_index + 1) : num_elements;
  MPI_Accumulate(
      src_data, num_elem_segment_1, CB_MPI_ELEMENT_TYPE, target_rank, buffer_element_displacement(buffer, in_index),
      num_elem_segment_1, CB_MPI_ELEMENT_TYPE, MPI_REPLACE, buffer->window);

  // Update temporary insertion pointer
  in_index += num_elem_segment_1;
  if (in_index > capacity)
    in_index = 0;

  // Second segment (if there is one)
  const int num_elem_segment_2 = num_elements - num_elem_segment_1;
  if (num_elem_segment_2 > 0) {
    MPI_Accumulate(
        src_data + num_elem_segment_1, num_elem_segment_2, CB_MPI_ELEMENT_TYPE, target_rank,
        buffer_element_displacement(buffer, in_index), num_elem_segment_2, CB_MPI_ELEMENT_TYPE, MPI_REPLACE,
        buffer->window);

    in_index += num_elem_segment_2;
  }

  // Update partial insertion pointer (locally only)
  buffer->local_header.circ_buffer.m.in[CB_PARTIAL] = in_index;

  // Update insertion index remotely and locally
  if (operation == CB_COMMIT) {
    buffer->local_header.circ_buffer.m.in[CB_FULL] = in_index;
    MPI_Accumulate(
        buffer->local_header.circ_buffer.m.in, 2, CB_MPI_ELEMENT_TYPE, target_rank,
        insertion_index_displacement(buffer), 2, CB_MPI_ELEMENT_TYPE, MPI_REPLACE, buffer->window);
  }

  MPI_Win_unlock(target_rank, buffer->window);

  io_timer_stop(&timer);

  buffer->local_header.circ_buffer.stats.num_write_elems += num_elements;
  buffer->local_header.circ_buffer.stats.num_writes++;
  buffer->local_header.circ_buffer.stats.total_write_time_ms += io_time_ms(&timer);

  return get_available_space(&buffer->local_header);
}

//F_StArT
//  function DCB_get(buffer, buffer_id, dest_data, num_elements, operation) result(num_available) BIND(C, name = 'DCB_get')
//    import :: C_PTR, C_INT, DATA_ELEMENT
//    implicit none
//    type(C_PTR), intent(in), value       :: buffer        !< DCB from which we want to read
//    integer(C_INT), intent(in), value    :: buffer_id     !< Which buffer in the DCB we want to read from
//    integer(DATA_ELEMENT), dimension(*), intent(inout) :: dest_data !< Where to put the data from the buffer
//    integer(C_INT), intent(in), value    :: num_elements  !< How many data elements to read
//    integer(C_INT), intent(in), value    :: operation     !< Whether to actually extract, read or just peek at the data
//    integer(C_INT) :: num_available
//  end function DCB_get
//F_EnD
//C_StArT
int DCB_get(
    distributed_circular_buffer_p buffer,       //!< [in,out] DCB from which we want to read
    const int                     buffer_id,    //!< [in] Specific buffer in the DCB
    int32_t*                      dest_data,    //!< [in] Where to put the data from the buffer
    const int                     num_elements, //!< [in] How many elements to read
    const int                     operation     //!< [in] What operation to perform: extract, read or just peek
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  io_timer_start(&timer);

  const int num_available_elem = DCB_wait_data_available(buffer, buffer_id, num_elements);
  if (num_available_elem < 0)
    return -1;

  circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id);

  // Update "max fill" metric
  if (instance->circ_buffer.stats.max_fill < (uint64_t)num_available_elem)
    instance->circ_buffer.stats.max_fill = num_available_elem;

  // Retrieve indices/pointers
  int32_t                   out_index   = instance->circ_buffer.m.out[CB_PARTIAL];
  const int32_t             capacity    = instance->capacity;
  const data_element* const buffer_data = instance->circ_buffer.data;

  // 1st segment
  const int num_elements_1 = num_elements >= (capacity - out_index) ? (capacity - out_index + 1) : num_elements;
  copy_elements(dest_data, buffer_data + out_index, num_elements_1);

  // Update temporary extraction pointer
  out_index += num_elements_1;
  if (out_index > capacity)
    out_index = 0;

  // 2nd segment (if there is one)
  const int num_elements_2 = num_elements - num_elements_1;
  if (num_elements_2 > 0) {
    copy_elements(dest_data + num_elements_1, buffer_data + out_index, num_elements_2);

    out_index += num_elements_2;
  }

  // Update partial extraction pointer if needed
  if (operation != CB_PEEK) {
    instance->circ_buffer.m.out[CB_PARTIAL] = out_index;
  }

  // Update full extraction pointer if needed
  if (operation == CB_COMMIT) {
    memory_fence(); // Make sure everything has been read, and the temp pointer actually updated

    volatile data_element* d_out = &instance->circ_buffer.m.out[CB_FULL];
    *d_out                       = out_index; // Update actual extraction pointer
  }

  io_timer_stop(&timer);
  instance->circ_buffer.stats.total_read_time_ms += io_time_ms(&timer);
  if (operation != CB_PEEK) {
    instance->circ_buffer.stats.num_read_elems += (uint64_t)num_elements;
    instance->circ_buffer.stats.num_reads++;
  }

  return get_available_data(instance);
}

/**
 * @brief Check the integrity of a single CB instance within the given DCB _(only valid for consumers/channels)_
 * @return 0 if the check is successful, a negative value otherwise
 */
int DCB_check_instance_integrity(
    const distributed_circular_buffer_p buffer,   //!< [in] The DCB we want to check
    const int                           buffer_id //!< [in] The ID of the CB we want to check within the DCB
) {
  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id);
  if (check_instance_consistency(instance) != 0)
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
  if (buffer == NULL) {
    printf("Buffer pointer is NULL!\n");
    return -1;
  }

  if ((buffer->producer_id >= 0) + (buffer->channel_id >= 0) + (buffer->consumer_id >= 0) != 1) {
    printf("Inconsistency in DCB IDs\n");
    return -1;
  }

  if (is_producer(buffer)) {
    if (check_instance_consistency(&buffer->local_header) != 0) {
      printf("Local instance failed integrity check!\n");
      return -1;
    }
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
