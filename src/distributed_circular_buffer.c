/*
 * Copyright (C) 2022  Environnement et Changement climatique Canada
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
 *     M. Valin,   Recherche en Prevision Numerique, 2020-2022
 *     V. Magnoux, Recherche en Prevision Numerique, 2020-2022
 */

#include <immintrin.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>

//C_StArT
#include <mpi.h>

#include "io-server/circular_buffer.h"
//C_EnD

#include "io-server/shmem_arena.h"
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

//! Rank on the server of the server process that will be the "root" of the DCB. _On the server communicator!!!_
static const int DCB_SERVER_ROOT_RANK = 0;

//C_StArT
/**
 * @brief Wrapper struct around a regular circular buffer. It adds some information for management within a set of
 * distributed circular buffers.
 *
 * Please don't change the order of data members (unless you know what the consequences are).
 */
typedef struct {
  int             target_rank; //!< With which process this instance should communicate for data transfers
  int             id;          //!< ID number assigned to the buffer instance
  uint64_t        capacity;    //!< How many bytes can fit in this instance
  void*           dummy;       //!< Force 64-bit alignment of the rest of the struct
  circular_buffer circ_buffer; //!< The buffer contained in this instance
} circular_buffer_instance;

typedef circular_buffer_instance* circular_buffer_instance_p;

enum channel_signal
{
  RSIG_NONE = 0,
  RSIG_SERVER_BARRIER,
  RSIG_FULL_BARRIER,
  RSIG_STOP,
};

typedef enum channel_signal channel_signal_t;

typedef struct {
  int32_t root_rank; //!< Rank of the "root" process of the DCB. Has to be on the server, and is not necessarily 0. _On the entire DCB communicator!_

  int32_t num_server_consumers;     //!< How many server processes will _read_ from individual buffers (server-bound)
  int32_t num_server_producers;     //!< How many server processes will _write_ to individual buffers (client-bound)
  int32_t num_channels;             //!< How many channels (server processes) can be used for MPI 1-sided communication (1 process = 1 channel)

  int32_t data_size;                  //!< Size of the data described by this struct, in number of #data_element (excluding the size of this struct)
  int32_t num_server_bound_instances; //!< How many server-bound client processes there are ( = number of offsets in shared memory)
  int32_t num_client_bound_instances; //!< How many client-bound client processes there are
  int32_t num_elem_per_server_bound_instance; //!< How many #data_element can be stored in each server-bound instance
  int32_t num_elem_per_client_bound_instance; //!< How many #data_element can be stored in each server-bound instance

  int32_t server_bound_win_offsets_index; 
  int32_t client_bound_win_offsets_index;
  int32_t server_bound_client_ranks_index;
  int32_t client_bound_client_ranks_index;
  int32_t channel_ranks_index;
  int32_t signals_index;
} control_header;

typedef control_header* control_header_p;

/**
 * @brief All information necessary to manage a set of circuler buffers accessed remotely (from _clients_), whose entire data is
 * located on a single node and accessible by any _server_ process.
 *
 * This struct is a _collective_ object, in the sense that every process that wants a remote circular buffer must have
 * an instance of the entire set description, including the _root_ process. The set is composed of multiple circular
 * buffer _instances_.
 * Some instances will be for _server-bound_ data and will be written by client processes and read by server processes.
 * The other instances will be for _client-bound_ data, and will be written by server processes and read by client processes.
 * All data contained in the instances (either server-bound or client-bound) is stored on the server and is accessible by any
 * server process.
 * 
 * Processes that read from a buffer are _consumers_ and processes that write to it are _producers_. There is exactly one
 * consumer and one producer for each buffer instance. A server-bound server process can be a consumer for multiple buffer instances,
 * a client-bound server process can be a producer for multiple buffer instances. However, a server process will never
 * handle both server-bound and client-bound data. Client processes handle exactly one buffer instance each
 * and each only holds a copy of the header of that instance (since the data content is located on the server).
 * _Server processes must be located on the same physical node._
 * 
 * The data on the server processes also form an MPI window into shared memory, which is
 * accessible remotely by every client process that collectively own the distributed buffer set.
 * _However, each client will only ever access the portion of that window that contains its associated buffer
 * instance._ This data is directly accessible (local load/store) by every server process. The only item that is ever
 * modified by a consumer (server-bound) server process is the extraction index in a circular buffer instance, whenever that consumer has
 * finished reading (extracting) a bunch of data from that instance. The opposite is true for producer (client-bound) server processes:
 * they can modify everything except the extraction pointer, which is updated (remotely) by the client process responsible for an instance.
 * In every remote communication, the server processes are always passive; this removes any need for explicit synchronization related
 * to data transfer. 
 * 
 * A set of _channel_ processes also exist on the server as communication targets only (they don't do any actual work).
 * Each channel can be used to transmit server-bound and client-bound data by any client process. This enables a
 * larger bandwidth for remote data transfers.
 * To ensure that the data will be received as quickly as possible from the passive/target
 * side, each channel process is constantly polling for updates by synchronizing the window.
 * _The channel processes must be located on the same physical node as the server processes._
 * 
 * One of the server processes is considered the _root_ process and is responsible for allocating/deallocating shared
 * memory and for initializing the buffer instances.
 */
typedef struct {
  int32_t channel_id;             //!< This provides an ID on channel processes (and -1 on other processes)
  int32_t server_bound_server_id; //!< ID on a server-bound server process (-1 on other processes)
  int32_t client_bound_server_id; //!< ID on a client-bound server process (-1 on other processes)
  int32_t server_bound_client_id; //!< ID on a server-bound client process (-1 on other processes)
  int32_t client_bound_client_id; //!< ID on a client-bound client process (-1 on other processes)

  int32_t dcb_rank;           //!< Rank of this process on the global DCB communicator
  int32_t server_rank;        //!< Rank of this process on the server communicator. (-1 if somewhere else)
  int32_t communication_type; //!< Can be either server-bound, model-bound or channel

  data_element window_offset; //!< Offset into the MPI window at which this client's circular buffer is located

  MPI_Comm communicator; //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win  window;       //!< MPI window into the circular buffers themselves, on the process which holds all data
  MPI_Comm server_communicator; //!< Communicator that groups processes located on the IO server

  io_timer_t existence_timer; //!< To keep track of how long ago the buffer was created

  //! Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  //! Will have some _control data_ at the beginning.
  data_element* raw_data;

  data_element* control_data;
  data_element* server_bound_win_offsets;
  data_element* client_bound_win_offsets;
  data_element* server_bound_client_ranks;
  data_element* client_bound_client_ranks;
  data_element* channel_ranks;
  channel_signal_t* channel_signals;

  //! Control metadata that is common to every DCB header. It will be computed once and transmitted to every DCB handle.
  control_header control_metadata;

  //! Header of the circular buffer instance. This is only valid for clients (server- and client-bound)
  //! This is the local copy and will be synchronized with the remote one, located in the shared memory region of the
  //! server processes
  circular_buffer_instance local_header;

} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;
//C_EnD

//F_StArT
//  interface
//F_EnD

// Forward declarations
static inline void print_instance(const circular_buffer_instance_p instance);
static inline void print_control_metadata(const control_header_p header, const data_element* data);
//C_StArT
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p, int32_t);
void DCB_full_barrier(distributed_circular_buffer_p buffer);
int  DCB_check_integrity(const distributed_circular_buffer_p buffer, int verbose);
//C_EnD

//! @{ @name Various size calculators

//! Gives the minumum _even_ number of #data_element that hold a circular_buffer (its header)
static inline size_t circular_buffer_header_size() {
  return num_bytes_to_num_elem_64(sizeof(circular_buffer));
}

//! Compute the _even_ number of #data_element taken by the circular_buffer_instance struct
static inline size_t instance_header_size() {
  return num_bytes_to_num_elem_64(sizeof(circular_buffer_instance));
}

//! Size of the common server header, in number of #data_element tokens
static inline size_t control_data_size(
    const int num_buffers, //!< Number of circular buffers in the set
    const int num_channels //!< Number of communication channels (PEs) used for MPI 1-sided calls
) {
  const size_t num_bytes = (size_t)num_buffers  * sizeof(data_element) +    // window offsets size (server- and client-bound)
                           (size_t)num_buffers  * sizeof(data_element) +    // client ranks size (server- and client-bound)
                           (size_t)num_channels * sizeof(data_element) +    // channel (ghost process) ranks size
                           (size_t)num_channels * sizeof(channel_signal_t); // signals size
  return num_bytes_to_num_elem_64(num_bytes);
}

//! Size in #data_element of an entire circular buffer instance, based on the number of desired elements
static inline size_t total_circular_buffer_instance_size(
  const int num_desired_elem //!< Number of elements we want to be able to store in the buffer
) {
  // Always add 1 because there is one wasted slot in the data array (to be able to distinguish between empty and full)
  // If we're using 32-bit elements, round up the number of desired elements so that they align to 64 bits
  const int add = 1 + (num_desired_elem * sizeof(data_element)) % 8 == 0 ? 0 : 1;
  return instance_header_size() + num_desired_elem + add;
}

//! Compute the total size needed by the shared memory window to fit all circular buffers and metadata, in number of
//! #data_element tokens. The content includes the shared memory common header, the server-bound circular buffers and
//! client-bound circular buffers.
static inline size_t total_window_num_elem(
    const int num_server_bound_buffers,      //!< How many server-bound circular buffer instances the set will hold
    const int num_client_bound_buffers,      //!< How many client-bound circular buffer instances the set will hold
    const int num_channels,                  //!< How many communication channels with the IO server will be used
    const size_t num_elem_per_server_buffer, //!< How many #data_element tokens we want to be able to store in each server-bound buffer
    const size_t num_elem_per_client_buffer  //!< How many #data_element tokens we want to be able to store in each client-bound buffer
) {
  return num_server_bound_buffers * (total_circular_buffer_instance_size(num_elem_per_server_buffer)) +
         num_client_bound_buffers * (total_circular_buffer_instance_size(num_elem_per_client_buffer)) +
         control_data_size(num_server_bound_buffers + num_client_bound_buffers, num_channels);
}

//! @}

//! @{ \name Circular buffer instance management

static inline size_t get_available_space_bytes(
    const circular_buffer_instance_p buffer //!< [in] The buffer instance we want to query
) {
  return CB_get_available_space_bytes(&buffer->circ_buffer);
}

static inline size_t get_available_data_bytes(
    const circular_buffer_instance_p buffer //!< [in] The buffer instance we want to query
) {
  return CB_get_available_data_bytes(&buffer->circ_buffer);
}

//! @return CB_SUCCESS if everythin is fine, a negative error code if not
static inline int check_instance_consistency(
    const circular_buffer_instance_p instance,  //!< Buffer to check
    const int verbose                           //!< Whether to print something when the check fails
    )
{
  if (instance == NULL) {
    if (verbose) printf("Invalid b/c NULL pointer\n");
    return CB_ERROR_INVALID_POINTER;
  }

  if (instance->capacity != CB_get_capacity_bytes(&instance->circ_buffer))
  {
    if (verbose) printf("Invalid b/c wrong instance capacity (%ld, but CB has %ld)\n", instance->capacity, CB_get_capacity_bytes(&instance->circ_buffer));
    return DCB_ERROR_INVALID_CAPACITY;
  }

  if (instance->target_rank < 0)
  {
    if (verbose) printf("Invalid rank\n");
    return DCB_ERROR_INVALID_RANK;
  }

  if (instance->id < 0)
  {
    if (verbose) printf("Invalid instance ID\n");
    return DCB_ERROR_INVALID_INSTANCE;
  }

  const int cb_status = CB_check_integrity(&instance->circ_buffer);
  if (cb_status < 0)
  {
    if (verbose) printf("Invalid b/c CB integrity check failed: %s\n", CB_error_code_to_string(cb_status));
    return cb_status;
  }

  return CB_SUCCESS;
}

//! @}

//! @{ \name Shared server data management

//! Initialize the common_server_header of the given distributed_circular_buffer. This computes the offset of every
//! buffer instance within the set, sets the value of all ranks to -1 and all signals to RSIG_NONE.
static inline void init_control_metadata(
    control_header_p       metadata,                 //!< [in,out] Pointer to metadata struct we want to initialize
    const int              num_server_bound_buffers, //!< [in] How many server-bound circular buffer instances there are in the set
    const int              num_client_bound_buffers, //!< [in] How many client-bound circular buffer instances there are in the set
    const int              num_channels,             //!< [in] How many channels can be used to transmit data
    const size_t           num_elem_per_server_bound_instance, //!< [in] How many elements each server-bound instance can store
    const size_t           num_elem_per_client_bound_instance  //!< [in] How many elements each client-bound instance can store
) {
  metadata->data_size                          = control_data_size(num_server_bound_buffers + num_client_bound_buffers, num_channels);
  metadata->num_server_bound_instances         = num_server_bound_buffers;
  metadata->num_client_bound_instances         = num_client_bound_buffers;
  metadata->num_elem_per_server_bound_instance = num_elem_per_server_bound_instance;
  metadata->num_elem_per_client_bound_instance = num_elem_per_client_bound_instance;
  metadata->num_channels                       = num_channels;

  // Set up indices
  metadata->server_bound_win_offsets_index  = 0;
  metadata->client_bound_win_offsets_index  = metadata->server_bound_win_offsets_index  + metadata->num_server_bound_instances;
  metadata->server_bound_client_ranks_index = metadata->client_bound_win_offsets_index  + metadata->num_client_bound_instances;
  metadata->client_bound_client_ranks_index = metadata->server_bound_client_ranks_index + metadata->num_server_bound_instances;
  metadata->channel_ranks_index             = metadata->client_bound_client_ranks_index + metadata->num_client_bound_instances;
  metadata->signals_index                   = metadata->channel_ranks_index             + metadata->num_channels;
}

//! Initialize the pointer in the DCB struct that point into shared memory.
//! _The shared memory must be allocated before calling this function._
static inline void init_control_pointers(
    distributed_circular_buffer_p buffer
) {
  buffer->control_data = buffer->raw_data;
  buffer->server_bound_win_offsets  = buffer->control_data + buffer->control_metadata.server_bound_win_offsets_index;
  buffer->client_bound_win_offsets  = buffer->control_data + buffer->control_metadata.client_bound_win_offsets_index;
  buffer->server_bound_client_ranks = buffer->control_data + buffer->control_metadata.server_bound_client_ranks_index;
  buffer->client_bound_client_ranks = buffer->control_data + buffer->control_metadata.client_bound_client_ranks_index;
  buffer->channel_ranks             = buffer->control_data + buffer->control_metadata.channel_ranks_index;
  buffer->channel_signals           = (channel_signal_t*)(buffer->control_data + buffer->control_metadata.signals_index);
}

//! Initialize what is currently possible of the control block within the shared memory region. Process ranks will
//! be initialized later by their own process.
//! _The control pointers in the DCB struct must be initialized prior to calling this function._
static inline void init_control_data(
    distributed_circular_buffer_p buffer
) {
  // Compute window offsets
  data_element current_offset = buffer->control_metadata.data_size;
  for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {
    buffer->server_bound_win_offsets[i] = current_offset;
    current_offset += total_circular_buffer_instance_size(buffer->control_metadata.num_elem_per_server_bound_instance);
  }
  for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {
    buffer->client_bound_win_offsets[i] = current_offset;
    current_offset += total_circular_buffer_instance_size(buffer->control_metadata.num_elem_per_client_bound_instance);
  }

  // Clear process ranks and signals
  for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {
    buffer->server_bound_client_ranks[i] = -1;
  }
  for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {
    buffer->client_bound_client_ranks[i] = -1;
  }
  for (int i = 0; i < buffer->control_metadata.num_channels; ++i) {
    buffer->channel_ranks[i]   = -1;
    buffer->channel_signals[i] = RSIG_NONE;
  }
}

//! Print the contents of the given control metadata header. Optionally print the corresponding control data.
static inline void print_control_metadata(
    const control_header_p header, //! [in] Control metadata header we want to print
    const data_element* data       //! [in] (Optional) Pointer to the control data. Will be ignored if NULL.
) {
  printf(
      "------------------------------------------\n"
      "Common server header size:  %d elements\n"
      "Num server consumers:       %d\n"
      "Num server producers:       %d\n"
      "Num server-bound buffers:   %d\n"
      "Num client-bound buffers:   %d\n"
      "Num channels (and signals): %d\n"
      "Address %p\n",
      header->data_size, header->num_server_consumers, header->num_server_producers,
      header->num_server_bound_instances, header->num_client_bound_instances, header->num_channels, (void*)header);
  printf(
    "Indices:\n"
    "  Server-bound window offsets: %d\n"
    "  Client-bound window offsets: %d\n"
    "  Server-bound client ranks:   %d\n"
    "  Client-bound client ranks:   %d\n"
    "  Channel ranks:               %d\n"
    "  Signals:                     %d",
    header->server_bound_win_offsets_index, header->client_bound_win_offsets_index,
    header->server_bound_client_ranks_index, header->client_bound_client_ranks_index,
    header->channel_ranks_index, header->signals_index);

  if (data != NULL) {
    printf("\nData: (%p)", (void*)data);
    for (int i = 0; i < header->data_size; ++i) {
      if (i % 4 == 0) printf("\n  ");
      printf("%8ld ", (long)(data[i]));
    }
    printf("\n");
    if (header->num_server_bound_instances > 0) {
      printf("\nOffsets (server-bound):        ");
      const data_element* server_bound_offsets = data + header->server_bound_win_offsets_index;
      for (int i = 0; i < header->num_server_bound_instances; ++i) {
        printf("%ld ", (long)server_bound_offsets[i]);
      }
    }
    if (header->num_client_bound_instances > 0) {
      printf("\nOffsets (client-bound):        ");
      const data_element* client_bound_offsets = data + header->client_bound_win_offsets_index;
      for (int i = 0; i < header->num_client_bound_instances; ++i) {
        printf("%ld ", (long)client_bound_offsets[i]);
      }
    }
    if (header->num_server_bound_instances > 0) {
      printf("\nServer-bound client ranks:     ");
      const data_element* sb_client_ranks = data + header->server_bound_client_ranks_index;
      for (int i = 0; i < header->num_server_bound_instances; ++i) {
        printf("%ld ", (long)sb_client_ranks[i]);
      }
    }
    if (header->num_client_bound_instances > 0) {
      printf("\nClient-bound client ranks:     ");
      const data_element* cb_client_ranks = data + header->client_bound_client_ranks_index;
      for (int i = 0; i < header->num_client_bound_instances; ++i) {
        printf("%ld ", (long)cb_client_ranks[i]);
      }
    }
    printf("\nChannel ranks:                   ");
    const data_element* channel_ranks = data + header->channel_ranks_index;
    for (int i = 0; i < header->num_channels; ++i) {
      printf("%ld ", (long)channel_ranks[i]);
    }
    printf("\nSignals:                         ");
    channel_signal_t* signals = (channel_signal_t*)(data + header->signals_index);
    for (int i = 0; i < header->num_channels; ++i) {
      printf("%ld ", (long)signals[i]);
    }
  }
  printf("\n----------------------------------------\n");
}

//! @}

//! @{ @name Displacement calculators (within the MPI window)

//! Compute the displacement in the shared memory window where the window offset for the specified server-bound instance is
//! located. That displacement should land in the appropriate location in the common_server_header struct, which is
//! itself at the start of the window
//! @return The window displacement where a server-bound client can find the offset (displacement) of its buffer within that window
static inline MPI_Aint server_bound_window_offset_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    const int client_id //!< [in] ID of the (server-bound) client for which we want the window offset location
) {
  return (MPI_Aint)(buffer->control_metadata.server_bound_win_offsets_index + client_id);
}

//! Compute the displacement in the shared memory window where the window offset for the specified client-bound instance is
//! located. That displacement should land in the appropriate location in the common_server_header struct, which is
//! itself at the start of the window
//! @return The window displacement where a server-bound client can find the offset (displacement) of its buffer within that window
static inline MPI_Aint client_bound_window_offset_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] The DCB we are querying
    const int client_id //!< [in] ID of the (client-bound) client for which we want the window offset location
) {
  return (MPI_Aint)(buffer->control_metadata.client_bound_win_offsets_index + client_id);
}

//! @return The displacement in the shared memory window where the MPI rank of the given server-bound client is located.
static inline MPI_Aint server_bound_client_rank_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    const int client_id                         //!< [in] ID of the server-bound client whose rank we seek
) {
  return (MPI_Aint)(buffer->control_metadata.server_bound_client_ranks_index + client_id);
}

//! @return The displacement in the shared memory window where the MPI rank of the given client-bound client is located.
static inline MPI_Aint client_bound_client_rank_displacement(
  const distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
  const int client_id                         //!< [in] ID of the client-bound client whose rank we seek
) {
  return (MPI_Aint)(buffer->control_metadata.client_bound_client_ranks_index + client_id);
}

//! Compute the displacement in the shared memory window where the given element index for a circular buffer is located.
//! _Can only be called from a client process._
//! Displacement of the given element within the MPI window
static inline MPI_Aint buffer_element_displacement(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer in which the element is located
    const data_element                  index   //!< [in] Index of the element within the buffer
) {
  // Start of the buffer element section within the buffer instance
  const ptrdiff_t ptr_offset = (data_element*)buffer->local_header.circ_buffer.data - (data_element*)&buffer->local_header;
  const MPI_Aint base_displacement = buffer->window_offset + ptr_offset; // Add the start of this instance within the MPI window
  const MPI_Aint elem_displacement = base_displacement + index;          // Add the position of the element within the CB data
  return elem_displacement;
}

//! _Must be called from a client process._
//! @return The displacement in the shared memory window where the insertion index of the given buffer is located
static inline MPI_Aint insertion_index_displacement(const distributed_circular_buffer_p buffer) {
  // Position of the insertion index within the CB instance
  const ptrdiff_t ptr_offset = (data_element*)buffer->local_header.circ_buffer.m.in - (data_element*)&buffer->local_header;
  const data_element displacement = buffer->window_offset + ptr_offset; // Add the start of the instance within the MPI window
  return (MPI_Aint)displacement;
}

//! Get the displacement (in number of #data_element) in the shared memory window where the start of the buffer is
//! located. Must be called from a producer process
static inline MPI_Aint remote_header_displacement(const distributed_circular_buffer_p buffer) {
  return buffer->window_offset;
}

//! @}

//! @{ @name Helper functions

//! Check wether the given buffer is on a server-bound client process
static inline int is_server_bound_client(const distributed_circular_buffer_p buffer) {
  return (buffer->server_bound_client_id >= 0);
}

//! Check wether the given buffer is on a client-bound client process
static inline int is_client_bound_client(const distributed_circular_buffer_p buffer) {
  return (buffer->client_bound_client_id >= 0);
}

//! Check whether the given distributed buffer is located on a client process
static inline int is_client(const distributed_circular_buffer_p buffer) {
  return (is_server_bound_client(buffer) || is_client_bound_client(buffer));
}

//! Check whether the given distributed buffer is located on the DCB root process
static inline int is_root(const distributed_circular_buffer_p buffer) {
  return (buffer->server_rank == DCB_SERVER_ROOT_RANK);
}

//! Check whether the given distributed buffer is located on a channel process
static inline int is_channel(const distributed_circular_buffer_p buffer) {
  return (buffer->channel_id >= 0);
}

//! Check whether the given distributed buffer is located on a client process
static inline int is_on_server(const distributed_circular_buffer_p buffer) {
  return (buffer->server_rank >= 0);
}

//! Check wether the given DCB is on a server-bound server process
static inline int is_server_bound_server(const distributed_circular_buffer_p buffer) {
  return (buffer->server_bound_server_id >= 0);
}

//! Check wether the given DCB is on a client-bound server process
static inline int is_client_bound_server(const distributed_circular_buffer_p buffer) {
  return (buffer->client_bound_server_id >= 0);
}

//! Set the process rank to be used for MPI communication for the given CB instance
static inline void assign_target_channel(
    const distributed_circular_buffer_p buffer,    //!< [in] DCB to which this instance belongs
    const circular_buffer_instance_p    instance,  //!< [in, out] Buffer instance whose target we want to set
    const int                           channel_id //!< [in] Which channel this instance should use
) {
  instance->target_rank = buffer->channel_ranks[channel_id];
}

//! Get a pointer to a certain circular_buffer_instance within the given distributed_circular_buffer
static inline circular_buffer_instance_p get_circular_buffer_instance(
    distributed_circular_buffer_p buffer_set, //!< [in] The buffer set from which we want the circ buffer instance
    const int                     buffer_id,  //!< [in] ID of the circ buffer instance within the buffer set
    const int                     communication_type //!< [in] Server- or client-bound instance (#DCB_SERVER_BOUND_TYPE or #DCB_CLIENT_BOUND_TYPE)
) {
  const data_element* offsets =
              communication_type == DCB_SERVER_BOUND_TYPE ? buffer_set->server_bound_win_offsets :
              communication_type == DCB_CLIENT_BOUND_TYPE ? buffer_set->client_bound_win_offsets :
              NULL;
  return (circular_buffer_instance_p)(buffer_set->raw_data + offsets[buffer_id]);
}

//! Retrieve the window offset of the appropriate circular buffer instance within the given distributed buffer from the server
//! (according to its ID). _Can only be called from a client process_
static void retrieve_window_offset_from_remote(
    distributed_circular_buffer_p buffer //!< Buffer for which we want the window offset
) {
  // We communicate through the root process, because we don't yet know the rank of our target
  MPI_Win_lock(MPI_LOCK_SHARED, buffer->control_metadata.root_rank, MPI_MODE_NOCHECK, buffer->window);
  const MPI_Aint displacement =
    is_server_bound_client(buffer) ? server_bound_window_offset_displacement(buffer, buffer->server_bound_client_id) :
    is_client_bound_client(buffer) ? client_bound_window_offset_displacement(buffer, buffer->client_bound_client_id) :
    -1;
  MPI_Get(&buffer->window_offset, 1, CB_MPI_ELEMENT_TYPE, buffer->control_metadata.root_rank, displacement, 1, CB_MPI_ELEMENT_TYPE, buffer->window);
  MPI_Win_unlock(buffer->control_metadata.root_rank, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance.
//! _Can only be called from a client process._
static inline void update_local_header_from_remote(
    distributed_circular_buffer_p buffer, //!< [in] Buffer set from which we want to update a single instance
    const int                     full    //!< [in] Whether to perform a full update (if =1) or only a partial one
) {
  // Gotta load the target rank first, because it's going to be overwritten by the MPI_Get
  const int      target_rank  = buffer->local_header.target_rank >= 0 ? buffer->local_header.target_rank : buffer->control_metadata.root_rank;
  const int      num_elem     = instance_header_size();
  const MPI_Aint displacement = remote_header_displacement(buffer);

  // Retrieve the entire header in a buffer: lock window, get, then unlock
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

//! Set the value of this client process' rank in the server shared data area, at the right location in the MPI window.
//! (We use the rank on the global DCB communicator)
static void set_client_rank_on_remote(
    distributed_circular_buffer_p buffer //!< [in,out] Buffer we want to update
) {
  const int          target_rank         = buffer->control_metadata.root_rank; // Target the root of the DCB, which we know to be on the server
  const int          num_elem            = 1; // Rank takes exactly 1 element
  const data_element value               = buffer->dcb_rank;

  const MPI_Aint target_displacement = 
    is_server_bound_client(buffer) ? server_bound_client_rank_displacement(buffer, buffer->server_bound_client_id) :
    is_client_bound_client(buffer) ? client_bound_client_rank_displacement(buffer, buffer->client_bound_client_id) :
    -1;

  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, buffer->window);
  MPI_Put(
      &value, num_elem, CB_MPI_ELEMENT_TYPE, target_rank, target_displacement, num_elem, CB_MPI_ELEMENT_TYPE,
      buffer->window);
  MPI_Win_unlock(target_rank, buffer->window);
}

//! @brief Copy from the consumer process the header associated with this circular buffer instance and compute how much
//! space is available
//! @return The number of #data_element tokens that can still be stored in the buffer
static inline size_t get_available_space_from_remote_bytes(
    const distributed_circular_buffer_p buffer //!< [in] The buffer we want to query
) {
  update_local_header_from_remote(buffer, 0);
  return get_available_space_bytes(&buffer->local_header);
}

//! @brief Stop and wait until there is enough space in this circular buffer instance.
//!
//! If the latest copy of the header shows enough space, compute + return the result immediately.
//! Otherwise, copy metadata from the consumer process and check, until there is enough space.
//! _Can only be called from a server-bound client._
//! @return The number of available elements according to the latest copy of the header. If there was not enough
//! initially, that copy is updated. If there is an error, returns a negative error code.
static int64_t DCB_wait_space_available_client(
    distributed_circular_buffer_p buffer,               //!< [in] Pointer to the distributed buffer we're waiting for
    const size_t                  num_requested_bytes,  //!< [in] Needed number of available bytes
    const int                     timeout_ms            //!< [in] How long (in ms) to wait before declaring failure, (almost) forever if negative
) {
  // Function inputs and buffer consistency checks
  if (buffer == NULL)
    return CB_ERROR_INVALID_POINTER;

  if (!is_server_bound_client(buffer))
    return DCB_ERROR_WRONG_CALLER_ROLE;

  const circular_buffer_instance_p instance = &buffer->local_header;
  if (num_requested_bytes > CB_get_capacity_bytes(&instance->circ_buffer))
    return CB_ERROR_INSUFFICIENT_SPACE;
  
  const int status = check_instance_consistency(instance, 1);
  if (status != CB_SUCCESS)
    return status;

  // First check locally for space
  size_t num_available_bytes = get_available_space_bytes(instance);
  if (num_available_bytes >= num_requested_bytes)
    return num_available_bytes;

  // Then get info from remote location, until there is enough space (or time runs out)
  size_t num_waits = 0;
  const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / DCB_SPACE_CHECK_DELAY_US;
  num_available_bytes = get_available_space_from_remote_bytes(buffer);
  for (num_waits = 0; num_waits < max_num_waits && num_available_bytes < num_requested_bytes; ++num_waits) {
    sleep_us(DCB_SPACE_CHECK_DELAY_US);
    num_available_bytes = get_available_space_from_remote_bytes(buffer);
  }

  // Update stats
  buffer->local_header.circ_buffer.stats.total_write_wait_time_ms += num_waits * DCB_SPACE_CHECK_DELAY_US / 1000.0;

  // Check whether there still isn't enough free space in the buffer
  if (num_available_bytes < num_requested_bytes)
    return CB_ERROR_TIMEOUT;

  return num_available_bytes;
}

//! Stop and wait until there is enough data in this circular buffer instance
//! _Can only be called from a server-bound server process._
//!
//! @return The number of data elements in the buffer, if everything goes smoothly, -1 otherwise.
static int64_t DCB_wait_data_available_server(
    const distributed_circular_buffer_p buffer, //!< [in] Buffer we are querying
    const int    buffer_id,           //!< [in] Which specific circular buffer we want to query (there are multiple ones)
    const size_t num_requested_bytes, //!< [in] Number of bytes we want to read
    const int    timeout_ms           //!< [in] How long (in ms) to wait before declaring failure, (almost) forever if negative
) {
  // Function inputs and buffer consistency checks
  if (buffer == NULL)
    return CB_ERROR_INVALID_POINTER;
  
  if (!is_server_bound_server(buffer))
    return DCB_ERROR_WRONG_CALLER_ROLE;

  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id, DCB_SERVER_BOUND_TYPE);
  if (num_requested_bytes > CB_get_capacity_bytes(&instance->circ_buffer))
    return CB_ERROR_INSUFFICIENT_SPACE;
  
  const int status = check_instance_consistency(instance, 1);
  if (status != CB_SUCCESS)
    return status;

  // Only check locally, waiting a bit between each check
  size_t num_available_bytes = get_available_data_bytes(instance);
  size_t num_waits           = 0;
  const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / DCB_DATA_CHECK_DELAY_US;
  for (num_waits = 0; num_waits < max_num_waits && num_available_bytes < num_requested_bytes; ++num_waits) {
    sleep_us(DCB_DATA_CHECK_DELAY_US);
    num_available_bytes = get_available_data_bytes(instance);
  }

  // Update stats
  instance->circ_buffer.stats.total_read_wait_time_ms += num_waits * DCB_DATA_CHECK_DELAY_US / 1000.0;

  // Check whether there still isn't enough data in the buffer
  if (num_available_bytes < num_requested_bytes)
    return CB_ERROR_TIMEOUT;

  return (int64_t)num_available_bytes;
}

//! Initialize the given circular buffer instance, including the circular_buffer it contains
static inline int init_circular_buffer_instance(
    circular_buffer_instance_p instance,    //!< Buffer instance we want to init
    const int                  id,          //!< ID of the buffer instance
    const size_t               num_elements //!< How many elements are taken by the circular buffer
) {
  instance->target_rank = -1;
  instance->id          = id;
  instance->dummy       = NULL;
  instance->capacity    = 0;

  if (CB_init_bytes(&instance->circ_buffer, num_elements * sizeof(data_element)) == NULL)
    return CB_ERROR;

  instance->capacity = CB_get_capacity_bytes(&instance->circ_buffer);

  return CB_SUCCESS;
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
      "Num bytes: %ld, num spaces: %ld\n"
      "Target rank: %ld\n"
      "Instance ID %d\n"
      "Capacity %ld\n",
      get_available_data_bytes(instance), get_available_space_bytes(instance), (int64_t)instance->target_rank, instance->id,
      instance->capacity);
  CB_print_header(&instance->circ_buffer);
  for (uint64_t i = 0; i < sizeof(circular_buffer_instance); ++i) {
    if (i % 8 == 0)
      printf("\n");

    printf("%03d ", ((unsigned char*)instance)[i]);

    if (i% 8 == 7) {
      const uint32_t* pos = (const uint32_t*) &(((const unsigned char*)instance)[i-8]);
      printf(" -- %11d %11d (%ld)", pos[0], pos[1], ((uint64_t*)pos)[0]);
    }
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
  for (int i = 0; i < buffer->control_metadata.num_channels; ++i)
    set_channel_signal(&buffer->channel_signals[i], value);
}

//! Put default values into DCB struct members
static inline void reset_dcb_struct(distributed_circular_buffer_p buffer) {
  buffer->control_metadata.root_rank                  = -1;
  buffer->control_metadata.num_server_producers       = -1;
  buffer->control_metadata.num_server_consumers       = -1;
  buffer->control_metadata.num_server_bound_instances = -1;
  buffer->control_metadata.num_client_bound_instances = -1;
  buffer->control_metadata.num_channels               = -1;

  buffer->channel_id             = -1;
  buffer->server_bound_server_id = -1;
  buffer->client_bound_server_id = -1;
  buffer->server_bound_client_id = -1;
  buffer->client_bound_client_id = -1;

  buffer->communication_type       = -1;
  buffer->server_rank              = -1;
  buffer->dcb_rank                 = -1;
  buffer->local_header.target_rank = -1;

  buffer->window_offset       = -1;
  buffer->communicator        = MPI_COMM_NULL;
  buffer->server_communicator = MPI_COMM_NULL;
  buffer->window              = MPI_WIN_NULL;

  buffer->existence_timer.start      = 0;
  buffer->existence_timer.total_time = 0.0;

  buffer->raw_data                  = NULL;
  buffer->control_data              = NULL;
  buffer->server_bound_win_offsets  = NULL;
  buffer->client_bound_win_offsets  = NULL;
  buffer->server_bound_client_ranks = NULL;
  buffer->client_bound_client_ranks = NULL;
  buffer->channel_ranks             = NULL;
  buffer->channel_signals           = NULL;
}

//! Count the number of each process type (server-bound server, client-bound server, channel, server-bound client, client-bound client)
//! The number will be set in the metadata struct for the ROOT process only
static inline distributed_circular_buffer_p count_process_types(distributed_circular_buffer_p buffer) {
  const int32_t zero = 0;
  const int32_t one  = 1;

  if (buffer->server_communicator != MPI_COMM_NULL) // Server processes
  {
    MPI_Comm_rank(buffer->server_communicator, &buffer->server_rank);

    // Determine rank of the root of the DCB (it's the process with server rank #DCB_SERVER_ROOT_RANK)
    const int root_rank = (buffer->server_rank == DCB_SERVER_ROOT_RANK) ? buffer->dcb_rank : 0;
    MPI_Allreduce(&root_rank, &buffer->control_metadata.root_rank, 1, MPI_INT, MPI_SUM, buffer->communicator);

    if (buffer->communication_type == DCB_SERVER_BOUND_TYPE) {
      // printf("I am a server-bound server (%d)\n", buffer->dcb_rank);
      MPI_Reduce(&one,  &buffer->control_metadata.num_server_consumers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&zero, &buffer->control_metadata.num_server_producers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&zero, &buffer->control_metadata.num_channels,         1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
    }
    else if (buffer->communication_type == DCB_CLIENT_BOUND_TYPE) {
      // printf("I am a client-bound server (%d)\n", buffer->dcb_rank);
      MPI_Reduce(&zero, &buffer->control_metadata.num_server_consumers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&one,  &buffer->control_metadata.num_server_producers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&zero, &buffer->control_metadata.num_channels,         1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
    }
    else if (buffer->communication_type == DCB_CHANNEL_TYPE) {
      // printf("I am a channel (%d)\n", buffer->dcb_rank);
      MPI_Reduce(&zero, &buffer->control_metadata.num_server_consumers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&zero, &buffer->control_metadata.num_server_producers, 1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
      MPI_Reduce(&one,  &buffer->control_metadata.num_channels,         1, MPI_INT, MPI_SUM, DCB_SERVER_ROOT_RANK, buffer->server_communicator);
    }
    else {
      printf("ERROR during DCB_create. Server process given an invalid communication type\n");
      return NULL;
    }

    MPI_Reduce(&zero, &buffer->control_metadata.num_server_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);
    MPI_Reduce(&zero, &buffer->control_metadata.num_client_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);

    // printf("%d cons, %d prod, %d channels, %d sb, %d cb (rank %d)\n", 
    //   buffer->control_metadata.num_server_consumers, buffer->control_metadata.num_server_producers, buffer->control_metadata.num_channels,
    //   buffer->control_metadata.num_server_bound_instances, buffer->control_metadata.num_client_bound_instances,
    //   buffer->dcb_rank
    // );

    // Do some sanity checks
    if (buffer->server_rank == DCB_SERVER_ROOT_RANK) {

      printf("From DCB root, rank %d\n", buffer->dcb_rank);

      if (buffer->control_metadata.num_channels < 1 || (buffer->control_metadata.num_server_consumers + buffer->control_metadata.num_server_producers < 1)) {
        printf(
          "ERROR during DCB_create. We are missing some processes on the server. "
          "There are currently %d consumer(s), %d producer(s) and %d channel(s). "
          "We need at least one channel et one of either a consumer or a producer.\n",
          buffer->control_metadata.num_server_consumers, buffer->control_metadata.num_server_producers, buffer->control_metadata.num_channels
        );
        return NULL;
      }

      if (buffer->control_metadata.num_server_bound_instances > 0 && buffer->control_metadata.num_server_consumers <= 0) {
        printf("BIG WARNING during DCB_create. We have %d server-bound buffer instance(s), but no server consumer!\n",
               buffer->control_metadata.num_server_bound_instances);
        // return NULL;
      }

      if (buffer->control_metadata.num_client_bound_instances > 0 && buffer->control_metadata.num_server_producers <= 0) {
        printf("BIG WARNING during DCB_create. We have %d client-bound buffer instance(s), but no server producer!\n",
               buffer->control_metadata.num_client_bound_instances);
        // return NULL;
      }
    }

    // Verify that server processes are all located on the same node
    {
      MPI_Comm local_comm;
      MPI_Comm_split_type(buffer->server_communicator, MPI_COMM_TYPE_SHARED, buffer->server_rank, MPI_INFO_NULL, &local_comm);

      int local_size, server_size;
      MPI_Comm_size(local_comm, &local_size);
      MPI_Comm_size(buffer->server_communicator, &server_size);
      if (local_size != server_size) {
        printf("ERROR during DCB create: Server processes appear to be on multiple nodes\n");
        return NULL;
      }
    }
  }
  else // Client processes
  {
    // Retrieve rank of root process
    MPI_Allreduce(&zero, &buffer->control_metadata.root_rank, 1, MPI_INT, MPI_SUM, buffer->communicator);

    if (buffer->communication_type == DCB_SERVER_BOUND_TYPE) {
      // printf("I am a server-bound client (%d)\n", buffer->dcb_rank);
      MPI_Reduce(&one,  &buffer->control_metadata.num_server_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);
      MPI_Reduce(&zero, &buffer->control_metadata.num_client_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);
    }
    else if (buffer->communication_type == DCB_CLIENT_BOUND_TYPE) {
      // printf("I am a client-bound client (%d)\n", buffer->dcb_rank);
      MPI_Reduce(&zero, &buffer->control_metadata.num_server_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);
      MPI_Reduce(&one,  &buffer->control_metadata.num_client_bound_instances, 1, MPI_INT, MPI_SUM, buffer->control_metadata.root_rank, buffer->communicator);
    }
    else {
      printf("ERROR during DCB_create. Invalid communication type given by a client process (%d)\n.", buffer->communication_type);
      return NULL;
    }
  }

  return buffer;
}

//! Assign an ID to every process that participates in the given DCB. There is a set of IDs for server consumers (server-bound servers),
//! one for server producers (client-bound servers), one for channels, one for server-bound clients, and one for client-bound clients
//! Each set starts at zero.
static inline distributed_circular_buffer_p assign_process_ids(distributed_circular_buffer_p buffer) {
  if (buffer->server_communicator == MPI_COMM_NULL) { // Client processes
    MPI_Comm new_comm;
    if (buffer->communication_type == DCB_SERVER_BOUND_TYPE) {
      MPI_Comm_split(buffer->communicator, 1, 0, &new_comm);
      MPI_Comm_rank(new_comm, &buffer->server_bound_client_id);
    }
    else {
      MPI_Comm_split(buffer->communicator, 2, 0, &new_comm);
      MPI_Comm_rank(new_comm, &buffer->client_bound_client_id);
    }
  }
  else { // Server processes
    MPI_Comm new_comm;
    MPI_Comm_split(buffer->communicator, 0, 0, &new_comm);

    if (buffer->server_rank < 0) {
      printf("WE HAVE A PROBLEM!!!\n");
      return NULL;
    }

    MPI_Comm_split(buffer->server_communicator, buffer->communication_type, buffer->server_rank, &new_comm);
    if (buffer->communication_type == DCB_SERVER_BOUND_TYPE)
      MPI_Comm_rank(new_comm, &buffer->server_bound_server_id);
    else if (buffer->communication_type == DCB_CLIENT_BOUND_TYPE)
      MPI_Comm_rank(new_comm, &buffer->client_bound_server_id);
    else
      MPI_Comm_rank(new_comm, &buffer->channel_id);
  }

  return buffer;
}

static inline void init_metadata(distributed_circular_buffer_p buffer, const size_t num_bytes_server_bound, const size_t num_bytes_client_bound) {
  const size_t num_elements_server_bound = num_bytes_to_num_elem(num_bytes_server_bound);
  const size_t num_elements_client_bound = num_bytes_to_num_elem(num_bytes_client_bound);

  if (is_root(buffer)) {
    init_control_metadata(
        &buffer->control_metadata, buffer->control_metadata.num_server_bound_instances,
        buffer->control_metadata.num_client_bound_instances, buffer->control_metadata.num_channels,
        num_elements_server_bound, num_elements_client_bound);
  }

  // Every process that participate in the DCB has an identical copy of the control metadata
  MPI_Bcast(&buffer->control_metadata, sizeof(control_header), MPI_BYTE, buffer->control_metadata.root_rank, buffer->communicator);
}

//! Allocate shared memory for the given DCB, initialize (most of) it and create the MPI one-sided window into
//! that shared memory. The initialization includes initiliazing the individual CB instances that reside on the
//! server.
static inline distributed_circular_buffer_p init_shmem_area_and_window(distributed_circular_buffer_p buffer) {
  const MPI_Aint win_total_size = total_window_num_elem(
      buffer->control_metadata.num_server_bound_instances, buffer->control_metadata.num_client_bound_instances,
      buffer->control_metadata.num_channels, buffer->control_metadata.num_elem_per_server_bound_instance,
      buffer->control_metadata.num_elem_per_client_bound_instance) * (MPI_Aint)sizeof(data_element);

  // Allocated shared memory area and send shmem info to other server processes
  if (is_root(buffer))
  {
    int id           = -1;
    // printf("Allocating %zu shmem bytes\n", win_total_size);
    buffer->raw_data = shmem_allocate_shared(&id, win_total_size);
    if (buffer->raw_data == NULL) {
      printf("Error when allocating shared memory for DCB\n");
      return NULL;
    }

    init_control_pointers(buffer); // Need the control pointers to initialize the data
    init_control_data(buffer); // Initialize control data before syncing with other server processes (this removes 1 barrier)
    MPI_Bcast(&id, 1, MPI_INT, DCB_SERVER_ROOT_RANK, buffer->server_communicator); // Send shared mem info to other server procs
  }
  else if (is_on_server(buffer))
  {
    int id;
    MPI_Bcast(&id, 1, MPI_INT, DCB_SERVER_ROOT_RANK, buffer->server_communicator); // Receive shared mem info from root process
    buffer->raw_data = shmem_address_from_id(id);
  }

  if (is_on_server(buffer)) {
    init_control_pointers(buffer);

    if (is_channel(buffer)) {
      buffer->channel_ranks[buffer->channel_id] = buffer->dcb_rank;
    }

    // Make sure channel ranks are properly set in the shared memory region before proceeding with initialization by root process
    MPI_Barrier(buffer->server_communicator);
  }

  // Root only: initialize the individual CB instances
  if (is_root(buffer)) {
    // Compute number of elements that fit in individual buffers (excludes the space taken by some headers)
    const int num_elem_in_server_bound_cb =
        total_circular_buffer_instance_size(buffer->control_metadata.num_elem_per_server_bound_instance) - instance_header_size() + circular_buffer_header_size();
    const int num_elem_in_client_bound_cb =
        total_circular_buffer_instance_size(buffer->control_metadata.num_elem_per_client_bound_instance) - instance_header_size() + circular_buffer_header_size();

    // Initialize the individual buffers
    for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i, DCB_SERVER_BOUND_TYPE);
      init_circular_buffer_instance(buffer_instance, i, num_elem_in_server_bound_cb);
      assign_target_channel(buffer, buffer_instance, i % buffer->control_metadata.num_channels);
    }

    for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {
      circular_buffer_instance_p buffer_instance = get_circular_buffer_instance(buffer, i, DCB_CLIENT_BOUND_TYPE);
      init_circular_buffer_instance(buffer_instance, i, num_elem_in_client_bound_cb);
      assign_target_channel(buffer, buffer_instance, i % buffer->control_metadata.num_channels);
    }
  }

  // Create the actual window that will be used for data transmission, using a pointer to the same shared memory on the server
  const MPI_Aint final_win_size = is_client(buffer) ? 0 : win_total_size; // Size used for actual window creation
  // printf("Creating window of size %ld (rank %d)\n", final_win_size, buffer->dcb_rank);
  MPI_Win_create(
      buffer->raw_data, final_win_size, sizeof(data_element), MPI_INFO_NULL, buffer->communicator, &buffer->window);

  return buffer;
}

//! @}

//! @{ \name Distributed circular buffer public interface

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
//! Some of the processes are server-side and hold the data for a circular buffer instance for each other process,
//! in a shared memory window.
//! Some processes are channels and are only there to serve as MPI communication targets and do no actual work.
//!  _It is the caller's responsibility to ensure that all consumer and channel processes are located on the same
//! physical node, which we call the "server node"._
//! The other processes are clients and only get a copy of the header of their circular buffer, as well as an offset
//! that points to the location of their data within the shared window.
//! _The buffer with rank #DCB_SERVER_ROOT_ID on the given server DCB communicator will be considered the root of the DCB._
//! \sa distributed_circular_buffer
//!
//! @return If all went well, a pointer to a newly-allocated distributed circular buffer struct that contains all the
//! relevant info. If there was an error, returns NULL
//C_StArT
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,           //!< [in] Communicator on which the distributed buffer is shared
    MPI_Comm      server_communicator,    //!< [in] Communicator that groups server processes (should be MPI_COMM_NULL on client processes)
    const int32_t communication_type,     //!< [in] Communication type of the calling process (server-bound, client-bound or channel, DCB_*_TYPE)
    const size_t  num_bytes_server_bound, //!< [in] Number of bytes in a single server-bound circular buffer (only needed on the root process)
    const size_t  num_bytes_client_bound, //!< [in] Number of bytes in a single client-bound circular buffer (only needed on the root process)
    const int     verbose                 //!< [in] Whether to print some DCB info (if == 1)
    )
//C_EnD
{
  distributed_circular_buffer_p buffer = (distributed_circular_buffer*)malloc(sizeof(distributed_circular_buffer));

  reset_dcb_struct(buffer);

  // Put default values everywhere
  buffer->communicator        = communicator;
  buffer->server_communicator = server_communicator;
  buffer->communication_type  = communication_type;

  MPI_Comm_rank(buffer->communicator, &buffer->dcb_rank);

  buffer = count_process_types(buffer);
  if (buffer == NULL) return NULL;

  buffer = assign_process_ids(buffer);
  if (buffer == NULL) return NULL;

  init_metadata(buffer, num_bytes_server_bound, num_bytes_client_bound);

  buffer = init_shmem_area_and_window(buffer);
  if (buffer == NULL) return NULL;
  
  // Clients: retrieve their assigned initialized CB instance and send their rank to the server
  if (is_client(buffer)) {
    retrieve_window_offset_from_remote(buffer); // Find out where in the window this instance is located
    update_local_header_from_remote(buffer, 1); // Get the header to sync the instance locally
    set_client_rank_on_remote(buffer);
  }

  // Wait until everyone (especially the clients, at this point) is properly initialized before allowing the use of the buffer
  MPI_Barrier(buffer->communicator);

  if (is_root(buffer) && verbose == 1)
    print_control_metadata(&buffer->control_metadata, buffer->control_data);

  if (DCB_check_integrity(buffer, 1) < 0) {
    printf("ERROR: just-created DCB is not consistent!!!!\n");
    return NULL;
  }

  IO_timer_start(&buffer->existence_timer);

  return buffer;
}

//F_StArT
//  function DCB_create(f_communicator, f_server_communicator, communication_type, num_bytes_server_bound, num_bytes_client_bound, verbose) result(p) BIND(C, name = 'DCB_create_f')
//    import :: C_PTR, C_INT, C_SIZE_T
//    implicit none
//    integer(C_INT),    intent(IN), value :: f_communicator         !< Communicator on which the distributed buffer is shared
//    integer(C_INT),    intent(IN), value :: f_server_communicator  !< Communicator that groups the server processes
//    integer(C_INT),    intent(IN), value :: communication_type     !< Communication type of the calling process (server-bound, client-bound or channel, DCB_*_TYPE)
//    integer(C_SIZE_T), intent(IN), value :: num_bytes_server_bound !< Number of bytes in a single server-bound circular buffer (only needed on the root process)
//    integer(C_SIZE_T), intent(IN), value :: num_bytes_client_bound !< Number of bytes in a single client-bound circular buffer (only needed on the root process)
//    integer(C_INT),    intent(IN), value :: verbose                !< Print some info when == 1
//    type(C_PTR) :: p                                       !< Pointer to created distributed circular buffer
//   end function DCB_create
//F_EnD
//! Wrapper function to call from Fortran code, with Fortran MPI communicators
distributed_circular_buffer_p DCB_create_f(
    int32_t f_communicator,         //!< [in] Communicator on which the distributed buffer is shared (in Fortran)
    int32_t f_server_communicator,  //!< [in] Communicator that groups server processes (in Fortran)
    int32_t communication_type,     //!< [in] Communication type of the calling process (server-bound, client-bound or channel, DCB_*_TYPE)
    size_t  num_bytes_server_bound, //!< [in] Number of bytes in a single server-bound circular buffer (only needed on the root process)
    size_t  num_bytes_client_bound, //!< [in] Number of bytes in a single client-bound circular buffer (only needed on the root process)
    int     verbose                 //!< [in] Print some info when == 1
) {
  return DCB_create(
      MPI_Comm_f2c(f_communicator), MPI_Comm_f2c(f_server_communicator), communication_type, num_bytes_server_bound, num_bytes_client_bound, verbose);
}

//F_StArT
//  subroutine DCB_print(buffer, dump_data) BIND(C, name = 'DCB_print')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR),    INTENT(IN), value :: buffer    !< Buffer for which to print data
//    integer(C_INT), INTENT(IN), value :: dump_data !< Whether to print buffer content
//  end subroutine DCB_print
//F_EnD
//! Print some debug info
void DCB_print(
  distributed_circular_buffer_p buffer, //!< [in] Buffer for which to print data
  int32_t dump_data                     //!< [in] Whether to print content too
) {
  printf(
      "--------------------------------\n"
      "Server-bound server ID: %d\n"
      "Client-bound server ID: %d\n"
      "Server-bound client ID: %d\n"
      "Client-bound client ID: %d\n"
      "Channel ID:             %d\n"
      "Server rank:            %d\n"
      "Current time:           %.2f ms\n"
      "Window offset:          %ld\n",
      buffer->server_bound_server_id, buffer->client_bound_server_id,
      buffer->server_bound_client_id, buffer->client_bound_client_id, buffer->channel_id,
      buffer->server_rank, IO_time_since_start(&buffer->existence_timer), (long int)buffer->window_offset);
  if (is_client(buffer)) {
    print_instance(&buffer->local_header);
    if (dump_data == 1) {
      CB_dump_data(&buffer->local_header.circ_buffer);
    }
  }
  else if (is_root(buffer)) {
    print_control_metadata(&buffer->control_metadata, NULL);
    for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {
      const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, i, DCB_SERVER_BOUND_TYPE);
      printf("From root: buffer %d has %ld data in it\n", i, get_available_data_bytes(instance));
      if (dump_data == 1) {
        print_instance(instance);
        CB_dump_data(&instance->circ_buffer);
      }
    }
    for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {
      const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, i, DCB_CLIENT_BOUND_TYPE);
      printf("From root: buffer %d has %ld data in it\n", i, get_available_data_bytes(instance));
      if (dump_data == 1) {
        print_instance(instance);
        CB_dump_data(&instance->circ_buffer);
      }
    }
  }
  printf("------------------------------\n");
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

  IO_timer_stop(&buffer->existence_timer);

  // Producers must send their collected stats to the root
  if (is_client(buffer)) {
    const int id = is_server_bound_client(buffer) ? buffer->server_bound_client_id : buffer->client_bound_client_id;
    MPI_Send(&buffer->local_header.circ_buffer.stats, sizeof(cb_stats), MPI_BYTE, buffer->control_metadata.root_rank, id, buffer->communicator);
  }

  // The root will print all stats, and compute some global stat values
  if (is_root(buffer)) {
    send_channel_signal(buffer, RSIG_STOP); // Tell channels to stop any activity
    uint64_t total_data_received = 0;
    uint64_t total_data_sent     = 0;
    printf(
        "------------------------------------------------------------------------------------\n"
        "DCB STATS\n"
        "Num server-bound server procs: %d\n"
        "Num client-bound server procs: %d\n"
        "Num channels: %d\n"
        "Num server-bound buffers: %d\n"
        "Num client-bound buffers: %d\n",
        buffer->control_metadata.num_server_consumers, buffer->control_metadata.num_server_producers,
        buffer->control_metadata.num_channels,
        buffer->control_metadata.num_server_bound_instances, buffer->control_metadata.num_client_bound_instances);

    for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {
      const circular_buffer_instance_p instance   = get_circular_buffer_instance(buffer, i, DCB_SERVER_BOUND_TYPE);
      cb_stats_p                       full_stats = &instance->circ_buffer.stats;

      // Combine the stats from remote and server
      MPI_Status status;
      cb_stats   remote_stats;
      MPI_Recv(&remote_stats, sizeof(cb_stats), MPI_BYTE, buffer->server_bound_client_ranks[i], i, buffer->communicator, &status);
      full_stats->num_writes               = remote_stats.num_writes;
      full_stats->num_write_elems          = remote_stats.num_write_elems;
      full_stats->total_write_time_ms      = remote_stats.total_write_time_ms;
      full_stats->total_write_wait_time_ms = remote_stats.total_write_wait_time_ms;

      if (i == 0) {
        printf(" -----------------------------\n"
               "  Server-bound transfer stats  \n");
      }
      print_instance_stats(instance, i == 0);

      total_data_received += remote_stats.num_write_elems;
    }

    for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {
      const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, i, DCB_CLIENT_BOUND_TYPE);
      cb_stats_p full_stats = &instance->circ_buffer.stats;

      MPI_Status status;
      cb_stats   remote_stats;
      MPI_Recv(&remote_stats, sizeof(cb_stats), MPI_BYTE, buffer->client_bound_client_ranks[i], i, buffer->communicator, &status);
      full_stats->num_writes               = remote_stats.num_writes;
      full_stats->num_write_elems          = remote_stats.num_write_elems;
      full_stats->total_write_time_ms      = remote_stats.total_write_time_ms;
      full_stats->total_write_wait_time_ms = remote_stats.total_write_wait_time_ms;

      if (i == 0) {
        printf(" -----------------------------\n"
               "  Client-bound transfer stats  \n");
      }
      print_instance_stats(instance, i == 0);

      total_data_sent += remote_stats.num_write_elems;
    }

    const double existence_time = IO_time_ms(&buffer->existence_timer);
    char total_data_recv_s[8], dps_recv_s[8];
    char total_data_send_s[8], dps_send_s[8];
    readable_element_count((double)total_data_received * sizeof(data_element), total_data_recv_s);
    readable_element_count(total_data_received / existence_time * 1000.0 * sizeof(data_element), dps_recv_s);
    readable_element_count((double)total_data_sent * sizeof(data_element), total_data_send_s);
    readable_element_count(total_data_sent / existence_time * 1000.0 * sizeof(data_element), dps_send_s);
    printf(" -----------------------------\n");
    printf("Total data received: %sB (%sB/s)\n", total_data_recv_s, dps_recv_s);
    printf("Total data sent:     %sB (%sB/s)\n", total_data_send_s, dps_send_s);
    printf("------------------------------------------------------------------------------------\n");
  }

  // We can finally release the memory
  MPI_Win_free(&buffer->window);
  free(buffer);
}

//F_StArT
//  function DCB_get_available_data(buffer, buffer_id) result(num_bytes) BIND(C, name = 'DCB_get_available_data')
//    import :: C_PTR, C_INT, C_INT64_T
//    implicit none
//    type(C_PTR), intent(in), value    :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT64_T) :: num_bytes
//  end function DCB_get_available_data
//F_EnD
//! Check how many elements are stored in the specified buffer.
//! _Can only be called from a server consumer process._
//! @return The number of elements in the specified buffer, or -1 if called from a wrong process
//C_StArT
int64_t DCB_get_available_data(
    distributed_circular_buffer_p buffer,   //!< [in] DCB we are querying
    const int                     buffer_id //!< [in] Which specific buffer in the DCB
    )
//C_EnD
{
  if (buffer == NULL) return CB_ERROR_INVALID_POINTER;
  if (!is_server_bound_server(buffer)) return DCB_ERROR_WRONG_CALLER_ROLE;
  if (buffer_id < 0 || buffer_id >= buffer->control_metadata.num_server_bound_instances) return DCB_ERROR_INVALID_BUFFER_ID;

  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id, DCB_SERVER_BOUND_TYPE);
  if (instance == NULL) return CB_ERROR_INVALID_POINTER;
  return (int64_t)get_available_data_bytes(instance);
}

//F_StArT
//  function DCB_get_available_space(buffer, update_from_remote) result(num_spaces) BIND(C, name = 'DCB_get_available_space')
//    import :: C_PTR, C_INT, C_INT64_T
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT), intent(in), value :: update_from_remote
//    integer(C_INT64_T) :: num_spaces
//  end function DCB_get_available_space
//F_EnD
//C_StArT
//! Check how many bytes can still fit in the remaining space in the buffer. _Can only be called from a server-bound client process._
int64_t DCB_get_available_space(
    distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    int update_from_remote                //!< [in] Whether to look at the server to get the absolute latest num spaces
    )
//C_EnD
{
  if (!is_server_bound_client(buffer)) return DCB_ERROR_WRONG_CALLER_ROLE;

  if (update_from_remote == 1)
    return get_available_space_from_remote_bytes(buffer);
  else
    return get_available_space_bytes(&buffer->local_header);
}

//F_StArT
// function DCB_get_server_bound_client_id(buffer) result(server_bound_client_id) BIND(C, name = 'DCB_get_server_bound_client_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: server_bound_client_id
// end function DCB_get_server_bound_client_id
//F_EnD
int32_t DCB_get_server_bound_client_id(const distributed_circular_buffer_p buffer) {
  return buffer->server_bound_client_id;
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
// function DCB_get_server_bound_server_id(buffer) result(server_bound_server_id) BIND(C, name = 'DCB_get_server_bound_server_id')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: server_bound_server_id
// end function DCB_get_server_bound_server_id
//F_EnD
int32_t DCB_get_server_bound_server_id(const distributed_circular_buffer_p buffer) {
  return buffer->server_bound_server_id;
}

//F_StArT
//  function DCB_get_num_server_bound_instances(buffer) result(num_instances) BIND(C, name = 'DCB_get_num_server_bound_instances')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_instances
//  end function DCB_get_num_server_bound_instances
//F_EnD
int32_t DCB_get_num_server_bound_instances(const distributed_circular_buffer_p buffer) {
  return buffer->control_metadata.num_server_bound_instances;
}

//F_StArT
//  function DCB_get_num_server_consumers(buffer) result(num_consumers) BIND(C, name = 'DCB_get_num_server_consumers')
//    import :: C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT) :: num_consumers
//  end function DCB_get_num_server_consumers
//F_EnD
int32_t DCB_get_num_server_consumers(const distributed_circular_buffer_p buffer) {
  return buffer->control_metadata.num_server_consumers;
}

//F_StArT
//  function DCB_get_capacity_local(buffer) result(capacity_bytes) BIND(C, name = 'DCB_get_capacity_local')
//    import :: C_PTR, C_INT64_T
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    integer(C_INT64_T) :: capacity_bytes
//  end function DCB_get_capacity_local
//F_EnD
//C_StArT
int64_t DCB_get_capacity_local(const distributed_circular_buffer_p buffer
)
//C_EnD
{
  if (!is_client(buffer)) return DCB_ERROR_WRONG_CALLER_ROLE;
  return buffer->local_header.capacity;
}

//F_StArT
//  function DCB_get_capacity_server(buffer, buffer_id) result(capacity_bytes) BIND(C, name = 'DCB_get_capacity_server')
//    import :: C_PTR, C_INT, C_INT64_T
//    implicit none
//    type(C_PTR),    intent(in), value :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT64_T) :: capacity_bytes
//  end function DCB_get_capacity_server
//F_EnD
//! Must be called from a server process.
//! @return Capacity of the specified buffer, or -1 if we are not a server process
int64_t DCB_get_capacity_server(const distributed_circular_buffer_p buffer, int buffer_id) {

  if (buffer_id < 0) return DCB_ERROR_INVALID_BUFFER_ID;

  if (is_server_bound_server(buffer)) {
    if (buffer_id >= buffer->control_metadata.num_server_bound_instances) return DCB_ERROR_INVALID_BUFFER_ID;
    return get_circular_buffer_instance(buffer, buffer_id, DCB_SERVER_BOUND_TYPE)->capacity;
  }
  else if (is_client_bound_server(buffer)) {
    if (buffer_id >= buffer->control_metadata.num_client_bound_instances) return DCB_ERROR_INVALID_BUFFER_ID;
    return get_circular_buffer_instance(buffer, buffer_id, DCB_CLIENT_BOUND_TYPE)->capacity;
  }
  
  return DCB_ERROR_WRONG_CALLER_ROLE;
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
    return DCB_ERROR_WRONG_CALLER_ROLE;

  volatile channel_signal_t* signal = buffer->channel_signals + buffer->channel_id;

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
    case RSIG_STOP: return CB_SUCCESS;
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

  if (is_on_server(buffer))
    MPI_Barrier(buffer->server_communicator);
}

//F_StArT
//  function DCB_put_client(buffer, src_data, num_bytes, operation, timeout_ms) result(status) BIND(C, name = 'DCB_put_client')
//    import :: C_PTR, C_INT, C_SIZE_T
//    implicit none
//    type(C_PTR),       intent(in), value :: buffer      !< Buffer where we want to insert data
//    type(C_PTR),       intent(in), value :: src_data    !< Data to insert
//    integer(C_SIZE_T), intent(in), value :: num_bytes   !< How many data elements we want to insert
//    integer(C_INT),    intent(in), value :: operation   !< Whether to commit the transaction or wait
//    integer(C_INT),    intent(in), value :: timeout_ms  !< How long (in ms) we should wait before declaring failure, forever if negative
//    integer(C_INT) :: status !< CB_SUCCESS if success, a negative error code if failure
//  end function DCB_put_client
//F_EnD
//! @brief Insert data into the given buffer, once there is enough space (will wait if there isn't enough initially)
//!
//! The data will be inserted into the buffer instance associated with the calling process.
//! We use the MPI_Accumulate function along with the MPI_REPLACE operation, rather than MPI_Put, to ensure the order
//! in which the memory transfers are done. We need to have finished the transfer of the data itself before updating
//! the insertion pointer on the root node (otherwise it might try to read data that have not yet arrived).
//! An operation must be specified. The options for now are either COMMIT or NO_COMMIT. COMMIT will make the data
//! available for the consumer to read, whereas NO_COMMIT will _send_ the data but _not_ make it available.
//! _With NO_COMMIT, the data is still sent through MPI_.
//!
//! _Can only be called from a server-bound client process._
//!
//! @return CB_SUCCESS if everything went smoothly, a negative error code otherwise
//C_StArT
int DCB_put_client(
    distributed_circular_buffer_p buffer,    //!< [in,out] Distributed buffer in which we want to put data
    void* const                   src_data,  //!< [in] Pointer to the data we want to insert
    const size_t                  num_bytes, //!< [in] How many bytes we want to insert
    const int                     operation, //!< [in] What operation to perform (whether to commit the transaction)
    const int                     timeout_ms //!< [in] How long (in ms) we should wait before declaring failure
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  IO_timer_start(&timer);

  const size_t  num_elements = num_bytes_to_num_elem(num_bytes);
  const int64_t num_spaces   = DCB_wait_space_available_client(buffer, num_elements * sizeof(data_element), timeout_ms);
  if (num_spaces < 0)
    return num_spaces;

  const int target_rank = buffer->local_header.target_rank;

  // NOTE: We could in theory use the MPI_MODE_NOCHECK flag, but it does not work with OpenMPI 4.0.5 on large data transfers
  MPI_Win_lock(MPI_LOCK_SHARED, target_rank, 0, buffer->window);

  uint64_t       in_index = buffer->local_header.circ_buffer.m.in[CB_PARTIAL];
  const uint64_t limit    = buffer->local_header.circ_buffer.m.limit;

  // First segment
  const size_t num_elem_segment_1 = num_elements > (limit - in_index) ? (limit - in_index) : num_elements;
  const size_t num_bytes_1        = num_elem_segment_1 < num_elements ? num_elem_segment_1 * sizeof(data_element) : num_bytes;
  {
    const int fractional = num_bytes_1 != num_elem_segment_1 * sizeof(data_element);

    const int          num_to_send  = fractional ? num_bytes_1 : num_elem_segment_1;
    const MPI_Datatype type_to_send = fractional ? MPI_BYTE : CB_MPI_ELEMENT_TYPE;

    MPI_Accumulate(
        src_data, num_to_send, type_to_send, target_rank, buffer_element_displacement(buffer, in_index),
        num_to_send, type_to_send, MPI_REPLACE, buffer->window);
  }

  // Update temporary insertion pointer
  in_index += num_elem_segment_1;
  if (in_index >= limit)
    in_index = 0;

  // Second segment (if there is one)
  if (num_elements > num_elem_segment_1) {
    const size_t num_elem_segment_2 = num_elements - num_elem_segment_1;
    const size_t num_bytes_2        = num_bytes - num_bytes_1;
    const int    fractional         = num_bytes_2 != num_elem_segment_2 * sizeof(data_element);

    const int          num_to_send  = fractional ? num_bytes_2 : num_elem_segment_2;
    const MPI_Datatype type_to_send = fractional ? MPI_BYTE : CB_MPI_ELEMENT_TYPE;

    MPI_Accumulate(
        (char*)src_data + (num_elem_segment_1 * sizeof(data_element)), num_to_send, type_to_send, target_rank,
        buffer_element_displacement(buffer, in_index), num_to_send, type_to_send, MPI_REPLACE,
        buffer->window);

    in_index += num_elem_segment_2;
  }

  // Update partial insertion pointer (locally only)
  buffer->local_header.circ_buffer.m.in[CB_PARTIAL] = in_index;

  // Update insertion index remotely and locally
  if (operation == CB_COMMIT) {
    buffer->local_header.circ_buffer.m.in[CB_FULL] = in_index;
    MPI_Accumulate(
        buffer->local_header.circ_buffer.m.in, 2, MPI_LONG_LONG_INT, target_rank,
        insertion_index_displacement(buffer), 2, MPI_LONG_LONG_INT, MPI_REPLACE, buffer->window);
  }

  MPI_Win_unlock(target_rank, buffer->window);

  IO_timer_stop(&timer);

  buffer->local_header.circ_buffer.stats.num_write_elems += num_elements;
  buffer->local_header.circ_buffer.stats.num_writes++;
  buffer->local_header.circ_buffer.stats.total_write_time_ms += IO_time_ms(&timer);

  if (num_bytes != num_elements * sizeof(data_element)) buffer->local_header.circ_buffer.stats.num_fractional_writes++;

  return CB_SUCCESS;
}

//F_StArT
//  function DCB_get_server(buffer, buffer_id, dest_data, num_bytes, operation, timeout_ms) result(status) BIND(C, name = 'DCB_get_server')
//    import :: C_PTR, C_INT, C_SIZE_T
//    implicit none
//    type(C_PTR),       intent(in), value :: buffer      !< DCB from which we want to read
//    integer(C_INT),    intent(in), value :: buffer_id   !< Which buffer in the DCB we want to read from
//    type(C_PTR),       intent(in), value :: dest_data   !< Where to put the data from the buffer
//    integer(C_SIZE_T), intent(in), value :: num_bytes   !< How many bytes to read
//    integer(C_INT),    intent(in), value :: operation   !< Whether to actually extract, read or just peek at the data
//    integer(C_INT),    intent(in), value :: timeout_ms  !< How long (in ms) to wait before declaring failure. Forever, if negative
//    integer(C_INT) :: status  !< CB_SUCCESS on success, a negative error code on failure
//  end function DCB_get_server
//F_EnD
//C_StArT
//! Read data from the specified buffer. This operation does not perform any MPI communication.
//! 
//! 3 operations are possible: extract (CB_COMMIT) will read the data and make the space available for the client
//! to insert more into the buffer, read (CB_NO_COMMIT) will read the data but will _not_ remove it from the buffer,
//! and peek (CB_PEEK) will have a look at the data, but that data will still be available to read afterwards. 
//!
//! _Can only be called from a server-bound server process._
//!
//! @return CB_SUCCESS on success, a negative error code on error
int DCB_get_server(
    distributed_circular_buffer_p buffer,    //!< [in,out] DCB from which we want to read
    const int                     buffer_id, //!< [in] Specific buffer in the DCB
    void*                         dest_data, //!< [in] Where to put the data from the buffer
    const size_t                  num_bytes, //!< [in] How many bytes to read
    const int                     operation, //!< [in] What operation to perform: extract, read or just peek
    const int                     timeout_ms //!< [in] How long (in ms) to wait before declaring failure. Forever, if negative
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  IO_timer_start(&timer);

  const size_t  num_elements       = num_bytes_to_num_elem(num_bytes);
  const int64_t num_available_elem = DCB_wait_data_available_server(buffer, buffer_id, num_elements * sizeof(data_element), timeout_ms);
  if (num_available_elem < 0)
    return num_available_elem;

  circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id, DCB_SERVER_BOUND_TYPE);

  // Update "max fill" metric
  if (instance->circ_buffer.stats.max_fill < (uint64_t)num_available_elem)
    instance->circ_buffer.stats.max_fill = num_available_elem;

  // Retrieve indices/pointers
  uint64_t                  out_index   = instance->circ_buffer.m.out[CB_PARTIAL];
  const uint64_t            limit       = instance->circ_buffer.m.limit;
  const data_element* const buffer_data = instance->circ_buffer.data;

  // 1st segment
  const size_t num_elem_segment_1 = num_elements > (limit - out_index) ? (limit - out_index) : num_elements;
  const size_t num_bytes_1        = num_elem_segment_1 < num_elements ? num_elem_segment_1 * sizeof(data_element) : num_bytes;
  copy_bytes(dest_data, (void*)(buffer_data + out_index), num_bytes_1);

  // Update temporary extraction pointer
  out_index += num_elem_segment_1;
  if (out_index >= limit)
    out_index = 0;

  // 2nd segment (if there is one)
  if (num_elem_segment_1 < num_elements) {
    const size_t num_elem_segment_2 = num_elements - num_elem_segment_1;
    const size_t num_bytes_2        = num_bytes - num_bytes_1;
    copy_bytes((char*)dest_data + (num_bytes_1), (void*)(buffer_data + out_index), num_bytes_2);

    out_index += num_elem_segment_2;
  }

  // Update partial extraction pointer if needed
  if (operation != CB_PEEK) {
    instance->circ_buffer.m.out[CB_PARTIAL] = out_index;
  }

  // Update full extraction pointer if needed
  if (operation == CB_COMMIT) {
    full_memory_fence(); // Make sure everything has been read, and the temp pointer actually updated

    volatile uint64_t* d_out = &instance->circ_buffer.m.out[CB_FULL];
    *d_out                   = out_index; // Update actual extraction pointer
  }

  IO_timer_stop(&timer);
  instance->circ_buffer.stats.total_read_time_ms += IO_time_ms(&timer);
  if (operation != CB_PEEK) {
    instance->circ_buffer.stats.num_read_elems += num_elements;
    instance->circ_buffer.stats.num_unique_reads++;
  }

  instance->circ_buffer.stats.num_reads++;

  if (num_bytes != num_elements * sizeof(data_element)) instance->circ_buffer.stats.num_fractional_reads++;

  return CB_SUCCESS;
}

/**
 * @brief Check the integrity of a single CB instance within the given DCB _(only valid for server processes)_
 * @return CB_SUCCESS if the check is successful, a negative error code otherwise
 */
int DCB_check_instance_integrity(
    const distributed_circular_buffer_p buffer,     //!< [in] The DCB we want to check
    const int                           buffer_id,  //!< [in] The ID of the CB we want to check within the DCB
    const int communication_type //!< [in] Whether we are looking for a server- or client-bound CB
) {
  const circular_buffer_instance_p instance = get_circular_buffer_instance(buffer, buffer_id, communication_type);
  return check_instance_consistency(instance, 1);
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
    if (verbose) printf("Buffer pointer is NULL!\n");
    return CB_ERROR_INVALID_POINTER;
  }

  if ((buffer->server_bound_server_id >= 0) + (buffer->client_bound_server_id >= 0) + (buffer->channel_id >= 0) +
      (buffer->server_bound_client_id >= 0) + (buffer->client_bound_client_id >= 0) != 1) {
    if (verbose) printf("Inconsistency in DCB IDs\n");
    return DCB_ERROR_WRONG_CALLER_ROLE;
  }

  if (is_client(buffer)) {
    const int status = check_instance_consistency(&buffer->local_header, verbose);
    if (status != CB_SUCCESS) {
      if (verbose) printf("Local instance %d/%d failed integrity check!\n", buffer->server_bound_client_id, buffer->client_bound_client_id);
      return status;
    }
  }
  else if (is_server_bound_server(buffer)) {
    for (int i = 0; i < buffer->control_metadata.num_server_bound_instances; ++i) {

      // Check whether the limit of this CB points to the beginning of the next CB
      if (i < buffer->control_metadata.num_server_bound_instances - 1) {
        const circular_buffer_instance_p current_instance = get_circular_buffer_instance(buffer, i, DCB_SERVER_BOUND_TYPE);
        const circular_buffer_instance_p next_instance    = get_circular_buffer_instance(buffer, i+1, DCB_SERVER_BOUND_TYPE);
        const data_element* end_of_buffer = current_instance->circ_buffer.data + current_instance->circ_buffer.m.limit;

        if ((void*)end_of_buffer != (void*)next_instance) {
          printf("AAAHHHHhhh end of CB %d (0x%.16lx) does not match with start of CB %d (0x%.16lx)\n", i, (uint64_t)end_of_buffer, i + 1, (uint64_t)next_instance);
          return CB_ERROR;
        }
      }

      // Check consistency of that particular buffer
      const int status = DCB_check_instance_integrity(buffer, i, DCB_SERVER_BOUND_TYPE);
      if (status != CB_SUCCESS)
        return status;
    }
  }
  else if (is_client_bound_server(buffer)) {
    for (int i = 0; i < buffer->control_metadata.num_client_bound_instances; ++i) {

      // Check whether the limit of this CB points to the beginning of the next CB
      if (i < buffer->control_metadata.num_client_bound_instances - 1) {
        const circular_buffer_instance_p current_instance = get_circular_buffer_instance(buffer, i, DCB_CLIENT_BOUND_TYPE);
        const circular_buffer_instance_p next_instance    = get_circular_buffer_instance(buffer, i+1, DCB_CLIENT_BOUND_TYPE);
        const data_element* end_of_buffer = current_instance->circ_buffer.data + current_instance->circ_buffer.m.limit;

        if ((void*)end_of_buffer != (void*)next_instance) {
          printf("AAAHHHHhhh end of CB %d (%ld) does not match with start of CB %d (%ld)\n", i, (uint64_t)end_of_buffer, i + 1, (uint64_t)next_instance);
          return CB_ERROR;
        }
      }

      // Check consistency of that particular buffer
      const int status = DCB_check_instance_integrity(buffer, i, DCB_CLIENT_BOUND_TYPE);
      if (status != CB_SUCCESS)
        return status;
    }
  }

  return CB_SUCCESS;
}

//! @}
//F_StArT
//  end interface
//F_EnD
