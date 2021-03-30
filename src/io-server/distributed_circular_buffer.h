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
// This file has been generated from distributed_circular_buffer.c
#ifndef IO_SERVER_distributed_circular_buffer_GEN_H
#define IO_SERVER_distributed_circular_buffer_GEN_H

#include <mpi.h>

#include "io-server/circular_buffer.h"
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
  int32_t    num_producers; //!< How many producer processes share this distributed buffer set
  int32_t    num_channels;  //!< How many channels can be used for MPI 1-sided communication (1 PE per channel)
  int32_t    num_consumers; //!< How many server processes will read from the individual buffers
  int32_t    num_element_per_instance; //!< How many elements form a single circular buffer instance in this buffer set
  data_index window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located

  int32_t channel_id;
  int32_t consumer_id;
  int32_t producer_id;

  int32_t server_rank;

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
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p);
void DCB_full_barrier(distributed_circular_buffer_p buffer);
int  DCB_check_integrity(const distributed_circular_buffer_p buffer, int verbose);
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,        //!< [in] Communicator on which the distributed buffer is shared
    MPI_Comm      server_communicator, //!< [in] Communicator that groups server processes
    const int32_t num_producers, //!< [in] Number of producer processes in the communicator (number of buffer instances)
    const int32_t num_channels,  //!< [in] Number of processes that can be the target of MPI 1-sided comm (channels)
    const int32_t num_elements   //!< [in] Number of elems in a single circular buffer (only needed on the root process)
);
int32_t DCB_get_num_elements(
    distributed_circular_buffer_p buffer,   //!< [in] DCB we are querying
    const int                     buffer_id //!< [in] Which specific buffer in the DCB
);
int32_t DCB_get_num_spaces(
    distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    int update_from_remote                //!< [in] Whether to look at the server to get the absolute latest num spaces
);
data_index DCB_put(
    distributed_circular_buffer_p buffer,       //!< [in,out] Distributed buffer in which we want to put data
    data_element* const           src_data,     //!< [in] Pointer to the data we want to insert
    const int                     num_elements, //!< [in] How many #data_element tokens we want to insert
    const int                     operation     //!< [in] What operation to perform (whether to commit the transaction)
);

#endif // IO_SERVER_distributed_circular_buffer_GEN_H
