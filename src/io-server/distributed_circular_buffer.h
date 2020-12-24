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
 * 
 * Authors:
 *     M. Valin,   Recherche en Prevision Numerique, 2020
 *     V. Magnoux, Recherche en Prevision Numerique, 2020
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
  int32_t    rank; //!< Rank of the process that initialized this instance of the distributed buffer description
  int32_t    num_producers; //!< How many producer processes share this distributed buffer set
  data_index window_offset; //!< Offset into the MPI window at which this producer's circular buffer is located
  int32_t    num_element_per_instance; //!< How many elements form a single circular buffer instance in this buffer set

  MPI_Comm communicator; //!< Communicator through which the processes sharing the distributed buffer set communicate
  MPI_Win  window;       //!< MPI window into the circular buffers themselves, on the process which holds all data
  MPI_Win  window_mem_dummy; //!< MPI window used only to allocate and free shared memory

  //! Pointer to the data holding the entire set of circular buffers (only valid for the consumers)
  //! Will have some metadata at the beginning
  data_element* raw_data;

//  DCB_stats  producer_stats;
//  MPI_Win    consumer_stats_window;
//  DCB_stats* consumer_stats;

  //! Header of the circular buffer instance (only valid for producers)
  //! This is the local copy and will be synchronized with the remote one, located in the shared memory region of the
  //! consumer processes
  circular_buffer_instance local_header;

} distributed_circular_buffer;

typedef distributed_circular_buffer* distributed_circular_buffer_p;
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p);
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,  //!< [in] Communicator on which the distributed buffer is shared
    const int32_t num_producers, //!< [in] Number of producer processes in the communicator (number of buffer instances)
    const int32_t num_elements   //!< [in] Number of elems in a single circular buffer (only needed on the root process)
);
int32_t DCB_get_num_elements(
    distributed_circular_buffer_p buffer,   //!< [in]
    const int                     buffer_id //!< [in]
);
data_index DCB_put(
    distributed_circular_buffer_p buffer,      //!< Distributed buffer in which we want to put data
    data_element* const           src_data,    //!< Pointer to the data we want to insert
    const int                     num_elements //!< How many 4-byte elements we want to insert
);

#endif // IO_SERVER_distributed_circular_buffer_GEN_H
