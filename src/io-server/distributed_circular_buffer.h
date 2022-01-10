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
// This file has been generated from distributed_circular_buffer.c
#ifndef IO_SERVER_distributed_circular_buffer_GEN_H
#define IO_SERVER_distributed_circular_buffer_GEN_H

/******************************************************************************
         INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
   --------------------------------------------------------------------------
 We use the '//C_StArT' and '//C_EnD' tags to indicate the beginning and end of
 extraction.
 To extract the entire function (for inline functions, for example), you must
 put the begin/end tags around the entire function code, and **MAKE SURE TO
 LEAVE A SPACE** between the closing parenthesis of the function header and the
 opening bracket of the function body. They have to be on the same line.

 For example:
     //C_StArT
     inline int my_function(type1 arg1, type2* arg2) {
         [function body]
     }
     //C_EnD

 or also:
     //C_StArT
     inline int my_function(type1 arg1, //!< Doxygen doc
                            type2* arg2 //!< More doc
       ) {
         [function body]
     }
     //C_EnD


 To extract the function interface only, you must put the begin/end tags around
 the header. The closing parenthesis/opening bracket must either
  - be on the same line, without a space between them
or
  - be on different lines, with the closing parenthesis by itself on a line

 For example:
     //C_StArT
     int my_function(
         type1 arg1, //! Doxygen doc
         type2* arg2 //! Moar doc
         )
     //C_EnD
     {
         [function body]
     }

 or also:
     //C_StArT
     int my_function(type1 arg1, type2* arg2){
     //C_EnD
         [function body]
     }
 ******************************************************************************/

#include <mpi.h>

#include "io-server/circular_buffer.h"
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
void DCB_delete(distributed_circular_buffer_p);
void DCB_print(distributed_circular_buffer_p, int32_t);
void DCB_full_barrier(distributed_circular_buffer_p buffer);
int  DCB_check_integrity(const distributed_circular_buffer_p buffer, int verbose);
distributed_circular_buffer_p DCB_create(
    MPI_Comm      communicator,           //!< [in] Communicator on which the distributed buffer is shared
    MPI_Comm      server_communicator,    //!< [in] Communicator that groups server processes (should be MPI_COMM_NULL on client processes)
    const int32_t communication_type,     //!< [in] Communication type of the calling process (server-bound, client-bound or channel, DCB_*_TYPE)
    const size_t  num_bytes_server_bound, //!< [in] Number of bytes in a single server-bound circular buffer (only needed on the root process)
    const size_t  num_bytes_client_bound  //!< [in] Number of bytes in a single client-bound circular buffer (only needed on the root process)
);
int64_t DCB_get_available_data(
    distributed_circular_buffer_p buffer,   //!< [in] DCB we are querying
    const int                     buffer_id //!< [in] Which specific buffer in the DCB
);
//! Check how many bytes can still fit in the remaining space in the buffer. _Can only be called from a server-bound client process._
int64_t DCB_get_available_space(
    distributed_circular_buffer_p buffer, //!< [in] DCB we are querying
    int update_from_remote                //!< [in] Whether to look at the server to get the absolute latest num spaces
);
int64_t DCB_get_capacity_local(const distributed_circular_buffer_p buffer
);
int32_t DCB_channel_start_listening(distributed_circular_buffer_p buffer //!< [in]
);
int DCB_put_client(
    distributed_circular_buffer_p buffer,    //!< [in,out] Distributed buffer in which we want to put data
    void* const                   src_data,  //!< [in] Pointer to the data we want to insert
    const size_t                  num_bytes, //!< [in] How many bytes we want to insert
    const int                     operation  //!< [in] What operation to perform (whether to commit the transaction)
);
//! Read data from the specified buffer. This operation does not perform any MPI communication.
//! 
//! 3 operations are possible: extract (CB_COMMIT) will read the data and make the space available for the client
//! to insert more into the buffer, read (CB_NO_COMMIT) will read the data but will _not_ remove it from the buffer,
//! and peek (CB_PEEK) will have a look at the data, but that data will still be available to read afterwards. 
//!
//! _Can only be called from a server-bound server process._
//!
//! @return 0 on success, -1 on error
int DCB_get_server(
    distributed_circular_buffer_p buffer,    //!< [in,out] DCB from which we want to read
    const int                     buffer_id, //!< [in] Specific buffer in the DCB
    void*                         dest_data, //!< [in] Where to put the data from the buffer
    const size_t                  num_bytes, //!< [in] How many bytes to read
    const int                     operation  //!< [in] What operation to perform: extract, read or just peek
);

#endif // IO_SERVER_distributed_circular_buffer_GEN_H
