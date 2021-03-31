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
// This file has been generated from circular_buffer.c
#ifndef IO_SERVER_circular_buffer_GEN_H
#define IO_SERVER_circular_buffer_GEN_H

/**
 \file
 \brief circular buffer package (C and Fortran)

 \verbatim
           circular buffer data layout

   (IN = OUT) (bufer empty) (LIMIT - FIRST -1 free slots)

 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   ........................................................
   ^------------------------------------------------------+
   |
 IN/OUT
   +------------------------------------------------------+
   ........................................................
   +--------------------^---------------------------------+
                        |
                      IN/OUT

   (IN = OUT - 1) (buffer full)

 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   +-------------------^^---------------------------------+
                       ||
                     IN  OUT
   +------------------------------------------------------+
   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
   ^------------------------------------------------------^
   |                                                      |
  OUT                                                     IN

   (OUT < IN) (LIMIT - IN -1) free, (IN - OUT) data
 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxx..........................................
   ^-------------^----------------------------------------+
   |             |
  OUT            IN

   (IN < OUT) (OUT - IN -1) free, (LIMIT - OUT + IN - FIRST) data
 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxx................................xxxxxxxxxx
   +-------------^-------------------------------^--------+
                 |                               |
                 IN                             OUT
   x = useful data       . = free space


      With partial insertion/extraction

  avail. data            available space           avail. data
   ...________          _________________         ____...
              |        |                 |       |
   +------------------------------------------------------+
   xxxxxxxxxxxx::::::::...................;;;;;;;xxxxxxxx
   +-----------^-------^------------------^------^--------+
               |       |                  |      |
               IN      |                 OUT     |
                  PARTIAL_IN                PARTIAL_OUT

   x = useful data    : = partially inserted data
   . = free space     ; = partially extracted data

 \endverbatim
*/
#include <stdlib.h>
#include <string.h>

#include "io-server/circular_buffer_defines.h"
#include "io-server/common.h"
#include "io-server/timer.h"

//!> version marker
#define FIOL_VERSION 0x1BAD

static const int MIN_CIRC_BUFFER_SIZE = 128; //!> Minimum size of a circular buffer, in number of #data_element

//!> circular buffer management variables
//!> <br>in == out means buffer is empty
//!> <br>in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct {
  data_element version; //!< version marker
  data_index   first;   //!< should be 0 (assumed to be 0 in circular_buffer.c)
  data_index   in[2];   //!< Start inserting data at data[in]
  data_index   out[2];  //!< Start reading data at data[out]
  data_index   limit;   //!< size of data buffer (last available index + 1)
} fiol_management;

//! pointer to circular buffer management part
typedef fiol_management* fiol_management_p;

//! Set of statistics we want to record as a circular buffer is used
typedef struct {
  uint64_t num_reads;
  uint64_t num_read_elems;
  double   total_read_wait_time_ms;
  double   total_read_time_ms;
  uint64_t max_fill;

  uint64_t num_writes;
  uint64_t num_write_elems;
  double   total_write_wait_time_ms;
  double   total_write_time_ms;
} cb_stats;

typedef cb_stats* cb_stats_p;

//! skeleton for circular buffer
typedef struct {
  fiol_management m;      //!< Management structure
  cb_stats        stats;  //!< Set of recorded statistics
  data_element    data[]; //!< Data buffer (contains at most limit - 1 useful data elements)
} circular_buffer;

//! pointer to circular buffer
typedef circular_buffer* circular_buffer_p;

//! @brief Compute how much data is stored in a circular buffer, given of set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline data_index available_data(
    const data_index in,   //!< [in] Index of insertion location in the buffer
    const data_index out,  //!< [in] Index of extraction location in the buffer
    const data_index limit //!< [in] Number of elements that the buffer can hold
) {
  return (in >= out) ? in - out : limit - out + in;
}

//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline data_index available_space(
    const data_index in,   //!< [in] Index of insertion location in the buffer
    const data_index out,  //!< [in] Index of extraction location in the buffer
    const data_index limit //!< [in] Number of elements that the buffer can hold
) {
  return limit - available_data(in, out, limit) - 1;
}

enum
{
  CB_FULL    = 0, //!< Array index corresponding to the circular buffer _full_ index (in or out)
  CB_PARTIAL = 1  //!< Array index corresponding to the circular buffer _partial_ index (in or out)
};

//! Print buffer header (to help debugging)
void CB_print_header(circular_buffer_p b //!< [in] Pointer to the buffer to print
);
//! initialize a circular buffer
//! <br> = CB_init(p, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_init(
    circular_buffer_p p,     //!< [in]  pointer to a circular buffer
    int32_t           nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
);
//! create and initialize a circular buffer of size nwords in "shared memory",
//! nwords in in 32 bit units<br>
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = CB_create_shared(&shmid, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_create_shared(
    int32_t* shmid, //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
    int32_t  nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
);
//! detach "shared memory segment" used by circular buffer
//! <br> = CB_detach_shared
//! @return 0 upon success, nonzero upon error
int32_t CB_detach_shared(circular_buffer_p p //!< [in] pointer to a circular buffer
);
//! create and initialize a circular buffer of size nwords in process memory
//! <br> = CB_create(nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_create(int32_t nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
);
//! create and initialize a circular buffer, using supplied space
//! <br> = CB_from_pointer(p, nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_from_pointer(
    void*   p,     //!< [in] Pointer to user supplied memory space
    int32_t nwords //!< [in] Size in number of elements of the circular buffer (#data_element)
);
//! Compute how much space (in number of #data_element) is available in a given circular buffer
//! @return How many elements can still be added
data_index CB_get_available_space(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! Compute how much data (in number of #data_element) is stored in a given circular buffer
//! @return How many elements are stored in the buffer
data_index CB_get_available_data(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! Compute the maximum number of elements the buffer can hold
data_index CB_get_capacity(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! wait until at least na empty slots are available for inserting data
//! <br> = CB_wait_space_available(p, n)
//! @return actual number of empty slots available, -1 on error
int32_t CB_wait_space_available(
    circular_buffer_p p, //!< [in]  pointer to a circular buffer
    int               n  //!< [in]  needed number of available slots (#data_element)
);
//! wait until at least n data tokens are available for extracting data
//! <br> = CB_wait_data_available(p, n)
//! @return actual number of data tokens available, -1 if error
int32_t CB_wait_data_available(
    circular_buffer_p p, //!< [in] pointer to a circular buffer
    int               n  //!< [in] needed number of available  #data_element tokens
);
//! wait until n tokens are available then extract them into dst
//! <br> = CB_atomic_get(p, dst, n)
//! @return number of data tokens available after this operation, -1 if error
int32_t CB_atomic_get(
    circular_buffer_p buffer,       //!< [in]  Pointer to a circular buffer
    data_element*     dst,          //!< [out] Destination array for data extraction
    int               num_elements, //!< [in]  Number of #data_element data items to extract
    int operation //!< [in]  Whether to update the buffer, do a partial read, or simply peek at the next values
);
//! wait until nsrc free slots are available then insert from src array
//! <br> = CB_atomic_put(p, src, n, commit_transaction)
//! @return number of free slots available after this operation, -1 upon error
int32_t CB_atomic_put(
    circular_buffer_p buffer,       //!< [in] Pointer to a circular buffer
    data_element*     src,          //!< [in] Source array for data insertion
    int               num_elements, //!< [in] Number of #data_element data items to insert
    int operation //!< [in] Whether to update the IN pointer so that the newly-inserted data can be read right away
);
int CB_check_integrity(const circular_buffer_p buffer //!< [in] The buffer we want to check
);
void CB_print_stats(
    const circular_buffer_p buffer,     //!< [in] Buffer whose stats we want to print
    int                     buffer_id,  //!< [in] ID of the buffer (displayed at beginning of line)
    int                     with_header //!< [in] Whether to print a header for the values
);

#endif // IO_SERVER_circular_buffer_GEN_H
