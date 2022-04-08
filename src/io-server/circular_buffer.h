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
// This file has been generated from circular_buffer.c
#ifndef IO_SERVER_circular_buffer_GEN_H
#define IO_SERVER_circular_buffer_GEN_H

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

#include "io-server/cb_data.h"
#include "io-server/rpn_extra.h"
#include "io-server/timer.h"

//!> version marker
#define FIOL_VERSION 0x1BAD

static const size_t CB_MIN_BUFFER_SIZE = 128 * 4; //!> Minimum size of a circular buffer, in bytes

//! Circular buffer management variables
//! Only use 64-bit members in that struct. Better for alignment
//! <br>in == out means buffer is empty
//! <br>in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct {
  uint64_t version; //!< version marker
  uint64_t first;   //!< should be 0, because the feature has not been implemented yet
  uint64_t in[2];   //!< Start inserting data at data[in]
  uint64_t out[2];  //!< Start reading data at data[out]
  uint64_t limit;   //!< Size of data buffer (last available index + 1)
  uint64_t capacity_byte; //!< Size of data buffer in bytes
  int32_t  lock;    //!< To be able to perform thread-safe operations (not necessarily used)
  int32_t  dummy; //!< So that the struct has size multiple of 64 bits
} fiol_management;

//! pointer to circular buffer management part
typedef fiol_management* fiol_management_p;

//! Set of statistics we want to record as a circular buffer is used
//! Only use 64-bit members in that struct. Better for alignment
typedef struct {
  uint64_t num_reads;
  uint64_t num_unique_reads;
  uint64_t num_read_elems;
  uint64_t num_fractional_reads;
  double   total_read_wait_time_ms;
  double   total_read_time_ms;
  uint64_t max_fill;

  uint64_t num_writes;
  uint64_t num_write_elems;
  uint64_t num_fractional_writes;
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
//! @return Number of elements stored in the buffer (available for reading)
static inline uint64_t available_data_elem(
    const uint64_t in,   //!< [in] Index of insertion location in the buffer
    const uint64_t out,  //!< [in] Index of extraction location in the buffer
    const uint64_t limit //!< [in] Number of elements that the buffer can hold
) {
  return (in >= out) ? in - out : limit - out + in;
}

//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
//! @return Available space in the buffer, in number of elements
static inline size_t available_space_elem(
    const uint64_t in,   //!< [in] Index of insertion location in the buffer
    const uint64_t out,  //!< [in] Index of extraction location in the buffer
    const uint64_t limit //!< [in] Number of elements that the buffer can hold
) {
  return limit - available_data_elem(in, out, limit) - 1;
}

enum
{
  CB_FULL    = 0, //!< Array index corresponding to the circular buffer _full_ index (in or out)
  CB_PARTIAL = 1  //!< Array index corresponding to the circular buffer _partial_ index (in or out)
};

//! @brief Compute the smallest number of elements that can fit the given number of bytes.
//! Basically, it's num_bytes / elem_size rounded up.
static inline size_t num_bytes_to_num_elem(const size_t num_bytes)
{
  const size_t remainder = num_bytes % sizeof(data_element) > 0 ? 1 : 0;
  return num_bytes / sizeof(data_element) + remainder;
}

static inline size_t num_bytes_to_num_elem_64(const size_t num_bytes) {
  const size_t num_elem = num_bytes_to_num_elem(num_bytes);
  const size_t add = (num_elem * sizeof(data_element)) % 8 == 0 ? 0 : 1;
  return num_elem + add;
}

/**
 * @brief Copy buffer elements into another array (either into or out of the buffer)
 */
static inline void copy_bytes(
    void*        dst,      //!< [out] Where to copy the elements
    const void*  src,      //!< [in]  The elements to copy
    const size_t num_bytes //!< [in] How many bytes we want to copy
) {
  memcpy(dst, src, num_bytes);
}

//! Compute the space in kilobytes taken by the given number of elements
static inline double num_elem_to_kb(const size_t num_elements) {
  return num_elements * sizeof(data_element) / 1024.0;
}

int CB_get_elem_size(
);
//! Print buffer header (to help debugging)
void CB_print_header(circular_buffer_p b //!< [in] Pointer to the buffer to print
);
void CB_dump_data(circular_buffer_p buffer //!< [in] Pointer to the buffer to print
);
//! initialize a circular buffer
//! <br> = CB_init_bytes(p, num_bytes)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_init_bytes(
    circular_buffer_p p,        //!< [in]  pointer to a circular buffer
    size_t            num_bytes //!< [in]  size in bytes of the circular buffer
);
//! create and initialize a circular buffer of size num_bytes in "shared memory",
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = CB_create_shared_bytes(&shmid, num_bytes)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_create_shared_bytes(
    int32_t* shmid,    //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
    size_t   num_bytes //!< [in]  size in bytes of the circular buffer
);
//! detach "shared memory segment" used by circular buffer
//! <br> = CB_detach_shared
//! @return 0 upon success, nonzero upon error
int32_t CB_detach_shared(circular_buffer_p p //!< [in] pointer to a circular buffer
);
//! create and initialize a circular buffer of size num_bytes in process memory
//! <br> = CB_create_bytes(num_bytes)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_create_bytes(size_t num_bytes //!< [in]  size in bytes of the circular buffer
);
//! create and initialize a circular buffer, using supplied space
//! <br> = CB_from_pointer_bytes(p, num_bytes)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_from_pointer_bytes(
    void*  p,        //!< [in] Pointer to user supplied memory space
    size_t num_bytes //!< [in] Size in bytes of the circular buffer
);
//! Compute how much space (in bytes) is available in a given circular buffer
//! @return How many bytes can still be added
size_t CB_get_available_space_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! Compute how much data (in bytes) is stored in a given circular buffer
//! @return How many bytes are stored in the buffer
size_t CB_get_available_data_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! Compute the maximum number of bytes the buffer can hold
size_t CB_get_capacity_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! wait until at least num_bytes_wanted empty slots are available for inserting data
//! <br> = CB_wait_space_available_bytes(p, num_bytes)
//! @return actual number of bytes available, a negative error code on error
int64_t CB_wait_space_available_bytes(
    circular_buffer_p p,                //!< [in]  pointer to a circular buffer
    size_t            num_bytes_wanted  //!< [in]  needed number of available bytes
);
//! wait until at least num_bytes_wanted are available for extracting data
//! <br> = CB_wait_data_available_bytes(p, num_bytes_wanted)
//! @return actual number of bytes available, a negative error code if error
int64_t CB_wait_data_available_bytes(
    circular_buffer_p p,                //!< [in] pointer to a circular buffer
    size_t            num_bytes_wanted  //!< [in] needed number of available bytes
);
//! wait until num_bytes are available then extract them into dst
//! <br> = CB_get(p, dest, num_bytes)
//! @return CB_SUCCESS on success, a negative error code on error
int CB_get(
    circular_buffer_p buffer,    //!< [in]  Pointer to a circular buffer
    void*             dest,      //!< [out] Destination array for data extraction
    size_t            num_bytes, //!< [in]  Number of #data_element data items to extract
    int operation                //!< [in]  Whether to update the buffer, do a partial read, or simply peek at the next values
);
//! wait until num_bytes are available then insert from src array
//! <br> = CB_put(p, src, num_bytes, commit_transaction)
//! @return CB_SUCCESS upon success, a negative error code upon error
int CB_put(
    circular_buffer_p buffer,    //!< [in] Pointer to a circular buffer
    void*             src,       //!< [in] Source array for data insertion
    size_t            num_bytes, //!< [in] Number of bytes to insert
    int operation,    //!< [in] Whether to update the IN pointer so that the newly-inserted data can be read right away
    int thread_safe   //!< [in] If 1, perform operation in a thread-safe way
);
int CB_check_integrity(
    const circular_buffer_p buffer  //!< [in] The buffer we want to check
);
const char* CB_error_code_to_string(
    const int error_code  //!< [in] The error code we want to translate into a string
);
void readable_element_count(
    const double num_elements, //!< [in]  Number we want to represent
    char*        buffer        //!< [out] Buffer where the string will be stored. Must contain at least 8 bytes
);
void CB_print_stats(
    const circular_buffer_p buffer,     //!< [in] Buffer whose stats we want to print
    int                     buffer_id,  //!< [in] ID of the buffer (displayed at beginning of line)
    int                     with_header //!< [in] Whether to print a header for the values
);

#endif // IO_SERVER_circular_buffer_GEN_H
