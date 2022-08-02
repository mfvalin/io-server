/*
 * Copyright (C) 2022 Environnement et Changement climatique Canada
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

// tell doxygen to ignore this file
// with PREDEFINED = DOXYGEN_SHOULD_SKIP_THIS in config file
#ifndef DOXYGEN_SHOULD_SKIP_THIS
//C_StArT
/**
//C_EnD
//F_StArT //C_StArT
// !> \file
// !> \brief circular buffer package (C and Fortran)
// !>
// !> \verbatim
// !>           circular buffer data layout
// !>
// !>   (IN = OUT) (bufer empty) (LIMIT - FIRST -1 free slots)
// !>
// !> FIRST                                                   LIMIT
// !>   |                                                       |
// !>   v                                                       v
// !>   +------------------------------------------------------+
// !>   ........................................................
// !>   ^------------------------------------------------------+
// !>   |
// !> IN/OUT
// !>   +------------------------------------------------------+
// !>   ........................................................
// !>   +--------------------^---------------------------------+
// !>                        |
// !>                      IN/OUT
// !>
// !>   (IN = OUT - 1) (buffer full)
// !>
// !> FIRST                                                   LIMIT
// !>   |                                                       |
// !>   v                                                       v
// !>   +------------------------------------------------------+
// !>   xxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
// !>   +-------------------^^---------------------------------+
// !>                       ||
// !>                     IN  OUT
// !>   +------------------------------------------------------+
// !>   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
// !>   ^------------------------------------------------------^
// !>   |                                                      |
// !>  OUT                                                     IN
// !>
// !>   (OUT < IN) (LIMIT - IN -1) free, (IN - OUT) data
// !> FIRST                                                   LIMIT
// !>   |                                                       |
// !>   v                                                       v
// !>   +------------------------------------------------------+
// !>   xxxxxxxxxxxxxx..........................................
// !>   ^-------------^----------------------------------------+
// !>   |             |
// !>  OUT            IN
// !>
// !>   (IN < OUT) (OUT - IN -1) free, (LIMIT - OUT + IN - FIRST) data
// !> FIRST                                                   LIMIT
// !>   |                                                       |
// !>   v                                                       v
// !>   +------------------------------------------------------+
// !>   xxxxxxxxxxxxxx................................xxxxxxxxxx
// !>   +-------------^-------------------------------^--------+
// !>                 |                               |
// !>                 IN                             OUT
// !>   x = useful data       . = free space
// !>
// !>
// !>      With partial insertion/extraction
// !>
// !>  avail. data            available space           avail. data
// !>   ...________          _________________         ____...
// !>              |        |                 |       |
// !>   +------------------------------------------------------+
// !>   xxxxxxxxxxxx::::::::...................;;;;;;;xxxxxxxx
// !>   +-----------^-------^------------------^------^--------+
// !>               |       |                  |      |
// !>               IN      |                 OUT     |
// !>                  PARTIAL_IN                PARTIAL_OUT
// !>
// !>   x = useful data    : = partially inserted data
// !>   . = free space     ; = partially extracted data
// !>
// !> \endverbatim
//F_EnD //C_EnD
//C_StArT
*/
//C_EnD

#include <math.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <unistd.h>

//C_StArT
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

//F_StArT
// type :: cb_stats
//   integer(C_INT64_T) :: num_reads            = 0
//   integer(C_INT64_T) :: num_unique_reads     = 0
//   integer(C_INT64_T) :: num_read_elems       = 0
//   integer(C_INT64_T) :: num_fractional_reads = 0
//   real(C_DOUBLE)     :: total_read_wait_time_ms = 0.0
//   real(C_DOUBLE)     :: total_read_time_ms      = 0.0
//   integer(C_INT64_T) :: max_fill       = 0

//   integer(C_INT64_T) :: num_writes            = 0
//   integer(C_INT64_T) :: num_write_elems       = 0
//   integer(C_INT64_T) :: num_fractional_writes = 0
//   real(C_DOUBLE)     :: total_write_wait_time_ms = 0.0
//   real(C_DOUBLE)     :: total_write_time_ms      = 0.0
// end type cb_stats
//F_EnD
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

//C_EnD

//! Number of microseconds to wait between reads of the IN/OUT indices of a buffer when waiting for data to arrive
static const int CB_DATA_CHECK_DELAY_US = 10;
//! Number of microseconds to wait between reads of the IN/OUT indices of a buffer when waiting for space to be freed
static const int CB_SPACE_CHECK_DELAY_US = 10;

int CB_check_integrity(const circular_buffer_p buffer);

//F_StArT
//  interface
//F_EnD

//C_StArT
int CB_get_elem_size(
                    )
//C_EnD
{ 
  return sizeof(data_element);
}

//F_StArT
//  function CB_get_stats(buffer) result(stats_ptr) bind(C, name = 'CB_get_stats')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: buffer
//    type(C_PTR) :: stats_ptr
//  end function CB_get_stats
//F_EnD
cb_stats_p CB_get_stats(circular_buffer_p b)
{
  return &(b->stats);
}

//F_StArT
//  subroutine CB_print_header(buffer) bind(C, name = 'CB_print_header')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< C pointer to the buffer we want to print
//  end subroutine CB_print_header
//F_EnD
//C_StArT
//! Print buffer header (to help debugging)
void CB_print_header(circular_buffer_p b //!< [in] Pointer to the buffer to print
                     )
//C_EnD
{
  printf(
      "version %ld, first %ld, in %ld, in partial %ld, out %ld, out partial %ld, limit %ld, capacity (bytes) %ld\n", (long)b->m.version,
      (long)b->m.first, (long)b->m.in[CB_FULL], (long)b->m.in[CB_PARTIAL], (long)b->m.out[CB_FULL],
      (long)b->m.out[CB_PARTIAL], (long)b->m.limit, (long)b->m.capacity_byte);
}

int might_contain_chars(const int num)
{
  const char* uchars = (char*)&num;
  for (int i = 0; i < 4; ++i) {
    if (uchars[i] < 32 || uchars[i] > 122) return 0;
  }

  return 1;
}

//F_StArT
//  subroutine CB_dump_data(buffer, num_bytes) bind(C, name = 'CB_dump_data')
//    import :: C_PTR, C_INT64_T
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< C pointer to the buffer we want to print
//    integer(C_INT64_T), intent(IN), value :: num_bytes !< How many bytes of data to print
//  end subroutine CB_dump_data
//F_EnD
//C_StArT
void CB_dump_data(
    circular_buffer_p buffer,   //!< [in] Pointer to the buffer to print
    const int64_t     num_bytes //!< [in] How many bytes of data to print
)
//C_EnD
{
  const int LINE_LENGTH = 10;
  const uint64_t num_elements = num_bytes >= 0 && (uint64_t)num_bytes <= buffer->m.limit * sizeof(data_element) ?
      num_bytes_to_num_elem_64(num_bytes) :
      buffer->m.limit;
  
  printf("Buffer data:");
  for (uint64_t i = 0; i < num_elements; ++i)
  {
    if (i % LINE_LENGTH == 0) printf("\n[%5ld] ", i / LINE_LENGTH);

    int add_space = 0;
    if (i == buffer->m.in[CB_PARTIAL]) { printf("\nPARTIAL IN"); add_space = 1; }
    if (i == buffer->m.in[CB_FULL]) { printf("\nFULL IN"); add_space = 1; }
    if (i == buffer->m.out[CB_PARTIAL]) { printf("\nPARTIAL OUT"); add_space = 1; }
    if (i == buffer->m.out[CB_FULL]) { printf("\nFULL OUT"); add_space = 1; }

    if (add_space) {
      printf("\n[     ] ");
      for (uint64_t j = 0; j < i % LINE_LENGTH; ++j) printf("                   ");
    }

    const int64_t elem = buffer->data[i];
    const int32_t* elems = (int32_t*)(&elem);

    for (int j = 0; j < 2; ++j) {
      if (might_contain_chars(elems[j])) {
        const char* letters = (const char*)(&elems[j]);
        printf("    %c%c%c%c ", letters[0], letters[1], letters[2], letters[3]);
      }
      else if (elems[j] > 99999999)
        printf("?%07d ", elems[j] % 10000000);
      else if (elems[j] < -9999999)
        printf("-?%06d ", -elems[j] % 1000000);
      else
        printf("%8d ", elems[j]);
    }
    printf(" ");
  }
  printf("\n");
}

//F_StArT
//   !> initialize a circular buffer<br>
//   !> buffer = CB_init_bytes(p, num_bytes)
//   function CB_init_bytes(p, num_bytes) result(buffer) bind(C,name='CB_init_bytes')
//     import :: C_PTR, C_INT, C_SIZE_T
//     implicit none
//     type(C_PTR),       intent(IN), value :: p           !< pointer to a circular buffer
//     integer(C_SIZE_T), intent(IN), value :: num_bytes   !< the size in bytes of the circular buffer
//     type(C_PTR) :: buffer                               !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
//   end function CB_init_bytes
//F_EnD
//C_StArT
//! initialize a circular buffer
//! <br> = CB_init_bytes(p, num_bytes)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_init_bytes(
    circular_buffer_p p,        //!< [in]  pointer to a circular buffer
    size_t            num_bytes //!< [in]  size in bytes of the circular buffer
    )
//C_EnD
{
  if (p == NULL) {
    printf("ERROR: Given null pointer for initializing a circular_buffer\n");
    return NULL;
  }

  if (num_bytes < CB_MIN_BUFFER_SIZE) {
    printf("ERROR: not requesting enough elements for circular_buffer initialization! Requested %ld, but min buffer size is %ld bytes\n", num_bytes, CB_MIN_BUFFER_SIZE);
    return NULL; // area is too small
  }

  p->m.version         = FIOL_VERSION;
  p->m.first           = 0;
  p->m.in[CB_FULL]     = 0;
  p->m.in[CB_PARTIAL]  = 0;
  p->m.out[CB_FULL]    = 0;
  p->m.out[CB_PARTIAL] = 0;
  p->m.lock            = 0; // Unlocked

  // Memory is already allocated so to get the number of full elements we round down
  const int num_elements = num_bytes / sizeof(data_element);
  // Header size in number of elements
  const data_element header_size  = num_bytes_to_num_elem(sizeof(circular_buffer));

  p->m.limit = num_elements - header_size;
  p->m.capacity_byte = (p->m.limit - 1) * sizeof(data_element);

  p->stats.num_reads               = 0;
  p->stats.num_unique_reads        = 0;
  p->stats.num_read_elems          = 0;
  p->stats.num_fractional_reads    = 0;
  p->stats.total_read_time_ms      = 0.0;
  p->stats.total_read_wait_time_ms = 0.0;
  p->stats.max_fill                = 0;

  p->stats.num_writes               = 0;
  p->stats.num_write_elems          = 0;
  p->stats.num_fractional_writes    = 0;
  p->stats.total_write_time_ms      = 0.0;
  p->stats.total_write_wait_time_ms = 0.0;

  return p;
}

//F_StArT
//   !> create and initialize a circular buffer of size num_bytes in "shared memory"<br>
//   !> p = CB_create_shared_bytes(shmid, num_bytes)
//   function CB_create_shared_bytes(shmid, num_bytes) result(p) BIND(C,name='CB_create_shared_bytes')
//     import :: C_PTR, C_INT, C_SIZE_T
//     implicit none
//     integer(C_INT), intent(OUT)       :: shmid       !< identifier of shared memory area (see man shmget) (-1 upon error)
//     integer(C_SIZE_T), intent(IN), value :: num_bytes   !< size in number of bytes of the circular buffer
//     type(C_PTR) :: p                                 !< pointer to created circular buffer
//   end function CB_create_shared_bytes
//F_EnD
//
// return the "shared memory segment" address of the circular buffer upon success, NULL otherwise
// shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//C_StArT
//! create and initialize a circular buffer of size num_bytes in "shared memory",
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = CB_create_shared_bytes(&shmid, num_bytes)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_create_shared_bytes(
    int32_t* shmid,    //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
    size_t   num_bytes //!< [in]  size in bytes of the circular buffer
    )
//C_EnD
{
  *shmid = -1;

  if (num_bytes < CB_MIN_BUFFER_SIZE)
    return NULL;

  int id = shmget(IPC_PRIVATE, num_bytes, IPC_CREAT); // create shared memory segment
  if (id == -1)
    return NULL;          // error occurred

  void* t = shmat(id, NULL, 0); // attach shared memory segment
  if (t == (void*)-1)
    return NULL;                      // error occurred

  struct shmid_ds ds;
  int status = shmctl(id, IPC_RMID, &ds); // mark segment for deletion (ONLY SAFE ON LINUX)
  if (status != 0)
    return NULL; // this should not fail

  *shmid = id;
  return CB_init_bytes((circular_buffer_p)t, num_bytes);
}

//F_StArT
//   !> detach "shared memory segment" used by circular buffer <br>
//   !> status = CB_detach_shared(p)
//   function CB_detach_shared(p) result(status) BIND(C,name='CB_detach_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
//     integer(C_INT) :: status               !< 0 upon success, -1 upon error
//   end function CB_detach_shared
//F_EnD
//C_StArT
//! detach "shared memory segment" used by circular buffer
//! <br> = CB_detach_shared
//! @return 0 upon success, nonzero upon error
int32_t CB_detach_shared(circular_buffer_p p //!< [in] pointer to a circular buffer
                         )
//C_EnD
{
  if (p == NULL)
    return CB_ERROR;
  return shmdt(p); // detach from "shared memory segment" creeated by CB_create_shared
}

//F_StArT
//   !> create and initialize a circular buffer of size num_bytes in process memory<br>
//   !> p = CB_create_bytes(num_bytes)
//   function CB_create_bytes(num_bytes) result(p) BIND(C,name='CB_create_bytes')
//     import :: C_PTR, C_SIZE_T
//     implicit none
//     integer(C_SIZE_T), intent(IN), value :: num_bytes  !< size in bytes of the circular buffer
//     type(C_PTR) :: p                                   !< pointer to created circular buffer
//   end function CB_create_bytes
//F_EnD
//C_StArT
//! create and initialize a circular buffer of size num_bytes in process memory
//! <br> = CB_create_bytes(num_bytes)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_create_bytes(size_t num_bytes //!< [in]  size in bytes of the circular buffer
                                 )
//C_EnD
{
  if (num_bytes < CB_MIN_BUFFER_SIZE)
    return NULL;
  circular_buffer_p buffer = (circular_buffer_p)malloc(num_bytes);
  return CB_init_bytes(buffer, num_bytes);
}

//F_StArT
//   !> create and initialize a circular buffer of size num_bytes from user supplied memory<br>
//   !> p = CB_from_pointer_bytes(ptr, num_bytes)
//   function CB_from_pointer_bytes(ptr, num_bytes) result(p) BIND(C,name='CB_from_pointer_bytes')
//     import :: C_PTR, C_SIZE_T
//     implicit none
//     type(C_PTR),       intent(IN), value :: ptr        !< pointer to user supplied memory
//     integer(C_SIZE_T), intent(IN), value :: num_bytes  !< size in bytes of the circular buffer
//     type(C_PTR) :: p                                   !< pointer to created circular buffer
//   end function CB_from_pointer_bytes
//F_EnD
//C_StArT
//! create and initialize a circular buffer, using supplied space
//! <br> = CB_from_pointer_bytes(p, num_bytes)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_from_pointer_bytes(
    void*  p,        //!< [in] Pointer to user supplied memory space
    size_t num_bytes //!< [in] Size in bytes of the circular buffer
    )
//C_EnD
{
  if (num_bytes < CB_MIN_BUFFER_SIZE) {
    printf("NOT ASKING FOR A BIG ENOUGH CIRCULAR BUFFER\n");
    return NULL;
  }

  return CB_init_bytes((circular_buffer_p)p, num_bytes);
}

//F_StArT
//  pure function CB_get_available_space_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_available_space_bytes')
//    import C_PTR, C_SIZE_T
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
//    integer(C_SIZE_T) :: num_bytes           !< How many slots are free
//  end function CB_get_available_space_bytes
//F_EnD
//C_StArT
//! Compute how much space (in bytes) is available in a given circular buffer
//! @return How many bytes can still be added
size_t CB_get_available_space_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
                                         )
//C_EnD
{
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile uint64_t* in  = &buffer->m.in[CB_PARTIAL];
  volatile uint64_t* out = &buffer->m.out[CB_FULL];
  return available_space_elem(*in, *out, buffer->m.limit) * sizeof(data_element);
}

//F_StArT
//  pure function CB_get_available_data_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_available_data_bytes')
//    import C_PTR, C_SIZE_T
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
//    integer(C_SIZE_T) :: num_bytes           !< How many bytes are stored in the buffer
//  end function CB_get_available_data_bytes
//F_EnD
//C_StArT
//! Compute how much data (in bytes) is stored in a given circular buffer
//! @return How many bytes are stored in the buffer
size_t CB_get_available_data_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
                                  )
//C_EnD
{
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile uint64_t* in  = &buffer->m.in[CB_FULL];
  volatile uint64_t* out = &buffer->m.out[CB_PARTIAL];
  return available_data_elem(*in, *out, buffer->m.limit) * sizeof(data_element);
}

//F_StArT
//  function CB_get_capacity_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_capacity_bytes')
//    import C_PTR, C_SIZE_T
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to the circular buffer we want to query
//    integer(C_SIZE_T) :: num_bytes           !< How many total elements can potentially be store in the buffer
//  end function CB_get_capacity_bytes
//F_EnD
//C_StArT
//! Compute the maximum number of bytes the buffer can hold
size_t CB_get_capacity_bytes(const circular_buffer_p buffer //!< [in] The buffer we want to query
                            )
//C_EnD
{
  return (buffer->m.limit - buffer->m.first - 1) * sizeof(data_element);
}

//F_StArT
//   !> wait until at least num_bytes_wanted empty slots are available for inserting data<br>
//   !> n = CB_wait_space_available_bytes(p, num_bytes_wanted, timeout_ms)
//   function CB_wait_space_available_bytes(p, num_bytes_wanted, timeout_ms) result(n) BIND(C,name='CB_wait_space_available_bytes')
//     import :: C_PTR, C_SIZE_T, C_INT64_T, C_INT
//     implicit none
//     type(C_PTR),       intent(IN), value :: p                 !< pointer to a circular buffer
//     integer(C_SIZE_T), intent(IN), value :: num_bytes_wanted  !< needed number of available bytes
//     integer(C_INT),    intent(IN), value :: timeout_ms        !< How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
//     integer(C_INT64_T) :: n                                   !< actual number of available bytes, a negative error code on error
//   end function CB_wait_space_available_bytes
//F_EnD
//C_StArT
//! wait until at least num_bytes_wanted empty slots are available for inserting data
//! <br> = CB_wait_space_available_bytes(p, num_bytes, timeout_ms)
//! @return actual number of bytes available, a negative error code on error
int64_t CB_wait_space_available_bytes(
    circular_buffer_p p,                //!< [in]  pointer to a circular buffer
    size_t            num_bytes_wanted, //!< [in]  needed number of available bytes
    int               timeout_ms        //!< [in]  How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
    )
//C_EnD
{
  const int status = CB_check_integrity(p);
  if (status != CB_SUCCESS)
    return status;

  // Check whether there is enough space in the entire CB
  if (num_bytes_wanted > p->m.capacity_byte)
    return CB_ERROR_INSUFFICIENT_SPACE;

  const size_t num_removable_data = CB_get_available_data_bytes(p);
  size_t num_available = CB_get_available_space_bytes(p);

  // Check whether there is enough fillable space in the CB
  if (num_available + num_removable_data < num_bytes_wanted)
    return CB_ERROR_INSUFFICIENT_SPACE;

  // Wait until there is enough space (up to the specified amount of time)
  const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / CB_SPACE_CHECK_DELAY_US;
  size_t num_waits = 0;
  for (num_waits = 0; num_waits < max_num_waits && num_available < num_bytes_wanted; ++num_waits) {
    sleep_us(CB_SPACE_CHECK_DELAY_US);
    num_available = CB_get_available_space_bytes(p);
  }

  // Update stats
  p->stats.total_write_wait_time_ms += num_waits * CB_SPACE_CHECK_DELAY_US / 1000.0;

  // Check if there still isn't enough space
  if (num_available < num_bytes_wanted) return CB_ERROR_TIMEOUT;

  return (int64_t)num_available;
}

//F_StArT
//   !> wait until at least num_bytes_wanted are available for extracting data<br>
//   !> p = CB_wait_data_available_bytes(p, num_bytes_wanted, timeout_ms)
//   function CB_wait_data_available_bytes(p, num_bytes_wanted, timeout_ms) result(n) BIND(C,name='CB_wait_data_available_bytes')
//     import :: C_PTR, C_SIZE_T, C_INT64_T, C_INT
//     implicit none
//     type(C_PTR),       intent(IN), value :: p                !< pointer to a circular buffer
//     integer(C_SIZE_T), intent(IN), value :: num_bytes_wanted !< needed number of available bytes
//     integer(C_INT),    intent(IN), value :: timeout_ms       !< How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
//     integer(C_INT64_T) :: n                                  !< actual number of bytes available, a negative error code if error
//   end function CB_wait_data_available_bytes
//F_EnD
//C_StArT
//! wait until at least num_bytes_wanted are available for extracting data
//! <br> = CB_wait_data_available_bytes(p, num_bytes_wanted)
//! @return actual number of bytes available, a negative error code if error
int64_t CB_wait_data_available_bytes(
    circular_buffer_p p,                //!< [in] pointer to a circular buffer
    size_t            num_bytes_wanted, //!< [in] needed number of available bytes
    int               timeout_ms        //!< [in] How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
    )
//C_EnD
{
  const int status = CB_check_integrity(p);
  if (status != CB_SUCCESS)
    return status;

  if (num_bytes_wanted > p->m.capacity_byte)
    return CB_ERROR_INSUFFICIENT_SPACE;

  // Wait until data becomes available (up until the specified amount of time)
  const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / CB_DATA_CHECK_DELAY_US;
  size_t num_available = CB_get_available_data_bytes(p);
  size_t num_waits = 0;
  for (num_waits = 0; num_waits < max_num_waits && num_available < num_bytes_wanted; ++num_waits) {
    sleep_us(CB_DATA_CHECK_DELAY_US);
    num_available = CB_get_available_data_bytes(p);
  }

  // Update stats
  p->stats.total_read_wait_time_ms += num_waits * CB_DATA_CHECK_DELAY_US / 1000.0;

  // Check whether there still isn't enough data
  if (num_available < num_bytes_wanted) return CB_ERROR_TIMEOUT;

  return (int64_t)num_available;
}

//F_StArT
//   !> wait until num_bytes are available then extract them into dst<br>
//   !> CB_get(p, dest, num_bytes, operation)
//   function CB_get(p, dest, num_bytes, operation, timeout_ms) result(status) BIND(C, name='CB_get')
//     import :: C_PTR, C_INT, C_SIZE_T
//     implicit none
//     type(C_PTR),       intent(IN), value :: p             !< pointer to a circular buffer
//     integer(C_SIZE_T), intent(IN), value :: num_bytes     !< number of bytes to extract
//     type(C_PTR),       intent(IN), value :: dest          !< destination array to receive extracted data
//     integer(C_INT),    intent(IN), value :: operation     !< Whether to update the OUT index, partially read, or peek
//     integer(C_INT),    intent(IN), value :: timeout_ms    !< How long to wait (in ms) before giving up and returning an error
//     integer(C_INT) :: status                              !< 0 if success, -1 if error
//   end function CB_get
//F_EnD
//C_StArT
//! wait until num_bytes are available then extract them into dst
//! <br> = CB_get(p, dest, num_bytes)
//! @return CB_SUCCESS on success, a negative error code on error
int CB_get(
    circular_buffer_p buffer,     //!< [in]  Pointer to a circular buffer
    void*             dest,       //!< [out] Destination array for data extraction
    size_t            num_bytes,  //!< [in]  Number of #data_element data items to extract
    int               operation,  //!< [in]  Whether to update the buffer, do a partial read, or simply peek at the next values
    int               timeout_ms  //!< [in]  How long to wait (in ms) before giving up and returning an error
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  IO_timer_start(&timer);

  const int64_t num_available = CB_wait_data_available_bytes(buffer, num_bytes, timeout_ms);
  if (num_available < 0)
    return num_available;

  uint64_t       out   = buffer->m.out[CB_PARTIAL];
  const uint64_t limit = buffer->m.limit;
  data_element*  data  = buffer->data;

  const size_t num_elements = num_bytes_to_num_elem(num_bytes);

  const size_t num_elem_1  = num_elements > (limit - out) ? (limit - out) : num_elements;
  const size_t num_bytes_1 = num_elem_1 < num_elements ? num_elem_1 * sizeof(data_element) : num_bytes;
  copy_bytes(dest, (void*)(data + out), num_bytes_1);
  out += num_elem_1;

  if (out >= limit)
    out = buffer->m.first;

  if (num_elem_1 < num_elements) {
    const size_t num_elem_2  = num_elements - num_elem_1;
    const size_t num_bytes_2 = num_bytes - num_bytes_1;
    copy_bytes((char*)dest + num_bytes_1, (void*)(data + out), num_bytes_2);
    out += num_elem_2;
  }

  if (operation != CB_PEEK) {
    buffer->m.out[CB_PARTIAL] = out;
  }

  if (operation == CB_COMMIT) {
    full_memory_fence(); // memory fence, make sure everything fetched and stored before adjusting the "out" pointer
    uint64_t volatile* outp = &(buffer->m.out[CB_FULL]);
    *outp                   = out;
  }

  if (operation != CB_PEEK) {
    buffer->stats.num_read_elems += num_elements;
    buffer->stats.num_unique_reads++;
  }

  buffer->stats.num_reads++;

  IO_timer_stop(&timer);
  buffer->stats.total_read_time_ms += IO_time_ms(&timer);

  // Only count fractional reads that won't be read again (so no CB_PEEK)
  if (num_bytes_1 != num_elements * sizeof(data_element) && operation != CB_PEEK) buffer->stats.num_fractional_reads++;

  return CB_SUCCESS;
}

//F_StArT
//   !> wait until num_bytes are available then insert from src array<br>
//   !> n = CB_put(p, src, num_bytes, commit_transaction)
//   function CB_put(p, src, num_bytes, commit_transaction, timeout_ms, thread_safe) result(status) BIND(C,name='CB_put')
//     import :: C_PTR, C_INT, C_SIZE_T
//     implicit none
//     type(C_PTR),       intent(IN), value :: p                  !< pointer to a circular buffer
//     integer(C_SIZE_T), intent(IN), value :: num_bytes          !< number of bytes to insert from src
//     type(C_PTR),       intent(IN), value :: src                !< source array for data insertion
//     integer(C_INT),    intent(IN), value :: commit_transaction !< Whether to make the inserted data available immediately (1) or not (0)
//     integer(C_INT),    intent(IN), value :: timeout_ms         !< How long to wait (in ms) before giving up and returning an error
//     integer(C_INT),    intent(IN), value :: thread_safe        !< Whether to perform the operation in a thread-safe manner (when == 1)
//     integer(C_INT) :: status                                   !< 0 if success, -1 if failure
//   end function CB_put
//F_EnD
//C_StArT
//! wait until num_bytes are available then insert from src array
//! <br> = CB_put(p, src, num_bytes, commit_transaction)
//! @return CB_SUCCESS upon success, a negative error code upon error
int CB_put(
    circular_buffer_p buffer,    //!< [in] Pointer to a circular buffer
    void*             src,       //!< [in] Source array for data insertion
    size_t            num_bytes, //!< [in] Number of bytes to insert
    int operation,    //!< [in] Whether to update the IN pointer so that the newly-inserted data can be read right away
    int timeout_ms,   //!< [in] How long to wait (in ms) before giving up and returning an error
    int thread_safe   //!< [in] If 1, perform operation in a thread-safe way
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  IO_timer_start(&timer);

  if (thread_safe == 1 && operation != CB_COMMIT) {
    printf("WARNING: Trying to put data in a CB in a thread safe way, but not committing the message. We don't allow that for now.\n");
    return CB_ERROR_NOT_ALLOWED;
  }

  if (thread_safe == 1) acquire_lock(&buffer->m.lock);

  const int64_t num_available = CB_wait_space_available_bytes(buffer, num_bytes, timeout_ms);
  if (num_available < 0) {
    if (thread_safe == 1) release_lock(&buffer->m.lock);
    return num_available;
  }

  data_element*  data       = buffer->data;
  uint64_t       current_in = buffer->m.in[CB_PARTIAL];
  const uint64_t limit      = buffer->m.limit;

  const size_t num_elements = num_bytes_to_num_elem(num_bytes);

  const size_t num_elem_1  = num_elements > (limit - current_in) ? (limit - current_in) : num_elements;
  const size_t num_bytes_1 = num_elem_1 < num_elements ? num_elem_1 * sizeof(data_element) : num_bytes;
  copy_bytes((void*)(data + current_in), src, num_bytes_1);
  current_in += num_elem_1;

  if (current_in >= limit)
    current_in = buffer->m.first;

  if (num_elements > num_elem_1) {
    const size_t num_elem_2  = num_elements - num_elem_1;
    const size_t num_bytes_2 = num_bytes - num_bytes_1;
    copy_bytes(data, (char*)src + num_bytes_1, num_bytes_2);
    current_in += num_elem_2;
  }

  buffer->m.in[CB_PARTIAL] = current_in;

  if (operation == CB_COMMIT) {
    write_fence(); // make sure everything is in memory before adjusting the "in" pointer
    uint64_t volatile* inp = &(buffer->m.in[CB_FULL]);
    *inp                   = current_in;
  }

  IO_timer_stop(&timer);

  const uint64_t new_fill = limit - num_bytes_to_num_elem(num_available) + num_elements - 1;
  if (new_fill > buffer->stats.max_fill) {
    buffer->stats.max_fill = new_fill;
  }
  buffer->stats.num_write_elems += num_elements;
  buffer->stats.num_writes++;
  buffer->stats.total_write_time_ms += IO_time_ms(&timer);

  if (num_bytes != num_elements * sizeof(data_element)) buffer->stats.num_fractional_writes++;

  if (thread_safe == 1) release_lock(&buffer->m.lock);

  return CB_SUCCESS;
}

//  F_StArT
//    pure function CB_check_integrity(buffer) result(status) BIND(C, name = 'CB_check_integrity')
//      import C_INT, C_PTR
//      implicit none
//      type(C_PTR),    intent(in), value :: buffer
//      integer(C_INT) :: status
//    end function CB_check_integrity
//  F_EnD
/**
 * @brief Verify the header of the given buffer is self-consistent (correct version, first = 0, in/out within limits)
 * @return CB_SUCCESS if the Buffer is consistent, an error code otherwise
 */
//  C_StArT
int CB_check_integrity(
    const circular_buffer_p buffer  //!< [in] The buffer we want to check
    )
//  C_EnD
{
  if (buffer == NULL) {
    return CB_ERROR_INVALID_POINTER;
  }

  if (buffer->m.version != FIOL_VERSION) {
    return CB_ERROR_INVALID_VERSION;
  }

  if (buffer->m.first != 0) {
    return CB_ERROR_INVALID_FIRST;
  }

  if (buffer->m.in[CB_FULL] < buffer->m.first || buffer->m.in[CB_FULL] >= buffer->m.limit) {
    return CB_ERROR_INVALID_FULL_IN;
  }

  if (buffer->m.out[CB_FULL] < buffer->m.first || buffer->m.out[CB_FULL] >= buffer->m.limit) {
    return CB_ERROR_INVALID_FULL_OUT;
  }

  if (buffer->m.in[CB_PARTIAL] < buffer->m.first || buffer->m.in[CB_PARTIAL] >= buffer->m.limit) {
    return CB_ERROR_INVALID_PARTIAL_IN;
  }

  if (buffer->m.out[CB_PARTIAL] < buffer->m.first || buffer->m.out[CB_PARTIAL] >= buffer->m.limit) {
    return CB_ERROR_INVALID_PARTIAL_OUT;
  }

  return CB_SUCCESS;
}

//  F_StArT
//  pure function CB_error_code_to_string(error_code) result(error_string) BIND(C, name = 'CB_error_code_to_string')
//    import C_INT, C_PTR
//    implicit none
//    integer(C_INT), intent(in), value :: error_code
//    type(C_PTR) :: error_string
//  end function CB_error_code_to_string
//  F_EnD
// C_StArT
const char* CB_error_code_to_string(
    const int error_code  //!< [in] The error code we want to translate into a string
    )
// C_EnD
{
  switch (error_code)
  {
  case CB_SUCCESS:                    return "CB_SUCCESS";
  case CB_ERROR:                      return "CB_ERROR";
  case CB_ERROR_INVALID_POINTER:      return "CB_ERROR_INVALID_POINTER";
  case CB_ERROR_INVALID_VERSION:      return "CB_ERROR_INVALID_VERSION";
  case CB_ERROR_INVALID_FIRST:        return "CB_ERROR_INVALID_FIRST";
  case CB_ERROR_INVALID_FULL_IN:      return "CB_ERROR_INVALID_FULL_IN";
  case CB_ERROR_INVALID_FULL_OUT:     return "CB_ERROR_INVALID_FULL_OUT";
  case CB_ERROR_INVALID_PARTIAL_IN:   return "CB_ERROR_INVALID_PARTIAL_IN";
  case CB_ERROR_INVALID_PARTIAL_OUT:  return "CB_ERROR_INVALID_PARTIAL_OUT";
  case CB_ERROR_INSUFFICIENT_SPACE:   return "CB_ERROR_INSUFFICIENT_SPACE";
  case CB_ERROR_TIMEOUT:              return "CB_ERROR_TIMEOUT";
  case CB_ERROR_NOT_ALLOWED:          return "CB_ERROR_NOT_ALLOWED";
  case DCB_ERROR_INVALID_CAPACITY:    return "DCB_ERROR_INVALID_CAPACITY";
  case DCB_ERROR_INVALID_RANK:        return "DCB_ERROR_INVALID_RANK";
  case DCB_ERROR_INVALID_INSTANCE:    return "DCB_ERROR_INVALID_INSTANCE";
  default:                            return "[UNKNOWN CB ERROR CODE]";
  }
}

//! Provide a string representation of a number in a human readable way (with the k, M or G suffix if needed)
//C_StArT
void readable_element_count(
    const double num_elements, //!< [in]  Number we want to represent
    char*        buffer        //!< [out] Buffer where the string will be stored. Must contain at least 8 bytes
    )
//C_EnD
{
  double amount = num_elements;
  int    unit   = 0;

  const char UNITS[] = {'\0', 'k', 'M', 'G', 'T', 'P', 'E'};

  while (amount > 1900.0 && unit < 6) {
    amount /= 1000.0;
    unit++;
  }

  if (unit == 0) {
    if (ceil(amount) == amount)
      sprintf(buffer, "%7.0f", amount);
    else
      sprintf(buffer, "%7.2f", amount);
  }
  else if (amount < 10000.0) {
    sprintf(buffer, "%6.1f%c", amount, UNITS[unit]);
  }
  else {
    sprintf(buffer, "    %s", "inf");
  }
}

//! Provide a string representation of a time in a human readable way, with units
void readable_time(
  const double time_ms, //!< [in]  Time we want to print, in ms
  char*        buffer   //!< [out] Buffer where the string will be stored. Must 
)
{
  const char* UNITS[] = {"ns", "us", "ms", "s ", "m ", "H "};
  double amount = time_ms;
  int    unit   = 2;

  if (time_ms < 1000.0) {
    while (amount < 1.0 && unit > 0) {
      amount *= 1000.0;
      unit--;
    }
  }
  else {
    amount /= 1000.0;
    unit++;
    while (amount > 120.0 && unit < 5) {
      amount /= 60.0;
      unit++;
    }
  }
  sprintf(buffer, "%5.1f%s", amount, UNITS[unit]);
}

//F_StArT
//  subroutine CB_print_stats(buffer, buffer_id, with_header) BIND(C, name = 'CB_print_stats')
//    import C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: buffer
//    integer(C_INT), intent(in), value :: buffer_id
//    integer(C_INT), intent(in), value :: with_header
//  end subroutine CB_print_stats
//F_EnD
//C_StArT
void CB_print_stats(
    const circular_buffer_p buffer,     //!< [in] Buffer whose stats we want to print
    int                     buffer_id,  //!< [in] ID of the buffer (displayed at beginning of line)
    int                     with_header //!< [in] Whether to print a header for the values
    )
//C_EnD
{
  const int status = CB_check_integrity(buffer);
  if (status != CB_SUCCESS) {
    printf("ERROR: Cannot print CB because CB is not valid: %s\n", CB_error_code_to_string(status));
    return;
  }

  const cb_stats_p stats = &buffer->stats;

  const uint64_t num_writes = stats->num_writes;
  const uint64_t num_reads  = stats->num_unique_reads;

  const uint64_t num_write_elems = stats->num_write_elems;
  const uint64_t num_read_elems  = stats->num_read_elems;

  char total_in_s[8], avg_in_s[8], total_out_s[8], avg_out_s[8], read_per_sec_s[8], write_per_sec_s[8], max_fill_s[8];
  char total_write_time_s[8], total_write_wait_time_s[8], avg_write_wait_time_s[8], total_read_time_s[8], total_read_wait_time_s[8], avg_read_wait_time_s[8];

  const double avg_bytes_in  = num_writes > 0 ? (double)stats->num_write_elems * sizeof(data_element) / num_writes : 0.0;
  const double avg_bytes_out = num_reads > 0 ? (double)stats->num_read_elems * sizeof(data_element) / num_reads : 0.0;

  readable_element_count(stats->num_write_elems * sizeof(data_element), total_in_s);
  readable_element_count(avg_bytes_in, avg_in_s);
  readable_element_count(stats->num_read_elems * sizeof(data_element), total_out_s);
  readable_element_count(avg_bytes_out, avg_out_s);

  const double avg_wait_w       = num_writes > 0 ? stats->total_write_wait_time_ms / num_writes : 0.0;
  const double avg_wait_r       = num_reads > 0 ? stats->total_read_wait_time_ms / num_reads : 0.0;
  const double total_write_time = stats->total_write_time_ms + 1e-24; // Lazy trick to avoid division by zero later on
  const double total_read_time  = stats->total_read_time_ms  + 1e-24; // Lazy trick to avoid division by zero later on
  readable_time(total_write_time, total_write_time_s);
  readable_time(total_read_time, total_read_time_s);
  readable_time(stats->total_write_wait_time_ms, total_write_wait_time_s);
  readable_time(stats->total_read_wait_time_ms, total_read_wait_time_s);
  readable_time(avg_wait_w, avg_write_wait_time_s);
  readable_time(avg_wait_r, avg_read_wait_time_s);

  readable_element_count(num_write_elems / total_write_time * 1000.0 * sizeof(data_element), write_per_sec_s);
  readable_element_count(num_read_elems  / total_read_time  * 1000.0 * sizeof(data_element), read_per_sec_s);

  readable_element_count(stats->max_fill * sizeof(data_element), max_fill_s);
  const int max_fill_percent = (int)(stats->max_fill * 100.0 / CB_get_capacity_bytes(buffer));

  const int frac_write_percent = num_writes > 0 ? (int)(stats->num_fractional_writes * 100.0 / num_writes) : 0;
  const int frac_read_percent  = num_reads  > 0 ? (int)(stats->num_fractional_reads  * 100.0 / num_reads)  : 0;

  if (with_header) {
    printf("     "
           "                       Write (ms)                          |"
           "                       Read (ms)                           |\n"
           "rank "
           "  #bytes  (B/call) : tot. time (B/sec) :  wait ms (/call)  |"
           "  #bytes  (B/call) : tot. time (B/sec) :  wait ms (/call)  | "
           "max fill B (%%)| frac. writes, reads (%%)\n");
  }

  printf(
      "%04d: "
      "%s (%s) : %s (%s) : %s (%s) | "
      "%s (%s) : %s (%s) : %s (%s) | "
      "%s (%3d) | %3d, %3d\n",
      buffer_id, total_in_s, avg_in_s, total_write_time_s, write_per_sec_s, total_write_wait_time_s, avg_write_wait_time_s,
      total_out_s, avg_out_s, total_read_time_s, read_per_sec_s, total_read_wait_time_s, avg_read_wait_time_s, max_fill_s,
      max_fill_percent, frac_write_percent, frac_read_percent);
}

//F_StArT
//  end interface
//F_EnD
#endif // DOXYGEN_SHOULD_SKIP_THIS
