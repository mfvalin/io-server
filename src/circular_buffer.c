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

//F_StArT
//  interface
//F_EnD

//C_StArT
#include <stdlib.h>
#include <string.h>

#include "io-server/cb_data.h"
#include "io-server/rpn_extra.h"
#include "io-server/timer.h"

//!> version marker
#define FIOL_VERSION 0x1BAD

static const int MIN_CIRC_BUFFER_SIZE = 128; //!> Minimum size of a circular buffer, in number of #data_element

//!> circular buffer management variables
//!> <br>in == out means buffer is empty
//!> <br>in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct {
  data_element version; //!< version marker
  data_element first;   //!< should be 0 (assumed to be 0 in circular_buffer.c)
  data_element in[2];   //!< Start inserting data at data[in]
  data_element out[2];  //!< Start reading data at data[out]
  data_element limit;   //!< size of data buffer (last available index + 1)
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
static inline data_element available_data(
    const data_element in,   //!< [in] Index of insertion location in the buffer
    const data_element out,  //!< [in] Index of extraction location in the buffer
    const data_element limit //!< [in] Number of elements that the buffer can hold
) {
  return (in >= out) ? in - out : limit - out + in;
}

//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline data_element available_space(
    const data_element in,   //!< [in] Index of insertion location in the buffer
    const data_element out,  //!< [in] Index of extraction location in the buffer
    const data_element limit //!< [in] Number of elements that the buffer can hold
) {
  return limit - available_data(in, out, limit) - 1;
}

enum
{
  CB_FULL    = 0, //!< Array index corresponding to the circular buffer _full_ index (in or out)
  CB_PARTIAL = 1  //!< Array index corresponding to the circular buffer _partial_ index (in or out)
};

/**
 * @brief Copy buffer elements into another array (either into or out of the buffer)
 */
static inline void copy_elements(
    data_element*       dst, //!< [out] Where to copy the elements
    const data_element* src, //!< [in]  The elements to copy
    int                 n    //!< [in] How many we want to copy
) {
  memcpy(dst, src, sizeof(data_element) * (size_t)n);
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
      "version %ld, first %ld, in %ld, in partial %ld, out %ld, out partial %ld, limit %ld\n", (long)b->m.version,
      (long)b->m.first, (long)b->m.in[CB_FULL], (long)b->m.in[CB_PARTIAL], (long)b->m.out[CB_FULL],
      (long)b->m.out[CB_PARTIAL], (long)b->m.limit);
}

//F_StArT
//   !> initialize a circular buffer<br>
//   !> buffer = CB_init(p, nwords)
//   function CB_init(p, nwords) result(buffer) bind(C,name='CB_init')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p           !< pointer to a circular buffer
//     integer(C_INT), intent(IN), value :: nwords   !< the size in elements of the circular buffer
//     type(C_PTR) :: buffer                         !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
//   end function CB_init
//F_EnD
//C_StArT
//! initialize a circular buffer
//! <br> = CB_init(p, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_init(
    circular_buffer_p p,     //!< [in]  pointer to a circular buffer
    int32_t           nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
    )
//C_EnD
{
  if (p == NULL) {
    printf("ERROR: Given null pointer for initializing a circular_buffer\n");
    return NULL;
  }

  if (nwords < MIN_CIRC_BUFFER_SIZE) {
    printf("ERROR: not requesting enough elements for circular_buffer initialization!\n");
    return NULL; // area is too small
  }

  p->m.version         = FIOL_VERSION;
  p->m.first           = 0;
  p->m.in[CB_FULL]     = 0;
  p->m.in[CB_PARTIAL]  = 0;
  p->m.out[CB_FULL]    = 0;
  p->m.out[CB_PARTIAL] = 0;

  // Header size in number of elements
  const data_element header_size =
      sizeof(circular_buffer) / sizeof(data_element) + (sizeof(circular_buffer) % sizeof(data_element) > 0);

  p->m.limit = nwords - header_size;

  p->stats.num_reads               = 0;
  p->stats.num_read_elems          = 0;
  p->stats.total_read_time_ms      = 0.0;
  p->stats.total_read_wait_time_ms = 0.0;
  p->stats.max_fill                = 0;

  p->stats.num_writes               = 0;
  p->stats.num_write_elems          = 0;
  p->stats.total_write_time_ms      = 0.0;
  p->stats.total_write_wait_time_ms = 0.0;

  return p;
}

//F_StArT
//   !> create and initialize a circular buffer of size nwords in "shared memory"<br>
//   !> p = CB_create_shared(shmid, nwords)
//   function CB_create_shared(shmid, nwords) result(p) BIND(C,name='CB_create_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(OUT) :: shmid          !< identifier of shared memory area (see man shmget) (-1 upon error)
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer
//   end function CB_create_shared
//F_EnD
//
// return the "shared memory segment" address of the circular buffer upon success, NULL otherwise
// shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//C_StArT
//! create and initialize a circular buffer of size nwords in "shared memory",
//! nwords in in 32 bit units<br>
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = CB_create_shared(&shmid, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_create_shared(
    int32_t* shmid, //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
    int32_t  nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
    )
//C_EnD
{
  void*           t;
  size_t          sz = nwords * sizeof(data_element);
  int             id;
  struct shmid_ds ds;
  int             status;

  *shmid = -1;
  if (sz < 64 * 1024)
    return NULL;
  id = shmget(IPC_PRIVATE, sz, IPC_CREAT); // create shared memory segment
  if (id == -1)
    return NULL;          // error occurred
  t = shmat(id, NULL, 0); // attach shared memory segment
  if (t == (void*)-1)
    return NULL;                      // error occurred
  status = shmctl(id, IPC_RMID, &ds); // mark segment for deletion (ONLY SAFE ON LINUX)
  if (status != 0)
    return NULL; // this should not fail
  *shmid = id;
  return CB_init((circular_buffer_p)t, nwords);
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
    return -1;
  return shmdt(p); // detach from "shared memory segment" creeated by CB_create_shared
}

//F_StArT
//   !> create and initialize a circular buffer of size nwords in process memory<br>
//   !> p = CB_create(nwords)
//   function CB_create(nwords) result(p) BIND(C,name='CB_create')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer
//   end function CB_create
//F_EnD
//C_StArT
//! create and initialize a circular buffer of size nwords in process memory
//! <br> = CB_create(nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_create(int32_t nwords //!< [in]  size in number of elements of the circular buffer (#data_element)
                            )
//C_EnD
{
  circular_buffer_p t;
  size_t            sz = nwords * sizeof(data_element);

  if (sz < MIN_CIRC_BUFFER_SIZE)
    return NULL;
  t = (circular_buffer_p)malloc(sz);
  return CB_init(t, nwords);
}

//F_StArT
//   !> create and initialize a circular buffer of size nwords from user supplied memory<br>
//   !> p = CB_from_pointer(ptr, nwords)
//   function CB_from_pointer(ptr, nwords) result(p) BIND(C,name='CB_from_pointer')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: ptr         !< pointer to user supplied memory
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer
//   end function CB_from_pointer
//F_EnD
//C_StArT
//! create and initialize a circular buffer, using supplied space
//! <br> = CB_from_pointer(p, nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_from_pointer(
    void*   p,     //!< [in] Pointer to user supplied memory space
    int32_t nwords //!< [in] Size in number of elements of the circular buffer (#data_element)
    )
//C_EnD
{
  circular_buffer_p t;
  size_t            sz = nwords * sizeof(data_element);

  if (sz < MIN_CIRC_BUFFER_SIZE) {
    printf("NOT ENOUGH ELEMENTS\n");
    return NULL;
  }
  t = (circular_buffer_p)p;
  return CB_init(t, nwords);
}

//F_StArT
//  function CB_get_available_space(buffer) result(num_elements) BIND(C, name = 'CB_get_available_space')
//    import C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
//    integer(C_INT) :: num_elements           !< How many slots are free
//  end function CB_get_available_space
//F_EnD
//C_StArT
//! Compute how much space (in number of #data_element) is available in a given circular buffer
//! @return How many elements can still be added
data_element CB_get_available_space(const circular_buffer_p buffer //!< [in] The buffer we want to query
                                    )
//C_EnD
{
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile data_element* in  = &buffer->m.in[CB_PARTIAL];
  volatile data_element* out = &buffer->m.out[CB_FULL];
  return available_space(*in, *out, buffer->m.limit);
}

//F_StArT
//  function CB_get_available_data(buffer) result(num_elements) BIND(C, name = 'CB_get_available_data')
//    import C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
//    integer(C_INT) :: num_elements           !< How many elements are stored in the buffer
//  end function CB_get_available_data
//F_EnD
//C_StArT
//! Compute how much data (in number of #data_element) is stored in a given circular buffer
//! @return How many elements are stored in the buffer
data_element CB_get_available_data(const circular_buffer_p buffer //!< [in] The buffer we want to query
                                   )
//C_EnD
{
  // Make sure that the values are really read by accessing them through a volatile pointer
  volatile data_element* in  = &buffer->m.in[CB_FULL];
  volatile data_element* out = &buffer->m.out[CB_PARTIAL];
  return available_data(*in, *out, buffer->m.limit);
}

//F_StArT
//  function CB_get_capacity(buffer) result(num_elements) BIND(C, name = 'CB_get_capacity')
//    import C_PTR, C_INT
//    implicit none
//    type(C_PTR), intent(in), value :: buffer !< Pointer to the circular buffer we want to query
//    integer(C_INT) :: num_elements           !< How many total elements can potentially be store in the buffer
//  end function CB_get_capacity
//F_EnD
//C_StArT
//! Compute the maximum number of elements the buffer can hold
data_element CB_get_capacity(const circular_buffer_p buffer //!< [in] The buffer we want to query
                             )
//C_EnD
{
  return buffer->m.limit - buffer->m.first - 1;
}

//F_StArT
//   !> wait until at least na empty slots are available for inserting data<br>
//   !> n = CB_wait_space_available(p, na)
//   function CB_wait_space_available(p, na) result(n) BIND(C,name='CB_wait_space_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p          !< pointer to a circular buffer
//     integer(C_INT), intent(IN), value :: na      !< needed number of available slots
//     integer(C_INT) :: n                          !< actual number of empty slots available, -1 on error
//   end function CB_wait_space_available
//F_EnD
//C_StArT
//! wait until at least na empty slots are available for inserting data
//! <br> = CB_wait_space_available(p, n)
//! @return actual number of empty slots available, -1 on error
int32_t CB_wait_space_available(
    circular_buffer_p p, //!< [in]  pointer to a circular buffer
    int               n  //!< [in]  needed number of available slots (#data_element)
    )
//C_EnD
{
  if (CB_check_integrity(p) != 0)
    return -1;

  if (n < 0 || n >= p->m.limit)
    return -1;

  data_element num_available = CB_get_available_space(p);
  int          num_waits     = 0;
  while (num_available < n) {
    sleep_us(CB_SPACE_CHECK_DELAY_US);
    num_available = CB_get_available_space(p);
    num_waits++;
  }

  p->stats.total_write_wait_time_ms += num_waits * CB_SPACE_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//F_StArT
//   !> wait until at least n data tokens are available for extracting data<br>
//   !> p = CB_wait_data_available(p, na)
//   function CB_wait_data_available(p, na) result(n) BIND(C,name='CB_wait_data_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer
//     integer(C_INT), intent(IN), value :: na        !< needed number of available tokens
//     integer(C_INT) :: n                            !< actual number of data tokens available, -1 if error
//   end function CB_wait_data_available
//F_EnD
//C_StArT
//! wait until at least n data tokens are available for extracting data
//! <br> = CB_wait_data_available(p, n)
//! @return actual number of data tokens available, -1 if error
int32_t CB_wait_data_available(
    circular_buffer_p p, //!< [in] pointer to a circular buffer
    int               n  //!< [in] needed number of available  #data_element tokens
    )
//C_EnD
{
  if (CB_check_integrity(p) != 0)
    return -1;

  if (n < 0 || n >= p->m.limit)
    return -1;

  data_element num_available = CB_get_available_data(p);
  int          num_waits     = 0;
  while (num_available < n) {
    sleep_us(CB_DATA_CHECK_DELAY_US);
    num_available = CB_get_available_data(p);
    num_waits++;
  }

  p->stats.total_read_wait_time_ms += num_waits * CB_DATA_CHECK_DELAY_US / 1000.0;

  return num_available;
}

//F_StArT
//   !> wait until ndst tokens are available then extract them into dst<br>
//   !> n = CB_atomic_get(p, dst, ndst, operation)
//   function CB_atomic_get(p, dst, ndst, operation) result(n) BIND(C,name='CB_atomic_get')
//     import :: C_PTR, C_INT, DATA_ELEMENT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
//     integer(C_INT), intent(IN), value :: ndst          !< number of tokens to extract
//     integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
//     integer(C_INT), intent(IN), value :: operation     !< Whether to update the OUT index, partially read, or peek
//     integer(C_INT) :: n                                !< number of data tokens available after this operation, -1 if error
//   end function CB_atomic_get
//F_EnD
//C_StArT
//! wait until n tokens are available then extract them into dst
//! <br> = CB_atomic_get(p, dst, n)
//! @return number of data tokens available after this operation, -1 if error
int32_t CB_atomic_get(
    circular_buffer_p buffer,       //!< [in]  Pointer to a circular buffer
    data_element*     dst,          //!< [out] Destination array for data extraction
    int               num_elements, //!< [in]  Number of #data_element data items to extract
    int operation //!< [in]  Whether to update the buffer, do a partial read, or simply peek at the next values
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  io_timer_start(&timer);

  const int num_available = CB_wait_data_available(buffer, num_elements);
  if (num_available < 0)
    return -1;

  // Update "max fill" metric
  if (buffer->stats.max_fill < (uint64_t)num_available)
    buffer->stats.max_fill = num_available;

  data_element       out   = buffer->m.out[CB_PARTIAL];
  const data_element limit = buffer->m.limit;
  data_element*      buf   = buffer->data;

  const int num_elem_1 = num_elements > (limit - out) ? (limit - out) : num_elements;
  copy_elements(dst, buf + out, num_elem_1);
  out += num_elem_1;

  if (out >= limit)
    out = buffer->m.first;

  if (num_elem_1 < num_elements) {
    const int num_elem_2 = num_elements - num_elem_1;
    copy_elements(dst + num_elem_1, buf + out, num_elem_2);
    out += num_elem_2;
  }

  if (operation != CB_PEEK) {
    buffer->m.out[CB_PARTIAL] = out;
  }

  if (operation == CB_COMMIT) {
    memory_fence(); // memory fence, make sure everything fetched and stored before adjusting the "out" pointer
    data_element volatile* outp = &(buffer->m.out[CB_FULL]);
    *outp                       = out;
  }

  io_timer_stop(&timer);
  buffer->stats.total_read_time_ms += io_time_ms(&timer);
  if (operation != CB_PEEK) {
    buffer->stats.num_read_elems += (uint64_t)num_elements;
    buffer->stats.num_reads++;
  }

  return CB_get_available_data(buffer);
}

//F_StArT
//   !> wait until nsrc free slots are available then insert from src array<br>
//   !> n = CB_atomic_put(p, src, nsrc, commit_transaction)
//   function CB_atomic_put(p, src, nsrc, commit_transaction) result(n) BIND(C,name='CB_atomic_put')
//     import :: C_PTR, C_INT, DATA_ELEMENT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                     !< pointer to a circular buffer
//     integer(C_INT), intent(IN), value :: nsrc               !< number of tokens to insert from src
//     integer(DATA_ELEMENT), dimension(*), intent(IN) :: src  !< source array for data insertion
//     integer(C_INT), intent(IN), value :: commit_transaction !< Whether to make the inserted data available immediately (1) or not (0)
//     integer(C_INT) :: n                                     !< number of free slots available after this operation
//   end function CB_atomic_put
//F_EnD
//C_StArT
//! wait until nsrc free slots are available then insert from src array
//! <br> = CB_atomic_put(p, src, n, commit_transaction)
//! @return number of free slots available after this operation, -1 upon error
int32_t CB_atomic_put(
    circular_buffer_p buffer,       //!< [in] Pointer to a circular buffer
    data_element*     src,          //!< [in] Source array for data insertion
    int               num_elements, //!< [in] Number of #data_element data items to insert
    int operation //!< [in] Whether to update the IN pointer so that the newly-inserted data can be read right away
    )
//C_EnD
{
  io_timer_t timer = {0, 0};
  io_timer_start(&timer);

  if (CB_wait_space_available(buffer, num_elements) < 0)
    return -1;

  data_element*      buf        = buffer->data;
  data_element       current_in = buffer->m.in[CB_PARTIAL];
  const data_element limit      = buffer->m.limit;

  const int num_elem_1 = num_elements > (limit - current_in) ? (limit - current_in) : num_elements;
  copy_elements(buf + current_in, src, num_elem_1);
  current_in += num_elem_1;

  if (current_in >= limit)
    current_in = buffer->m.first;

  if (num_elements > num_elem_1) {
    const int num_elem_2 = num_elements - num_elem_1;
    copy_elements(buf, src + num_elem_1, num_elem_2);
    current_in += num_elem_2;
  }

  buffer->m.in[CB_PARTIAL] = current_in;

  if (operation == CB_COMMIT) {
    write_fence(); // make sure everything is in memory before adjusting the "in" pointer
    data_element volatile* inp = &(buffer->m.in[CB_FULL]);
    *inp                       = current_in;
  }

  io_timer_stop(&timer);

  buffer->stats.num_write_elems += num_elements;
  buffer->stats.num_writes++;
  buffer->stats.total_write_time_ms += io_time_ms(&timer);

  return CB_get_available_space(buffer);
}

//  F_StArT
//    function CB_check_integrity(buffer) result(is_valid) BIND(C, name = 'CB_check_integrity')
//      import C_INT, C_PTR
//      implicit none
//      type(C_PTR), intent(in), value :: buffer
//      integer(C_INT) is_valid
//    end function CB_check_integrity
//  F_EnD
/**
 * @brief Verify the header of the given buffer is self-consistent (correct version, first = 0, in/out within limits)
 * @return 0 if the Buffer is consistent, a negative number otherwise
 */
//  C_StArT
int CB_check_integrity(const circular_buffer_p buffer //!< [in] The buffer we want to check
                       )
//  C_EnD
{
  if (buffer == NULL) {
    printf("Invalid b/c NULL pointer\n");
    return -1;
  }

  if (buffer->m.version != FIOL_VERSION) {
    printf("INVALID b/c wrong version (%d, should be %d) %ld\n", buffer->m.version, FIOL_VERSION, (long)buffer);
    return -1;
  }

  if (buffer->m.first != 0) {
    printf("INVALID b/c m.first is NOT 0 (%d)\n", buffer->m.first);
    return -1;
  }

  if (buffer->m.in[CB_FULL] < buffer->m.first || buffer->m.in[CB_FULL] >= buffer->m.limit) {
    printf(
        "INVALID b/c \"in\" full pointer is not between first and limit (%d, limit = %d)\n", buffer->m.in[CB_FULL],
        buffer->m.version);
    return -1;
  }

  if (buffer->m.out[CB_FULL] < buffer->m.first || buffer->m.out[CB_FULL] >= buffer->m.limit) {
    printf(
        "INVALID b/c \"out\" full pointer is not between first and limit (%d, limit = %d)\n", buffer->m.out[CB_FULL],
        buffer->m.limit);
    return -1;
  }

  if (buffer->m.in[CB_PARTIAL] < buffer->m.first || buffer->m.in[CB_PARTIAL] >= buffer->m.limit) {
    printf(
        "INVALID b/c \"in\" partial pointer is not between first and limit (%d, limit = %d)\n",
        buffer->m.in[CB_PARTIAL], buffer->m.version);
    return -1;
  }

  if (buffer->m.out[CB_PARTIAL] < buffer->m.first || buffer->m.out[CB_PARTIAL] >= buffer->m.limit) {
    printf(
        "INVALID b/c \"out\" partial pointer is not between first and limit (%d, limit = %d)\n",
        buffer->m.out[CB_PARTIAL], buffer->m.limit);
    return -1;
  }

  return 0;
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

  const char UNITS[] = {'\0', 'k', 'M', 'G'};

  while (amount > 1900.0 && unit < 3) {
    amount /= 1000.0;
    unit++;
  }

  if (unit == 0) {
    if (ceil(amount) == amount)
      sprintf(buffer, "%7.0f", amount);
    else
      sprintf(buffer, "%7.2f", amount);
  }
  else {
    sprintf(buffer, "%6.1f%c", amount, UNITS[unit]);
  }
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
  if (CB_check_integrity(buffer) < 0)
    return;

  const cb_stats_p stats = &buffer->stats;

  const uint64_t num_writes = stats->num_writes;
  const uint64_t num_reads  = stats->num_reads;

  char total_in_s[8], avg_in_s[8], total_out_s[8], avg_out_s[8], read_per_sec_s[8], write_per_sec_s[8], max_fill_s[8];

  const double avg_in  = num_writes > 0 ? (double)stats->num_write_elems / num_writes : 0.0;
  const double avg_out = num_reads > 0 ? (double)stats->num_read_elems / num_reads : 0.0;

  readable_element_count(stats->num_write_elems, total_in_s);
  readable_element_count(avg_in, avg_in_s);
  readable_element_count(stats->num_read_elems, total_out_s);
  readable_element_count(avg_out, avg_out_s);

  const double avg_wait_w       = num_writes > 0 ? (double)stats->total_write_wait_time_ms / num_writes : 0.0;
  const double avg_wait_r       = num_reads > 0 ? (double)stats->total_read_wait_time_ms / num_reads : 0.0;
  const double total_write_time = stats->total_write_time_ms;
  const double total_read_time  = stats->total_read_time_ms;
  readable_element_count(num_writes / total_write_time * 1000.0, write_per_sec_s);
  readable_element_count(num_reads / total_read_time * 1000.0, read_per_sec_s);

  readable_element_count(stats->max_fill, max_fill_s);
  const int max_fill_percent = (int)(stats->max_fill * 100.0 / CB_get_capacity(buffer));

  if (with_header) {
    printf("     "
           "                       Write (ms)                        |"
           "                       Read (ms)                         |\n"
           "rank "
           "   #elem  (#/call) : tot. time (#/sec) : wait ms (/call) |"
           "   #elem  (#/call) : tot. time (#/sec) : wait ms (/call) | "
           "max fill (%%)\n");
  }

  printf(
      "%04d: "
      "%s (%s) : %7.1f (%s) : %7.1f (%5.2f) | "
      "%s (%s) ; %7.1f (%s) : %7.1f (%5.1f) | "
      "%s (%3d)\n",
      buffer_id, total_in_s, avg_in_s, total_write_time, write_per_sec_s, stats->total_write_wait_time_ms, avg_wait_w,
      total_out_s, avg_out_s, total_read_time, read_per_sec_s, stats->total_read_wait_time_ms, avg_wait_r, max_fill_s,
      max_fill_percent);
}

//F_StArT
//  end interface
//F_EnD
#endif // DOXYGEN_SHOULD_SKIP_THIS
