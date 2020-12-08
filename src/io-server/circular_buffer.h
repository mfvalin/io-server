#ifndef IO_SERVER_circular_buffer_GEN_H
#define IO_SERVER_circular_buffer_GEN_H

/**
 \file
 \brief circular buffer package (C and Fortran)

 code extracted from circular_buffer.c
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
 \endverbatim
*/
#include "io-server/common.h"

#if ! defined(FIOL_VERSION)
//!> version marker
#define FIOL_VERSION 0x1BAD

static const int MIN_CIRC_BUFFER_SIZE = 128; //!> Minimum size of a circular buffer, in number of #data_element

//!> circular buffer management variables
//!> <br>in == out means buffer is empty
//!> <br>in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct{
  data_element version; //!< version marker
  data_index first;     //!< should be 0 (assumed to be 0 in circular_buffer.c)
  data_index in;        //!< start inserting data at data[in]
  data_index out;       //!< start extracting data at data[out]
  data_index limit;     //!< size of data buffer (last available index + 1)
} fiol_management;

//! pointer to circular buffer management part
typedef fiol_management *fiol_management_p;

//! skeleton for circular buffer
typedef struct{
  fiol_management m;   //!< management structure
  data_element data[]; //!< data buffer (contains at most limit -1 useful data elements)
} circular_buffer;

//! pointer to circular buffer
typedef circular_buffer *circular_buffer_p;


//! @brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline data_index available_space(
    const data_index in,   //!< [in] Index of insertion location in the buffer
    const data_index out,  //!< [in] Index of extraction location in the buffer
    const data_index limit //!< [in] Number of elements that the buffer can hold
) {
  return (in < out) ? out - in - 1 : limit - in + out - 1;
}

//! @brief Compute how much data is stored in a circular buffer, given of set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
static inline data_index available_data(
    const data_index in,   //!< [in] Index of insertion location in the buffer
    const data_index out,  //!< [in] Index of extraction location in the buffer
    const data_index limit //!< [in] Number of elements that the buffer can hold
) {
  return (in >= out) ? in - out : limit - out + in;
}

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

#endif
//! Print buffer header (to help debugging)
void circular_buffer_print_header(
    circular_buffer_p b   //!< [in] Pointer to the buffer to print
);
//! initialize a circular buffer
//! <br> = circular_buffer_init(p, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p circular_buffer_init(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int32_t nwords                           //!< [in]  size in number of elements of the circular buffer (#data_element)
  );
//! create and initialize a circular buffer of size nwords in "shared memory",
//! nwords in in 32 bit units<br>
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = circular_buffer_create_shared(&shmid, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p circular_buffer_create_shared(
  int32_t *shmid,                          //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
  int32_t nwords                           //!< [in]  size in number of elements of the circular buffer (#data_element)
  );
//! detach "shared memory segment" used by circular buffer
//! <br> = circular_buffer_detach_shared
//! @return 0 upon success, nonzero upon error
int32_t circular_buffer_detach_shared(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  );
//! create and initialize a circular buffer of size nwords in process memory
//! <br> = circular_buffer_create(nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p circular_buffer_create(
  int32_t nwords                           //!< [in]  size in number of elements of the circular buffer (#data_element)
  );
//! create and initialize a circular buffer, using supplied space
//! <br> = circular_buffer_from_pointer(p, nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p circular_buffer_from_pointer(
  void *p,                                 //!< [in]  pointer to user supplied memory space
  int32_t nwords                           //!< [in]  size in number of elements of the circular buffer (#data_element)
  );
//! Compute how much space (in number of #data_element) is available in a given circular buffer
//! @return How many elements can still be added
data_index circular_buffer_get_available_space(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! Compute how much data (in number of #data_element) is stored in a given circular buffer
//! @return How many elements are stored in the buffer
data_index circular_buffer_get_available_data(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
);
//! wait until at least na empty slots are available for inserting data
//! <br> = circular_buffer_wait_space_available(p, n)
//! @return actual number of empty slots available, -1 on error
int32_t circular_buffer_wait_space_available(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int n                                    //!< [in]  needed number of available slots (#data_element)
  );
//! wait until at least n data tokens are available for extracting data
//! <br> = circular_buffer_wait_data_available(p, n)
//! @return actual number of data tokens available, -1 if error
int32_t circular_buffer_wait_data_available(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int n                                    //!< [in]  needed number of available  #data_element tokens
  );
//! get the address of the first position in the circular data buffer
//! <br> = circular_buffer_start(p)
//! @return pointer to beginning of circular buffer
data_element *circular_buffer_start(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  );
//! get the address of the  insertion point in the circular data buffer (data snoop)
//! <br> = circular_buffer_data_in(p)
//! @return address of the  insertion point in the circular data buffer
data_element *circular_buffer_data_in(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  );
//! get the address of the extraction point in the circular data buffer (data snoop)
//! <br> = circular_buffer_data_out(p)
//! @return address of the  insertion point in the circular data buffer
data_element *circular_buffer_data_out(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  );
//! get pointer to the in position, assume that the caller knows the start of data buffer
//! <br> = circular_buffer_advance_in(p, &n1, &n2)
//! @return A pointer to the insertion position
data_element *circular_buffer_advance_in(
  circular_buffer_p p,                    //!< [in]  pointer to a circular buffer
  int32_t *n1,                            //!< [out] number of #data_element tokens available at the "in" position, -1 upon error
  int32_t *n2                             //!< [out] number of #data_element tokens available at the "start" of the buffer, -1 upon error
  );
//! return a pointer to the "out" position, assume that the caller knows the start of data buffer
//! <br> = circular_buffer_advance_out(p, &n1, &n2)
//! @return pointer to the "out" position, upon error, NULL is returned
data_element *circular_buffer_advance_out(
  circular_buffer_p p,                   //!< [in]  pointer to a circular buffer
  int32_t *n1,                           //!< [out] number of #data_element tokens available at the "out" position, -1 upon error
  int32_t *n2                            //!< [out] number of #data_element tokens available at the "start" of the buffer, -1 upon error
  );
//! wait until n tokens are available then extract them into dst
//! <br> = circular_buffer_atomic_get(p, dst, n)
//! @return number of data tokens available after this operation, -1 if error
int32_t circular_buffer_atomic_get(
  circular_buffer_p p,                       //!< [in]  pointer to a circular buffer
  data_element *dst,                         //!< [out] destination array for data extraction
  int n                                      //!< [in]  number of #data_element data items to extract
  );
//! get n data tokens at position "out + offset",
//! wait until n tokens are available at that position,
//! DO NOT UPDATE "out" unless update flag is non zero
//! <br> = circular_buffer_extract(p, dst, n, offset, update)
//! @return number of data tokens available after this operation, -1 upon error
int32_t circular_buffer_extract(
  circular_buffer_p p,                      //!< [in]  pointer to a circular buffer
  data_element *dst,                          //!< [out] destination array for data extraction
  int n,                                    //!< [in]  number of #data_element data items to extract
  int offset,                               //!< [in]  offset from the "out" position
  int update                                //!< [in]  if nonzero, update the "out" pointer
  );
//! wait until nsrc free slots are available then insert from src array
//! <br> = circular_buffer_atomic_put(p, src, n)
//! @return number of free slots available after this operation, -1 upon error
int32_t circular_buffer_atomic_put(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  data_element *src,                         //!< [in]  source array for data insertion
  int n                                    //!< [in]  number of #data_element data items to insert
  );
//! insert n tokens from the src array at position "in + offset",
//! wait until n free slots are available,
//! DO NOT UPDATE the "in" pointer unless update flag is non zero
//! <br> = circular_buffer_insert(p, src, n, offset, update)
//! @return number of free slots available after this operation, -1 upon error
int32_t circular_buffer_insert(
  circular_buffer_p p,                    //!< [in]  pointer to a circular buffer
  data_element *src,                        //!< [in]  source array for data insertion
  int n,                                  //!< [in]  number of #data_element data items to insert
  int offset,                             //!< [in]  offset from the "in" position
  int update                              //!< [in]  if nonzero, update the "in" pointer
  );

#endif // IO_SERVER_circular_buffer_GEN_H
