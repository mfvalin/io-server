/* 
 * Copyright (C) 2018  Environnement Canada
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
// !> code extracted from circular_buffer.c
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
// !> \endverbatim
//F_EnD //C_EnD
//C_StArT
*/
//C_EnD

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/types.h>
#include <sys/shm.h>

// #include "circular_buffer.h"
//C_StArT
#include <stdint.h>

#if ! defined(FIOL_VERSION)
//!> version marker
#define FIOL_VERSION 0x1BAD

//!> circular buffer management variables
//!> <br>in == out means buffer is empty
//!> <br>in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct{
  int32_t version;     //!< version marker
  int32_t first;       //!< should be 0 (assumed to be 0 in circular_buffer.c)
  int32_t in;          //!< start inserting data at data[in]
  int32_t out;         //!< start extracting data at data[out]
  int32_t limit;       //!< size of data buffer (last available index + 1)
} fiol_management;

//!> pointer to circular buffer management part
typedef fiol_management *fiol_management_p;

//!> skeleton for circular buffer
typedef struct{
  fiol_management m;   //!< management structure
  int32_t data[];      //!< data buffer (contains at most limit -1 useful data elements)
} circular_buffer;

//!> pointer to circular buffer 
typedef circular_buffer *circular_buffer_p;

#include <immintrin.h>

//!> memory store fence
#define W_FENCE __asm__ volatile("": : :"memory"); _mm_sfence();

//!> memory load fence
#define R_FENCE __asm__ volatile("": : :"memory"); _mm_lfence();

//!> memory load+store fence
#define M_FENCE __asm__ volatile("": : :"memory"); _mm_mfence();
#endif
//C_EnD

#define SPACE_AVAILABLE(in,out,limit)  ((in < out) ? out-in-1 : limit-in+out-1)

#define DATA_AVAILABLE(in,out,limit)  ((in >= out) ? in-out : limit-out+in)

static inline void move_integers(int *dst, int*src, int n){
  memcpy(dst, src, sizeof(int)*n);
}
//F_StArT
//   !> initialize a circular buffer<br>
//   !> buffer = circular_buffer_init(p, nwords)
//   function circular_buffer_init(p, nwords) result(buffer) bind(C,name='circular_buffer_init')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p           !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: nwords   !< the size in 32 bit elements of the circular buffer
//     type(C_PTR) :: buffer                         !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
//   end function circular_buffer_init
//F_EnD
//C_StArT
//! initialize a circular buffer
//! <br> = circular_buffer_init(p, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p circular_buffer_init(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int32_t nwords                           //!< [in]  size in 32 bit elements of the circular buffer
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(nwords < 128) return NULL;   // area is too small
  p->m.version = FIOL_VERSION;
  p->m.first = 0;
  p->m.in    = 0;
  p->m.out   = 0;
  p->m.limit = nwords - ( sizeof(fiol_management) / sizeof(int) );
  return p;
}
//F_StArT
//   !> create and initialize a circular buffer of size nwords in "shared memory"<br>
//   !> p = circular_buffer_create_shared(shmid, nwords)
//   function circular_buffer_create_shared(shmid, nwords) result(p) BIND(C,name='circular_buffer_create_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(OUT) :: shmid          !< identifier of shared memory area (see man shmget) (-1 upon error)
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer 
//   end function circular_buffer_create_shared
//F_EnD
// 
// return the "shared memory segment" address of the circular buffer upon success, NULL otherwise
// shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//C_StArT
//! create and initialize a circular buffer of size nwords in "shared memory", 
//! nwords in in 32 bit units<br>
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! <br> = circular_buffer_create_shared(&shmid, nwords)
//! @return pointer to buffer upon success, NULL upon error
circular_buffer_p circular_buffer_create_shared(
  int32_t *shmid,                          //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
  int32_t nwords                           //!< [in]  size in 32 bit elements of the circular buffer
  ){
//C_EnD
  void *t;
  size_t sz = nwords * sizeof(int);
  int id;
  struct shmid_ds ds;
  int status;

  *shmid = -1;
  if(sz < 64*1024) return NULL;
  id = shmget(IPC_PRIVATE, sz, IPC_CREAT);   // create shared memory segment
  if(id == -1) return NULL;                  // error occurred
  t = shmat(id, NULL, 0);                    // attach shared memory segment
  if( t == (void *) -1) return NULL;         // error occurred
  status = shmctl(id, IPC_RMID, &ds);        // mark segment for deletion (ONLY SAFE ON LINUX)
  if(status != 0) return NULL;               // this should not fail
  *shmid = id;
  return circular_buffer_init((circular_buffer_p)t, nwords) ;
}
//F_StArT
//   !> detach "shared memory segment" used by circular buffer <br>
//   !> status = circular_buffer_detach_shared(p)
//   function circular_buffer_detach_shared(p) result(status) BIND(C,name='circular_buffer_detach_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     integer(C_INT) :: status               !< 0 upon success, -1 upon error
//   end function circular_buffer_detach_shared
//F_EnD
//C_StArT
//! detach "shared memory segment" used by circular buffer
//! <br> = circular_buffer_detach_shared
//! @return 0 upon success, nonzero upon error
int32_t circular_buffer_detach_shared(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  if(p == NULL) return -1;
  return shmdt(p) ;   // detach from "shared memory segment" creeated by circular_buffer_create_shared
}
//F_StArT
//   !> create and initialize a circular buffer of size nwords in process memory<br>
//   !> p = circular_buffer_create(nwords)
//   function circular_buffer_create(nwords) result(p) BIND(C,name='circular_buffer_create')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer 
//   end function circular_buffer_create
//F_EnD
//C_StArT
//! create and initialize a circular buffer of size nwords in process memory
//! <br> = circular_buffer_create(nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p circular_buffer_create(
  int32_t nwords                           //!< [in]  size in 32 bit elements of the circular buffer
  ){
//C_EnD
  circular_buffer_p t;
  size_t sz = nwords * sizeof(int);

  if(sz < 128) return NULL;
  t = (circular_buffer_p ) malloc(sz);
  return circular_buffer_init(t, nwords) ;
}
//F_StArT
//   !> create and initialize a circular buffer of size nwords from user supplied memory<br>
//   !> p = circular_buffer_from_pointer(ptr, nwords)
//   function circular_buffer_from_pointer(ptr, nwords) result(p) BIND(C,name='circular_buffer_from_pointer')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: ptr         !< pointer to user supplied memory
//     integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
//     type(C_PTR) :: p                              !< pointer to created circular buffer 
//   end function circular_buffer_from_pointer
//F_EnD
//C_StArT
//! create and initialize a circular buffer, using supplied space
//! <br> = circular_buffer_from_pointer(p, nwords)
//! @return address of the circular buffer upon success, NULL otherwise
circular_buffer_p circular_buffer_from_pointer(
  void *p,                                 //!< [in]  pointer to user supplied memory space
  int32_t nwords                           //!< [in]  size in 32 bit elements of the circular buffer
  ){
//C_EnD
  circular_buffer_p t;
  size_t sz = nwords * sizeof(int);

  if(sz < 128) return NULL;
  t = (circular_buffer_p ) p;
  return circular_buffer_init(t, nwords) ;
}
//F_StArT
//   !> return the current number of empty slots available<br>
//   !> n = circular_buffer_space_available(p)
//   function circular_buffer_space_available(p) result(n) BIND(C,name='circular_buffer_space_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     integer(C_INT) :: n                    !< current number of empty slots available, -1 if error
//   end function circular_buffer_space_available
//F_EnD
//C_StArT
//! return the current number of empty slots available
//! <br> = circular_buffer_space_available(p)
//! @return 
int32_t circular_buffer_space_available(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  int  *inp = &(p->m.in);
  int  *outp = &(p->m.out);
  int in, out, limit;

  if(p == NULL) return -1;
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  return SPACE_AVAILABLE(in,out,limit);
}
//F_StArT
//   !> wait until at least na empty slots are available for inserting data<br>
//   !> n = circular_buffer_wait_space_available(p, na)
//   function circular_buffer_wait_space_available(p, na) result(n) BIND(C,name='circular_buffer_wait_space_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p          !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: na      !< needed number of available slots
//     integer(C_INT) :: n                          !< actual number of empty slots available, -1 on error
//   end function circular_buffer_wait_space_available
//F_EnD
//C_StArT
//! wait until at least na empty slots are available for inserting data
//! <br> = circular_buffer_wait_space_available(p, n)
//! @return actual number of empty slots available, -1 on error
int32_t circular_buffer_wait_space_available(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int n                                    //!< [in]  needed number of available slots
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int in, out, limit, navail;

  if(p == NULL) return -1;
  if(n < 0 || p->m.version != FIOL_VERSION) return -1;
  limit = p->m.limit;
  navail = 0;
  while(navail <n){
    in = *inp;
    out = *outp;
    navail = SPACE_AVAILABLE(in,out,limit);
  }
  return navail;
}
//F_StArT
//   !> get the current number of data tokens available<br>
//   !> p = circular_buffer_data_available(p)
//   function circular_buffer_data_available(p) result(n) BIND(C,name='circular_buffer_data_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer 
//     integer(C_INT) :: n                            !< current number of data tokens available, -1 if error
//   end function circular_buffer_data_available
//F_EnD
//C_StArT
//! get the current number of data tokens available
//! <br> = circular_buffer_data_available(p)
//! @return current number of data tokens available, -1 if error
int32_t circular_buffer_data_available(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  int  *inp = &(p->m.in);
  int  *outp = &(p->m.out);
  int in, out, limit;

  if(p == NULL) return -1;
  if(p->m.version != FIOL_VERSION) return -1;
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  return DATA_AVAILABLE(in,out,limit);
}
//F_StArT
//   !> wait until at least n data tokens are available for extracting data<br>
//   !> p = circular_buffer_wait_data_available(p, na)
//   function circular_buffer_wait_data_available(p, na) result(n) BIND(C,name='circular_buffer_wait_data_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: na        !< needed number of available tokens
//     integer(C_INT) :: n                            !< actual number of data tokens available, -1 if error
//   end function circular_buffer_wait_data_available
//F_EnD
//C_StArT
//! wait until at least n data tokens are available for extracting data
//! <br> = circular_buffer_wait_data_available(p, n)
//! @return actual number of data tokens available, -1 if error
int32_t circular_buffer_wait_data_available(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int n                                    //!< [in]  needed number of available tokens
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int in, out, limit, navail;

  if(p == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  limit = p->m.limit;
  navail = 0;
  while(navail <n){
    in = *inp;
    out = *outp;
    navail = DATA_AVAILABLE(in,out,limit);
  }
  return navail;
}
//F_StArT
//   !> get the address of the first position in the circular data buffer<br>
//   !> start = circular_buffer_start(p)
//   function circular_buffer_start(p) result(start) BIND(C,name='circular_buffer_start')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     type(C_PTR) :: start                   !< pointer to beginning of circular buffer
//   end function circular_buffer_start
//F_EnD
// 
//C_StArT
//! get the address of the first position in the circular data buffer
//! <br> = circular_buffer_start(p)
//! @return pointer to beginning of circular buffer
int32_t *circular_buffer_start(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return p->data;  // start of data buffer
}
//F_StArT
//   !> get the address of the  insertion point in the circular data buffer (data snoop)<br>
//   !> inp = circular_buffer_data_in(p)
//   function circular_buffer_data_in(p) result(inp) BIND(C,name='circular_buffer_data_in')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     type(C_PTR) :: inp                     !< address of the  insertion point in the circular data buffer
//   end function circular_buffer_data_in
//F_EnD
//C_StArT
//! get the address of the  insertion point in the circular data buffer (data snoop)
//! <br> = circular_buffer_data_in(p)
//! @return address of the  insertion point in the circular data buffer
int32_t *circular_buffer_data_in(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return  p->data+p->m.in;
}
//F_StArT
//   !> get the address of the extraction point in the circular data buffer (data snoop)<br>
//   !> outp = circular_buffer_data_out(p)
//   function circular_buffer_data_out(p) result(outp) BIND(C,name='circular_buffer_data_out')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     type(C_PTR) :: outp                    !< address of the extraction point in the circular data buffer
//   end function circular_buffer_data_out
//F_EnD
//C_StArT
//! get the address of the extraction point in the circular data buffer (data snoop)
//! <br> = circular_buffer_data_out(p)
//! @return address of the  insertion point in the circular data buffer
int32_t *circular_buffer_data_out(
  circular_buffer_p p                    //!< [in]  pointer to a circular buffer
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return  p->data+p->m.out;
}
//F_StArT
//   !> get pointer to the in position, assume that the caller knows the start of data buffer<br>
//   !> inp = circular_buffer_advance_in(p, n1, n2)
//   function circular_buffer_advance_in(p, n1, n2) result(inp) BIND(C,name='circular_buffer_advance_in')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     integer(C_INT), intent(OUT)    :: n1   !< number of slots available at the "in" position, -1 upon error
//     integer(C_INT), intent(OUT)    :: n2   !< number of slots available at the "start" of the buffer, -1 upon error
//     type(C_PTR)                    :: inp  !< pointer to the "in" position, C_NULL_PTR upon error
//   end function circular_buffer_advance_in
//F_EnD
//C_StArT
//! get pointer to the in position, assume that the caller knows the start of data buffer
//! <br> = circular_buffer_advance_in(p, &n1, &n2)
//! @return 
int32_t *circular_buffer_advance_in(
  circular_buffer_p p,                    //!< [in]  pointer to a circular buffer
  int32_t *n1,                            //!< [out] number of tokens available at the "in" position, -1 upon error
  int32_t *n2                             //!< [out] number of tokens available at the "start" of the buffer, -1 upon error
  ){
//C_EnD
  int  *inp = &(p->m.in);
  int  *outp = &(p->m.out);
  int in, out, limit;

  *n1 = -1;
  *n2 = -1;
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  if(in == out){
    if(in == 0){
      *n1 = limit - 1;      // "in" -> "limit -2"  (in to end -1)
      *n2 = 0;              // nothing available at beginning of buffer
    }else{
      *n1 = limit - in;     // "in" -> "limit -1"  (in to end)
      *n2 = out - 1;        // "first" -> "out -1"
    }
  }
  else if(in < out){
    *n1 = out - in - 1;     // available at "in"
    *n2 = 0;                // nothing available at beginning of buffer
  }
  else if(in > out){
    *n1 = limit - in - 1;   // "in" -> "limit -1"
    *n2 = out;              // available at beginning of buffer (technically out - first)
  }
  return p->data+in;
}
//F_StArT
//   !> get pointer to the "out" position, assume that the caller knows the start of data buffer<br>
//   !> outp = circular_buffer_advance_out(p, n1, n2)
//   function circular_buffer_advance_out(p, n1, n2) result(outp) BIND(C,name='circular_buffer_advance_out')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer 
//     integer(C_INT), intent(OUT)    :: n1   !< number of tokens available at the "out" position, -1 upon error
//     integer(C_INT), intent(OUT)    :: n2   !< number of tokens available at the "start" of the buffer, -1 upon error
//     type(C_PTR)                    :: outp !< pointer to the "out" position, C_NULL_PTR upon error
//   end function circular_buffer_advance_out
//F_EnD
//C_StArT
//! return a pointer to the "out" position, assume that the caller knows the start of data buffer
//! <br> = circular_buffer_advance_out(p, &n1, &n2)
//! @return pointer to the "out" position, upon error, NULL is returned
int32_t *circular_buffer_advance_out(
  circular_buffer_p p,                   //!< [in]  pointer to a circular buffer
  int32_t *n1,                           //!< [out] number of tokens available at the "out" position, -1 upon error
  int32_t *n2                            //!< [out] number of tokens available at the "start" of the buffer, -1 upon error
  ){
//C_EnD
  int  *inp = &(p->m.in);
  int  *outp = &(p->m.out);
  int in, out, limit;

  *n1 = -1;
  *n2 = -1;
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  if(in == out){
    *n1 = 0;            // nothing at "out"
    *n2 = 0;            // nothing at beginning of buffer
  }
  else if(in < out){
    *n1 = limit - out;  // available at "out"
    *n2 = in;           // available at beginning of buffer (technically in - first)
  }
  else if(in > out){
    *n1 = in - out;     // "out" -> "in - 1"
    *n2 = 0;            // nothing at beginning of buffer
  }
  return p->data+out;
}
//F_StArT
//   !> wait until ndst tokens are available then extract them into dst<br>
//   !> n = circular_buffer_atomic_get(p, dst, ndst)
//   function circular_buffer_atomic_get(p, dst, ndst) result(n) BIND(C,name='circular_buffer_atomic_get')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: ndst          !< number of tokens to extract
//     integer(C_INT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
//     integer(C_INT) :: n                                !< number of data tokens available after this operation, -1 if error
//   end function circular_buffer_atomic_get
//F_EnD
//C_StArT
//! wait until n tokens are available then extract them into dst
//! <br> = circular_buffer_atomic_get(p, dst, n)
//! @return number of data tokens available after this operation, -1 if error
int32_t circular_buffer_atomic_get(
  circular_buffer_p p,                       //!< [in]  pointer to a circular buffer
  int *dst,                                  //!< [out] destination array for data extraction
  int n                                      //!< [in]  number of data items to extract
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int *buf = p->data;
  int in, out, limit, navail, ni;
  useconds_t delay = 10;   // 10 microseconds

  if(p == NULL || dst == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  // wait until enough data is available
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  navail = DATA_AVAILABLE(in,out,limit);
  while(navail <n){
    usleep(delay);
    in = *inp;
    out = *outp;
    navail = DATA_AVAILABLE(in,out,limit);
  }

  if(out < in){         // 1 segment
    move_integers(dst, buf+out, n);
    out += n;
  }else{                // 1 or 2 segments
    ni = n > (limit-out) ? (limit-out) : n;
    move_integers(dst, buf+out, ni);
    n -= ni;
    out += ni;
    dst += ni;
    if(out >= limit) out = 0;
    move_integers(dst, buf+out, n);
    out += n;
  }
  M_FENCE;  // memory fence, make sure everything fetched and stored before adjusting the "out" pointer
  *outp = out;
  in = *inp;
//   out = *outp;
  return DATA_AVAILABLE(in,out,limit);
}

//F_StArT
//   !> wait until ndst tokens are available then copy to dst array<br>
//   !> DO NOT UPDATE "out" unless update flag is non zero<br>
//   !> n = circular_buffer_extract(p, dst, ndst, offset, update)
//   function circular_buffer_extract(p, dst, ndst, offset, update) result(n) BIND(C,name='circular_buffer_extract')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: ndst          !< number of tokens to copy to dst
//     integer(C_INT), dimension(*), intent(OUT) :: dst   !< destination array for data extraction
//     integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
//     integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
//     integer(C_INT) :: n                                !< number of free slots available after this operation
//   end function circular_buffer_extract
//F_EnD
//C_StArT
//! get n data tokens at position "out + offset", 
//! wait until n tokens are available at that position, 
//! DO NOT UPDATE "out" unless update flag is non zero
//! <br> = circular_buffer_extract(p, dst, n, offset, update)
//! @return number of data tokens available after this operation, -1 upon error
int32_t circular_buffer_extract(
  circular_buffer_p p,                      //!< [in]  pointer to a circular buffer
  int *dst,                                 //!< [out] destination array for data extraction
  int n,                                    //!< [in]  number of data items to extract
  int offset,                               //!< [in]  offset from the "out" position
  int update                                //!< [in]  if nonzero, update the "out" pointer
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int *buf = p->data;
  int in, out, limit, navail, ni;

  if(p == NULL || dst == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  // wait until enough data is available
  limit = p->m.limit;  // first is assumed to be 0
  navail = 0; in = 0 ;
  out = *outp;
  while(navail < (n + offset)){  // we need n tokens after position "out + offset" (modulo limit)
    in = *inp;
    navail = DATA_AVAILABLE(in,out,limit);
  }

  out = out + offset ;  // acccount for offset
  if(out >= limit) out = out - limit;

  if(out < in){         // 1 segment
    move_integers(dst, buf+out, n);
    out += n;
  }else{                // 1 or 2 segments
    ni = n > (limit-out) ? (limit-out) : n;
    move_integers(dst, buf+out, ni);
    n -= ni;
    out += ni;
    dst += ni;
    if(out >= limit) out = 0;
    move_integers(dst, buf+out, n);
    out += n;
  }
  if(update) { M_FENCE ; *outp = out; }  // memory fence, make sure everything fetched and stored before adjusting the "out" pointer
  in = *inp;
  return DATA_AVAILABLE(in,out,limit);
}
//F_StArT
//   !> wait until nsrc free slots are available then insert from src array<br>
//   !> n = circular_buffer_atomic_put(p, src, nsrc)
//   function circular_buffer_atomic_put(p, src, nsrc) result(n) BIND(C,name='circular_buffer_atomic_put')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
//     integer(C_INT), dimension(*), intent(IN) :: src    !< source array for data insertion
//     integer(C_INT) :: n                                !< number of free slots available after this operation
//   end function circular_buffer_atomic_put
//F_EnD
//C_StArT
//! wait until nsrc free slots are available then insert from src array
//! <br> = circular_buffer_atomic_put(p, src, n)
//! @return number of free slots available after this operation, -1 upon error
int32_t circular_buffer_atomic_put(
  circular_buffer_p p,                     //!< [in]  pointer to a circular buffer
  int *src,                                //!< [in]  source array for data insertion
  int n                                    //!< [in]  number of data items to insert
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int *buf = p->data;
  int in, out, limit, navail, ni;
  useconds_t delay = 10;    // 10 microseconds

  if(p == NULL || src == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  // wait until there is enough room to insert data
  limit = p->m.limit;
  in = *inp;
  out = *outp;
  navail = SPACE_AVAILABLE(in,out,limit);
  while(navail <n){
    usleep(delay);
    in = *inp;
    out = *outp;
    navail = SPACE_AVAILABLE(in,out,limit);
  }

  if(in < out){         // 1 segment
    move_integers(buf+in, src, n);
    in += n;
  }else{                // 1 or 2 segments
    ni = n > (limit-in) ? (limit-in) : n;
    move_integers(buf+in, src, ni);
    n -= ni;
    in += ni;
    src += ni;
    if(in >= limit) in = 0;
    move_integers(buf+in, src, n);
    in += n;
  }
  W_FENCE ;  // write fence, make sure everything is in memory before adjusting the "in" pointer
  *inp = in;
//   in = *inp;
  out = *outp;
  return SPACE_AVAILABLE(in,out,limit);
}
//F_StArT
//   !> wait until nsrc free slots are available then insert from src array<br>
//   !> DO NOT UPDATE the "in" pointer unless update flag is non zero<br>
//   !> n = circular_buffer_insert(p, src, nsrc, offset, update)
//   function circular_buffer_insert(p, src, nsrc, offset, update) result(n) BIND(C,name='circular_buffer_insert')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer 
//     integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
//     integer(C_INT), dimension(*), intent(IN) :: src    !< source array for data insertion
//     integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
//     integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
//     integer(C_INT) :: n                                !< number of free slots available after this operation
//   end function circular_buffer_insert
//F_EnD
//C_StArT
//! insert n tokens from the src array at position "in + offset", 
//! wait until n free slots are available, 
//! DO NOT UPDATE the "in" pointer unless update flag is non zero
//! <br> = circular_buffer_insert(p, src, n, offset, update)
//! @return number of free slots available after this operation, -1 upon error
int32_t circular_buffer_insert(
  circular_buffer_p p,                    //!< [in]  pointer to a circular buffer
  int *src,                               //!< [in]  source array for data insertion
  int n,                                  //!< [in]  number of data items to insert
  int offset,                             //!< [in]  offset from the "in" position
  int update                              //!< [in]  if nonzero, update the "in" pointer
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int *buf = p->data;
  int in, out, limit, navail, ni;

  if(p == NULL || src == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  // wait until there is enough room to insert data
  limit = p->m.limit;
  navail = 0; in = 0 ; out = 0;
  in = *inp;
  while(navail < (n + offset)){  // we need to insert n tokens after position "in + offset" (modulo limit)
    out = *outp;
    navail = SPACE_AVAILABLE(in,out,limit);
  }

  in = in + offset ;    // acccount for offset
  if(in >= limit) in = in - limit;

  if(in < out){         // 1 segment
    move_integers(buf+in, src, n);
    in += n;
  }else{                // 1 or 2 segments
    ni = n > (limit-in) ? (limit-in) : n;
    move_integers(buf+in, src, ni);
    n -= ni;
    in += ni;
    src += ni;
    if(in >= limit) in = 0;
    move_integers(buf+in, src, n);
    in += n;
  }
  if(update) { W_FENCE ; *inp = in; }   // write fence, make sure everything is in memory before adjusting the "in" pointer
  out = *outp;
  return SPACE_AVAILABLE(in,out,limit);
}

#endif