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

//            circular buffer data layout 
//
//    (IN = OUT) (bufer empty) (LIMIT - FIRST -1 free slots)
// 
//  FIRST                                                   LIMIT
//    |                                                       |
//    v                                                       v
//    +------------------------------------------------------+
//    ........................................................
//    ^------------------------------------------------------+
//    |
//  IN/OUT
//    +------------------------------------------------------+
//    ........................................................
//    +--------------------^---------------------------------+
//                         |
//                       IN/OUT
// 
//    (IN = OUT - 1) (buffer full)
// 
//  FIRST                                                   LIMIT
//    |                                                       |
//    v                                                       v
//    +------------------------------------------------------+
//    xxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
//    +-------------------^^---------------------------------+
//                        ||
//                      IN  OUT
//    +------------------------------------------------------+
//    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
//    ^------------------------------------------------------^
//    |                                                      |
//   OUT                                                     IN
// 
//    (OUT < IN) (LIMIT - IN -1) free, (IN - OUT) data
//  FIRST                                                   LIMIT
//    |                                                       |
//    v                                                       v
//    +------------------------------------------------------+
//    xxxxxxxxxxxxxx..........................................
//    ^-------------^----------------------------------------+
//    |             |
//   OUT            IN
// 
//    (IN < OUT) (OUT - IN -1) free, (LIMIT - OUT + IN - FIRST) data
//  FIRST                                                   LIMIT
//    |                                                       |
//    v                                                       v
//    +------------------------------------------------------+
//    xxxxxxxxxxxxxx................................xxxxxxxxxx
//    +-------------^-------------------------------^--------+
//                  |                               |
//                  IN                             OUT
//    x = useful data       . = free space
//
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/types.h>
#include <sys/shm.h>

#include "circular_buffer.h"

#define SPACE_AVAILABLE(in,out,limit)  ((in < out) ? out-in-1 : limit-in+out-1)

#define DATA_AVAILABLE(in,out,limit)  ((in >= out) ? in-out : limit-out+in-1)

// interface


static inline void move_integers(int *dst, int*src, int n){
  memcpy(dst, src, sizeof(int)*n);
}
//F_StArT
//   function circular_buffer_init(p, nwords) result(buffer) bind(C,name='circular_buffer_init')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(IN), value :: nwords
//     type(C_PTR) :: buffer
//   end function circular_buffer_init
//F_EnD
// initialize a circular buffer
// nwords is the size in 32 bit elements of the memory area
// return pointer to buffer upon success, NULL otherwise
//C_StArT
circular_buffer_p circular_buffer_init(
  circular_buffer_p p, 
  int32_t nwords
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(nwords < 4096) return NULL;   // area is too small
  p->m.version = FIOL_VERSION;
  p->m.first = 0;
  p->m.in    = 0;
  p->m.out   = 0;
  p->m.limit = nwords - ( sizeof(fiol_management) / sizeof(int) );
  return p;
}
//F_StArT
//   function circular_buffer_create_shared(shmid, nwords) result(p) BIND(C,name='circular_buffer_create_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(OUT) :: shmid
//     integer(C_INT), intent(IN), value :: nwords
//     type(C_PTR) :: p
//   end function circular_buffer_create_shared
//F_EnD
// create and initialize a circular buffer of size nwords in "shared memory"
// return the "shared memory segment" address of the circular buffer upon success, NULL otherwise
// shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//C_StArT
circular_buffer_p circular_buffer_create_shared(
  int32_t *shmid, 
  int32_t nwords
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
//   function circular_buffer_detach_shared(p) result(status) BIND(C,name='circular_buffer_detach_shared')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT) :: status
//   end function circular_buffer_detach_shared
//F_EnD
// detach from a "shared memory segment" circular buffer 
// return 0 upon success, -1 otherwise
//C_StArT
int circular_buffer_detach_shared(
  circular_buffer_p p
  ){
//C_EnD
  if(p == NULL) return -1;
  return shmdt(p) ;   // detach from "shared memory segment" creeated by circular_buffer_create_shared
}
//F_StArT
//   function circular_buffer_create(nwords) result(p) BIND(C,name='circular_buffer_create')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(IN), value :: nwords
//     type(C_PTR) :: p
//   end function circular_buffer_create
//F_EnD
// create and initialize a circular buffer of size nwords
// return the address of the circular buffer upon success, NULL otherwise
//C_StArT
circular_buffer_p circular_buffer_create(
  int32_t nwords
  ){
//C_EnD
  circular_buffer_p t;
  size_t sz = nwords * sizeof(int);

  if(sz < 4096) return NULL;
  t = (circular_buffer_p ) malloc(sz);
  return circular_buffer_init(t, nwords) ;
}
//F_StArT
//   function circular_buffer_from_pointer(ptr, nwords) result(p) BIND(C,name='circular_buffer_from_pointer')
//     import :: C_PTR, C_INT
//     implicit none
//     integer(C_INT), intent(IN), value :: nwords
//     type(C_PTR), intent(IN), value :: ptr
//     type(C_PTR) :: p
//   end function circular_buffer_from_pointer
//F_EnD
// create and initialize a circular buffer, using supplied space of size nwords at address p
// return the address of the circular buffer upon success, NULL otherwise
//C_StArT
circular_buffer_p circular_buffer_from_pointer(
  void *p, 
  int32_t nwords
  ){
//C_EnD
  circular_buffer_p t;
  size_t sz = nwords * sizeof(int);

  if(sz < 4096) return NULL;
  t = (circular_buffer_p ) p;
  return circular_buffer_init(t, nwords) ;
}
//F_StArT
//   function circular_buffer_space_available(p) result(n) BIND(C,name='circular_buffer_space_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT) :: n
//   end function circular_buffer_space_available
//F_EnD
// return the current number of empty slots available, -1 on error
//C_StArT
int circular_buffer_space_available(
  circular_buffer_p p
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
//   function circular_buffer_wait_space_available(p, na) result(n) BIND(C,name='circular_buffer_wait_space_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(IN), value :: na
//     integer(C_INT) :: n
//   end function circular_buffer_wait_space_available
//F_EnD
// wait until at least n empty slots are available for inserting data
// return the actual number of empty slots available, -1 on error
//C_StArT
int circular_buffer_wait_space_available(
  circular_buffer_p p, 
  int n
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
//   function circular_buffer_data_available(p) result(n) BIND(C,name='circular_buffer_data_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT) :: n
//   end function circular_buffer_data_available
//F_EnD
// returns the current number of data tokens available, -1 on error
//C_StArT
int circular_buffer_data_available(
  circular_buffer_p p
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
//   function circular_buffer_wait_data_available(p, na) result(n) BIND(C,name='circular_buffer_wait_data_available')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(IN), value :: na
//     integer(C_INT) :: n
//   end function circular_buffer_wait_data_available
//F_EnD
// wait until at least n data tokens are available for extracting data
// return the actual number of data tokens available
// return -1 upon error
//C_StArT
int circular_buffer_wait_data_available(
  circular_buffer_p p, 
  int n
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
//   function circular_buffer_start(p) result(start) BIND(C,name='circular_buffer_start')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     type(C_PTR) :: start
//   end function circular_buffer_start
//F_EnD
// get the address of the first position in the circular data buffer
//C_StArT
int32_t *circular_buffer_start(
  circular_buffer_p p
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return p->data;  // start of data buffer
}
//F_StArT
//   function circular_buffer_data_in(p) result(inp) BIND(C,name='circular_buffer_data_in')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     type(C_PTR) :: inp
//   end function circular_buffer_data_in
//F_EnD
// returns a pointer to the  insertion point of the circular data buffer
// useful in conjunction with circular_buffer_data_snoop
//C_StArT
int32_t *circular_buffer_data_in(
  circular_buffer_p p
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return  p->data+p->m.in;
}
//F_StArT
//   function circular_buffer_data_out(p) result(outp) BIND(C,name='circular_buffer_data_out')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     type(C_PTR) :: outp
//   end function circular_buffer_data_out
//F_EnD
// return a pointer to the  extraction point of the circular data buffer
// useful in conjunction with circular_buffer_data_snoop
//C_StArT
int32_t *circular_buffer_data_out(
  circular_buffer_p p
  ){
//C_EnD
  if(p == NULL) return NULL;
  if(p->m.version != FIOL_VERSION) return NULL;
  return  p->data+p->m.out;
}
//F_StArT
//   function circular_buffer_advance_in(p, n1, n2) result(inp) BIND(C,name='circular_buffer_advance_in')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(OUT) :: n1
//     integer(C_INT), intent(OUT) :: n2
//     type(C_PTR) :: inp
//   end function circular_buffer_advance_in
//F_EnD
// return a pointer to the "in" position, assume that the caller knows the start of data buffer
// n1 slots available at "in", n2 slots available at "start"
// upon error, NULL is returned, and n1 and n2 are set to -1
//C_StArT
int32_t *circular_buffer_advance_in(
  circular_buffer_p p, 
  int32_t *n1, 
  int32_t *n2
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
//   function circular_buffer_advance_out(p, n1, n2) result(outp) BIND(C,name='circular_buffer_advance_out')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(OUT) :: n1
//     integer(C_INT), intent(OUT) :: n2
//     type(C_PTR) :: outp
//   end function circular_buffer_advance_out
//F_EnD
// return a pointer to the "out" position, assume that the caller knows the start of data buffer
// n1 tokens available at "out", n2 tokens available at "start"
// upon error, NULL is returned, and n1 and n2 are set to -1
//C_StArT
int32_t *circular_buffer_advance_out(
  circular_buffer_p p, 
  int32_t *n1, 
  int32_t *n2
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
//   function circular_buffer_atomic_get(p, dst, ndst) result(n) BIND(C,name='circular_buffer_atomic_get')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(IN), value :: ndst
//     integer(C_INT), dimension(*), intent(OUT) :: dst
//     integer(C_INT) :: n
//   end function circular_buffer_atomic_get
//F_EnD
// atomic extraction of n tokens into the dst array
// wait until n tokens are available
// return the number of data tokens available after this operation
// return -1 upon error
//C_StArT
int circular_buffer_atomic_get(
  circular_buffer_p p, 
  int *dst, 
  int n
  ){
//C_EnD
  int volatile *inp = &(p->m.in);
  int volatile *outp = &(p->m.out);
  int *buf = p->data;
  int in, out, limit, navail, ni;

  if(p == NULL || dst == NULL) return -1;
  if(p->m.version != FIOL_VERSION || n < 0) return -1;
  // wait until enough data is available
  limit = p->m.limit;
  navail = 0; in = 0 ; out = 0;
  while(navail <n){
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

// get n data tokens at position "out + offset"
// DO NOT UPDATE out unless update flag is non zero
// wait until n tokens are available at that position
// return the number of data tokens available after this operation
// return -1 upon error
//C_StArT
int circular_buffer_extract(
  circular_buffer_p p, 
  int *dst, 
  int n, 
  int offset, 
  int update
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
//   function circular_buffer_atomic_put(p, src, nsrc) result(n) BIND(C,name='circular_buffer_atomic_put')
//     import :: C_PTR, C_INT
//     implicit none
//     type(C_PTR), intent(IN), value :: p
//     integer(C_INT), intent(IN), value :: nsrc
//     integer(C_INT), dimension(*), intent(IN) :: src
//     integer(C_INT) :: n
//   end function circular_buffer_atomic_put
//F_EnD
// atomic insertion of n tokens from the src array
// wait until n free slots are available
// return the number of free slots available after this operation
// return -1 upon error
//C_StArT
int circular_buffer_atomic_put(
  circular_buffer_p p, 
  int *src, 
  int n
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
  while(navail <n){
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

// insertion of n tokens from the src array at position "in + offset"
// DO NOT UPDATE in unless update flag is non zero
// wait until n free slots are available
// return the number of free slots available after this operation
// return -1 upon error
//C_StArT
int circular_buffer_insert(
  circular_buffer_p p, 
  int *src, 
  int n, 
  int offset, 
  int update
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
// end interface
