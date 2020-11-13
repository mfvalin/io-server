/* functions for C and FORTRAN programming
 * Copyright (C) 2020  Recherche en Prevision Numerique
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

/**
 \file
 \brief quick and dirty heap management (C and Fortran)

 code extracted from file memory_arena.c
 \verbatim

  ServerHeap : A quick and dirty heap management package
               (mostly intended for use in managing shared memory across processes)

  Server Heap layout

  +------+---------------+    +---------------+     +---------------+------+
  |  NT  |    block 0    | ...|    block i    | ... |    block N    | 0/1  |
  +------+---------------+    +---------------+     +---------------+------+

  NT  :  total number of elements in entire Heap including NT itself

  Server Heap block i layout

  +------+----------------------------------+------+
  |  NHi |        NHi - 2 elements          |  NHi |
  +------+----------------------------------+------+
  <------------------ NHi elements ---------------->

  abs(NHi)     : number of elements in block, including NH itself, (must be >2)
                 value at end of block is always positive

  NHi >  2     : block is free
  NHi < -2     : block is in use
  NHi 0/1/-1   : last block (null block) (normally 0)
  NSi = abs(NHi)
  block[0]     : NHi, size of block + in use flag (at head)
  block[NSi-1] : size of block (at tail)

  Server Heap contents at creation

  +------+------+----------------------------------+------+------+
  |  NT  |  NH0 |   NH0 - 2 elements (free space)  |  NH0 |   0  |
  +------+------+----------------------------------+------+------+
         <------------------ NH elements ----------------->
  <------------------------- NT elementss ----------------------->
  NT  :  total number of elements in entire Heap
  NH0 :  NT - 2 (initial block with NT -2 elements)

  elements are expected to be 32 bit elements in this implementation

 \endverbatim
*/

//!> heap element (32 bits for now)
typedef int32_t heap_element ;     // "small" heap, with no more than 2*1024*1024*1024 - 1 elements (8 GBytes)

//! initialize a Server Heap
//! @return address of Server Heap if successful, NULL otherwise
void *ServerHeapInit(
  void *addr,                    //!< [in]  desired heap address, if NULL, allocate space with malloc
  size_t sz                      //!< [in]  size in bytes of space pointed to by addr
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element heap_sz = sz / sizeof(heap_element) ;

  if(h == NULL) h = (heap_element *) malloc(sz) ;
  if(h == NULL) return NULL ;

  h[0] = heap_sz ;              // size of heap
  h[1] = heap_sz -2 ;           // size of first block (head)
  h[heap_sz - 2] = heap_sz -2 ; // size of first block (tail)
  h[heap_sz - 1] = 0 ;          // last block (not locked)
  return h ; // O.K.
}

//! check integrity of Server Heap
//! @return 0 if successful, 0 if O.K., nonzero if not
int32_t ServerHeapCheck(
  void *addr,                     //!< [in]  address of Server Heap to check
  int32_t *free_blocks,           //!< [out] number of free blocks
  size_t *free_space,             //!< [out] available space in bytes
  int32_t *used_blocks,           //!< [out] number of used blocks
  size_t *used_space              //!< [out] used space in bytes
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;
  heap_element cur, limit ;
  int32_t free, used ;
  int32_t space_used, space_free ;

  *free_blocks = 0 ; free = 0 ;
  *free_space  = 0 ; used = 0 ;
  *used_blocks = 0 ; space_used = 0 ;
  *used_space  = 0 ; space_free = 0 ;
  if(addr == NULL) return 1;

  sz    = h[0] ;
  limit = sz - 1 ;

  if(h[limit] > 1  || h[limit] < -1) return 2  ;  // not a Server Heap or corrupted information

  free = 0 ; space_free = 0 ;
  used = 0 ; space_used = 0 ;
  for(cur = 1 ; cur < limit ; cur = cur + sz){  // go through block chain
    if(h[cur] == 0) break;
// printf("cur = %d, limit = %d", cur, limit);
    sz = (h[cur] > 0) ? h[cur] : - h[cur] ;     // size of block (negative value means block in use)
// printf(", sz = %d\n",sz);
    if(sz < 3 && sz > 0) return 3 ;                      // size must be > 2
    if(h[cur+sz-1] != sz) {
      printf("bad tail size, got %d, expected %d\n",h[cur+sz-1], sz);
      return 4 ;            // tail size not correct
    }
    if(h[cur] > 0){                             // free block
//       printf("free block\n");
      free++ ;
      space_free = space_free + sz - 2 ;
    }else{                                      // block in use
//       printf("used block\n");
      used++ ;
      space_used = space_used + sz - 2 ;
    }
  }
//   printf("free = %d, space_free = %d, used = %d, space_used = %d\n",free, space_free, used, space_used);
  *free_blocks = free ;
  *free_space  = space_free ;
  *used_blocks = used ;
  *used_space  = space_used ;
  return 0 ;
}

//! allocate space on a Server Heap
//! @return address of block
void *ServerHeapAllocBlock(
  void *addr,                      //!< [in]  address of Server Heap
  int32_t bsz,                     //!< [in]  size of block to allocate
  int32_t safe                     //!< [in]  if nonzero, perform operation under lock
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element sz, limit, cur, next ;
  heap_element *t ;

  sz = h[0] ;
  limit = sz - 1 ;
// printf("request block size = %d, sz = %d, limit = %d\n",bsz,sz,limit);
//   bsz *= sizeof(heap_element) ;
  t = NULL ;
  if(h[limit] > 1  || h[limit] < -1) return NULL  ;  // not a Server Heap or corrupted information
  if(safe){                                          // lock heap
    while(! __sync_bool_compare_and_swap(h + limit, 0, -1) ) ;  // wait for 0, then set to -1 to indicate lock
  }
  bsz += 2 ; // add head + tail elements
  for(cur = 1 ; cur < limit ; cur += sz){            // scan block list to find/make a large enough free block
// printf("cur = %d\n",cur);
    sz = (h[cur] < 0) ? -h[cur] : h[cur] ;           // abs(h[cur])
    if(h[cur] < 0) continue ;                        // block is not free
    next = cur + sz ;                                // next block
    while(next < limit && h[next] > 2) {             // next block is free
//       printf("coalescing blocks\n");
      sz += h[next] ;                                // coalesce blocks
      next = cur + sz ;                              // new next after block coalescing
      h[cur] = sz ;                                  // head size marker
      h[next - 1] = sz ;                             // tail size marker
    }
// printf("bsz = %d, sz = %d\n", bsz, sz);
    if(bsz <= sz){                               // block large enough to satisfy request
// printf("block is large enough, need %d, have %d\n", bsz, (sz-2) );
      t = h + cur + 1 ;                              // point to element following size marker
      if(sz - bsz > 64) { //  split block if worth it (more than 64 extra elements
// printf("splitting block cur = %d, bsz = %d, sz = %d, sz-bsz = %d\n", cur, bsz, sz, sz-bsz);
        h[cur]       =  -bsz ;      // head
        h[cur+bsz-1] =   bsz ;      // tail marker
        h[cur+bsz]    = sz - bsz ;  // head (next block)
        h[cur + sz - 1] = sz - bsz ;  // tail marker (next block)
      }
    }else{
//       printf("block is too small to allocate, need %d, have %d\n", bsz, sz-2);
    }
      break ;
  }
  if(safe){                                          // unlock heap
    h[limit] = 0 ;
  }
  return t ;
}

//! allocate space on a Server Heap
//! @return 0 if O.K., nonzero if error
int32_t ServerHeapFreeBlock(
  void *addr                       //!< [in]  address of block
    ){
  heap_element *h = (heap_element *) addr ;
  heap_element nw ;

  h-- ;                            // point to count (one element below block)
  nw = h[0];                       // if block is in use, nw will be negative
  if(nw >= 0) return -1 ;          // certainly not a block in use
  nw = -nw ;                       // make count positive
  if(h[nw - 1] != nw) return -1 ;  // backward count not found or not consistent
  h[0] = nw ;                      // mark memory block as free
  return 0;
}

//! check if Server Heap is locked
//! @return 0 if not locked, nonzero if locked
int32_t ServerHeapIslocked(
  void *addr                      //!< [in]  address of Server Heap
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  return (h[sz-1] != 0) ;    // 0 normally, non zero if locked
}

//! lock Server Heap
//! @return none
void ServerHeapLock(
  void *addr                      //!< [in]  address of Server Heap
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  h = h + sz - 1 ;                                     // last element
  while( ! __sync_bool_compare_and_swap(h, 0, -1) ) ;  // if zero, set to -1 to indicate lock
}

//! unlock Server Heap
//! @return none
void ServerHeapUnlock(
  void *addr                      //!< [in]  address of Server Heap
  ){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  h[sz-1] = 0 ;   // reset last element to unlocked value (zero)
}

#if defined(SELF_TEST)

#define NINDEXES  1024
#include <stdio.h>
#include <mpi.h>
int main ( int argc, char *argv[] )
{
  
  typedef struct{   // shared memory layout : nindx, index[nindx], heap [] 
    int nindx ;
    int *index ;
    int *heap ;
  } mem_layout;
  mem_layout sm = {0, NULL, NULL} ;
  void *myheap ;
  int32_t *t ;

  int rank, size, shared_elem = 0, i, status;
  MPI_Aint ssize; 
  int *shared;
  int errors = 0 ;
  int32_t free_blocks ;
  size_t free_space ;
  int32_t used_blocks ;
  size_t used_space ;

  MPI_Init ( &argc, &argv );
  MPI_Comm_rank ( MPI_COMM_WORLD, &rank );
  MPI_Comm_size ( MPI_COMM_WORLD, &size );
  MPI_Win win;

  if (rank == 0){         // process 0
    ssize = size * 1024 * 1024 * sizeof(int);          // process 0 allocates tha shared  memory
    MPI_Win_allocate_shared(ssize, sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &shared, &win);

    shared[0] = NINDEXES ;    // initialize basic sm structure and shared memory
    sm.nindx  = NINDEXES ;
    sm.index  = shared+1 ;
    sm.heap   = shared + 1 + NINDEXES ;
    for(i=0 ; i<NINDEXES ; i++) sm.index[i] = 0 ;

    myheap = ServerHeapInit(sm.heap, 32*1024) ; // create heap
    // Heap check (pristine Heap)
    status = ServerHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);

    for(i=0 ; i<64 ; i++){      // allocate 64 blocks if possible
      t = (int32_t *)ServerHeapAllocBlock(sm.heap, 1025, 0) ;    // try to allocate block
      if(t == NULL) {                                            // not enough room left
        printf("allocation failed, block number = %d\n",i);
        break ;
      }
      sm.index[i] = t - sm.heap ;                                // index displacement to block from base of heap
      printf("block %i allocated at %p, heap = %p, index = %d\n",i,t,sm.heap,sm.index[i]);
    }
    // Heap check (Heap with data blocks)
    status = ServerHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);
  }else{           // all other processes
    int disp_unit;
    ssize = 0;          // no extra memory for processes other than 0
    MPI_Win_allocate_shared(ssize, sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &shared, &win);
    MPI_Win_shared_query(win, 0, &ssize, &disp_unit, &shared);
  }
// windows allocated, all processes have base address of shared memory segment
  MPI_Barrier(MPI_COMM_WORLD) ;

  sm.nindx  = shared[0] ;    // get local addresses for basic sm structure
  sm.index  = shared+1 ;
  sm.heap   = shared + 1 + NINDEXES ;

  if(rank == 1){      // rank 1 process
    errors = 0 ;
    for(i=0 ; i<NINDEXES ; i++) if(sm.index[i] != 0 ) {
      printf("index[%d] = %d \n", i, sm.index[i]) ;
      errors++;
    }
    printf("process %d detected %d block(s), index size = %d\n", rank, errors, sm.nindx) ;
    status = ServerHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);
    // now free all allocated blocks
    for(i=0 ; i < NINDEXES ; i++) {
      if(sm.index[i] != 0) {
        ServerHeapFreeBlock(sm.heap + sm.index[i]) ;
        printf("block %d freed at index %d\n",i,sm.index[i]);
      }
      sm.index[i] = 0 ;
    }
    // Heap check (Heap after all blocks have been freed)
    status = ServerHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);
    t = (int32_t *)ServerHeapAllocBlock(sm.heap, 10, 0) ;
    sm.index[0] = t - sm.heap ;   // lone block just allocated
  }

  MPI_Barrier(MPI_COMM_WORLD) ;  // make sure everything rank 1 did is complete

// index check and Heap check by all processes
  for(i=0 ; i<NINDEXES ; i++) if(sm.index[i] != 0 ) {
    printf("index[%d] = %d \n", i, sm.index[i]) ;
  }
  // Heap check (there should only be 1 free block and 1 used block because of free block coalescing)
  status = ServerHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
  printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
          rank, free_blocks, free_space, used_blocks, used_space, status);

//=======================================================================================//
  MPI_Barrier(MPI_COMM_WORLD) ;        // NEEDED for the next part of the test code to work
//=======================================================================================//
  size = ssize / sizeof(int) ;
  if(rank==0){         // rank 0 populates shared memory
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, MPI_MODE_NOCHECK, win);
      for(i = 0; i < size; i++) shared[i] = 1000000000 + i ;
      MPI_Win_unlock(0,win);
  }
  MPI_Barrier(MPI_COMM_WORLD);

  // all processes check that the proper value is found
  errors = 0 ;  // all ranks check that shared memory contains expected value
  for(i = 0; i < size; i++) if(shared[i] != 1000000000 + i) errors++ ;
  printf("Process : %d, size = %d, errors = %d\n", rank, size, errors);

  MPI_Finalize ( );
  return 0 ;
}
#endif
