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

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

/*

  ServerHeap : A quick and dirty heap management system
               (mostly for use in managing shared memory across processes)

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

  NHi >  2   : block is free
  NHi < -2   : block is in use
  NHi 0/1/-1 : last block (null block) (normally 0)

  Server Heap contents at creation

  +------+------+----------------------------------+------+------+
  |  NT  |  NH0 |   NH0 - 2 elements (free space)  |  NH0 |   0  |
  +------+------+----------------------------------+------+------+
         <------------------ NH elements ----------------->
  <------------------------- NT elementss ----------------------->
  NT  :  total number of elements in entire Heap
  NH0 :  NT - 2 (initial block with NT -2 elements)

  elements are normally expected to be 32 bit elements

*/

typedef int32_t heap_element ;     // "small" heap, with no more than 2*1024*1024*1024 - 1 elements (8 GBytes)

// initialize Server Heap
// if addr is NULL, allocate space with malloc  
// sz size in bytes of space pointed to by addr
// return is address of Server Heap if successful, NULL otherwise
void * ServerHeapInit(void *addr, size_t sz){
  heap_element *h = (heap_element *) addr ;
  heap_element heap_sz = sz / sizeof(heap_element) ;

  if(h == NULL) h = (heap_element *) malloc(sz) ;
  if(h == NULL) return NULL ;

  h[0] = heap_sz ;         // size of heap
  h[1] = heap_sz -2 ;      // size of first block (head)
  h[sz - 2] = heap_sz -2 ; // size of first block (tail)
  h[sz - 1] = 0 ;          // last block (not locked)

  return h ; // O.K.
}

// check integrity of Server Heap
// return 0 if O.K., 1 if not
// addr : address of Server Heap to check
// output arguments
//   free_blocks  : number of free blocks
//   used_blocks  : number of used blocks
//   free_space   : available space in bytes
//   used_space   : used space in bytes
// 
int32_t ServerHeapCheck(void *addr, int *free_blocks, size_t *free_space, int *used_blocks, size_t *used_space){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;
  heap_element cur, limit ;
  int free, used ;
  int64_t space_used, space_free ;

  *free_blocks = 0 ;
  *free_space  = 0 ;
  *used_blocks = 0 ;
  *used_space  = 0 ;
  if(addr == NULL) return 1;

  sz    = h[0] ;
  limit = sz - 1 ;
  if(h[limit] > 1  || h[limit] < -1) return 1  ;  // not a Server Heap or corrupted information

  free = 0 ; space_free = 0 ;
  used = 0 ; space_used = 0 ;
  for(cur = 1 ; cur < limit ; cur = cur + sz){  // go through block chain
    sz = (h[cur] > 0) ? h[cur] : - h[cur] ;     // size of block (negative value means block in use)
    if(sz < 3) return -1 ;                      // size must be > 2
    if(h[cur+sz-1] != sz) return 1 ;            // tail size not correct
    if(h[cur] > 0){                             // free block
      free++ ;
      space_free += sz ;
    }else{                                      // block in use
      used++ ;
      space_used += sz ;
    }
  }
  *free_blocks = free ;
  *free_space  = space_free * sizeof(heap_element) ;
  *used_blocks = used ;
  *used_space  = space_used * sizeof(heap_element) ;
  return 0 ;
}

void * ServerHeapAlloc(void *addr, uint32_t bsz, int safe){
  heap_element *h = (heap_element *) addr ;
  heap_element sz, limit, cur, next ;
  heap_element *t ;

  sz = h[0] ;
  limit = sz - 1 ;
  bsz /= sizeof(heap_element) ;
  t = NULL ;
  if(h[limit] > 1  || h[limit] < -1) return NULL  ;  // not a Server Heap or corrupted information
  if(safe){                                          // lock heap
    while(! __sync_bool_compare_and_swap(h + limit, 0, -1) ) ;  // wait for 0, then set to -1 to indicate lock
  }
  for(cur = 1 ; cur < limit ; cur += sz){            // scan block list to find/make a large enough free block
    sz = (h[cur] < 0) ? -h[cur] : h[cur] ;           // abs(h[cur])
    if(h[cur] < 0) continue ;                        // block is not free
    next = cur + sz ;                                // next block
    while(next < limit && h[next] > 2) {             // next block is free
      sz += h[next] ;                                // coalesce blocks
      next = cur + sz ;                              // new next after block coalescing
      h[cur] = sz ;                                  // head size marker
      h[next - 1] = sz ;                             // tail size marker
    }
    if(bsz <= sz - 2){                               // block large enough to satisfy request
      t = h + cur + 1 ;                              // point to element following size marker
      if(sz - bsz > 66) { //  split block if worth it (more than 64 extra elements
      }
      break ;
    }
  }
  if(safe){                                          // unlock heap
    h[limit] = 0 ;
  }
  return t ;
}

int ServerHeapIslocked(void *addr){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  return (h[sz-1] != 0) ;    // 0 normally, non zero if locked
}

void ServerHeapLock(void *addr){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  h = h + sz - 1 ;                                     // last element
  while( ! __sync_bool_compare_and_swap(h, 0, -1) ) ;  // if zero, set to -1 to indicate lock
}

void ServerHeapUnlock(void *addr){
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  h[sz-1] = 0 ;   // reset last element to unlocked value (zero)
}

#if defined(SELF_TEST)
#include <stdio.h>
#include <mpi.h>
int main ( int argc, char *argv[] )
{
  int rank, size, shared_elem = 0, i;
  MPI_Aint ssize; 
  int *shared;
  int errors = 0 ;

  MPI_Init ( &argc, &argv );
  MPI_Comm_rank ( MPI_COMM_WORLD, &rank );
  MPI_Comm_size ( MPI_COMM_WORLD, &size );
  MPI_Win win;

  if (rank == 0){
    ssize = size * 1024 * 1024 * sizeof(int);
    MPI_Win_allocate_shared(ssize, sizeof(int), MPI_INFO_NULL,
                            MPI_COMM_WORLD, &shared, &win);
  }else{
    int disp_unit;
    MPI_Win_allocate_shared(0, sizeof(int), MPI_INFO_NULL,
                            MPI_COMM_WORLD, &shared, &win);
    MPI_Win_shared_query(win, 0, &ssize, &disp_unit, &shared);
  }
  size = ssize / sizeof(int) ;
  if(rank==0){
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, MPI_MODE_NOCHECK, win);
      for(i = 0; i < size; i++) shared[i] = 1000000000 + i ;
      MPI_Win_unlock(0,win);
  }
  MPI_Barrier(MPI_COMM_WORLD);

  for(i = 0; i < size; i++) if(shared[i] != 1000000000 + i) errors++ ;
  printf("Process : %d, size = %d, errors = %d\n", rank, size, errors);

  MPI_Finalize ( );
  return 0 ;
}
#endif
