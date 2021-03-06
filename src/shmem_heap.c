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

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

#include "io-server/rpn_extra.h"

//   C_StArT
/**
 \file
 io-server/shmem_heap.h include file is needed to use shared memory heap functions<br>
 (extracted from file memory_arena.c)
*/
//   C_EnD
/**
 \file
   quick and dirty "process shared memory" heap management (C and Fortran)

 \verbatim

  ShmemHeap : A quick and dirty heap management package
               (mostly intended for use in managing shared memory across processes)

  Server Heap layout

  +------+---------------+    +---------------+     +---------------+------+-----------------+
  |  NT  |    block 0    | ...|    block i    | ... |    block N    | 0/1  | heap statistics |
  +------+---------------+    +---------------+     +---------------+------+-----------------+
  <------------------------------------ NT elements ----------------------->

  NT  :  total number of elements in entire Heap including NT itself (but EXCLUDING statistics)

  Server Heap block i layout

  +------+------+----------------------------------+------+------+
  |  NHi | HEAD |        NHi - 4 elements          | TAIL |  NHi |
  +------+-----------------------------------------+------+------+
  <------------------------- NHi elements ----------------------->

  abs(NHi)     : number of elements in block, including NH itself, (must be >2)
                 value at end of block is always positive

  NHi >  4     : block is free
  NHi < -4     : block is in use
  NHi 0/1/-1   : last block (null block) (normally 0)
  NSi = abs(NHi)
  block[0]     : NHi, size of block + in use flag (before head)
  block[NSi-1] : size of block (after tail)
  HEAD         : head marker  0xCAFEFADE
  TAIL         : tail marker  0xBEBEFADA

  Server Heap contents at creation

  +------+------+------+----------------------------------+------+------+------+-----------------+
  |  NT  |  NH0 | HEAD |   NH0 - 4 elements (free space)  | TAIL |  NH0 |   0  | heap statistics |
  +------+------+-----------------------------------------+------+------+------+-----------------+
         <------------------------- NH0 elements ----------------------->
  <-------------------------------- NT elementss ------------------------------>
  NT  :  total number of elements in entire Heap
  NH0 :  NT - 2 (initial block with NT -2 elements)

  elements are expected to be 32 bit elements in this implementation (see io-server/common.h)

 \endverbatim
*/

#include <io-server/shmem_heap.h>

// the block that follows was extracted to io-server/shmem_heap.h
#ifndef MAX_HEAPS

// \cond DOXYGEN_SHOULD_SKIP_THIS
//   C_StArT
#include <io-server/cb_data.h>

// remain consistent with io-server package

// heap element (see data_element in common.h)<br>
// if data_element is int32_t, "small" heap, with no more than 2*1024*1024*1024 - 1 elements (8 GBytes)
// heap element is identical to data_element
// typedef data_element heap_element

//!> heap element (same as data_element)
#if defined(DATA_ELEMENT_64)
typedef int64_t heap_element ;
#else
typedef data_element heap_element ;
#endif
// #define heap_element data_element

//!> maximum number of registered heaps
#define MAX_HEAPS 64

//!> HEAD marker below block
#define HEAD 0xCAFEFADE

//!> TAIL marker above block
#define TAIL 0xBEBEFADA

//!> heap statistics
typedef struct{
  uint64_t      max ;     //!< high water of heap (highest allocation point)
  uint64_t      nblk ;    //!< number of block allocations
  uint64_t      nbyt ;    //!< number of bytes allocated
} heap_stats ;

//!> heap information
typedef struct{
  heap_element *bot ;     //!< bottom of heap (lowest address)
  heap_element *top ;     //!< top of heap (highest address + 1)
  heap_stats   *inf ;     //!< pointer to heap statistics
} heap_item ;

//!> metadata description for Fortran (up to 5D arrays)
typedef  struct {
    int d[5];             //!< the 5 allowed dimensions     (private information, DO NOT USE)
    int tkr ;             //!< type, kind, rank information (private information, DO NOT USE)
  } meta_c;
//    C_EnD
// \endcond

#endif

//!> number of registered heaps
static int32_t nheaps = 0 ;

//!> table containing registered heap information
static heap_item heap_table[MAX_HEAPS] ;

//!> default heap (used by some functions if heap address is NULL)
static void *default_heap = NULL ;

//  C_StArT
//! get offset between 2 pointers in specified units (1/2/4/8/16 bytes)
//! @return offset between 2 pointers in specified units (1/2/4/8/16 bytes)
intptr_t Pointer_offset(
  void *ref,                //!< [in]  reference address
  void *to,                 //!< [in]  pointer for whic a difference with ref is sought
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  ){
//  C_EnD
  intptr_t offset = (char *)to - (char *)ref;
  while(szeof > 1) { offset >>= 1 ; szeof >>= 1 ; }
  return offset;
}


//  C_StArT
//! add offset to pointer in specified units (1/2/4/8/16 bytes)
//! @return pointer after adding offset in specified units (1/2/4/8/16 bytes)
void *Pointer_add_offset(
  void *ref,                //!< [in]  reference address
  intptr_t offset,          //!< [in]  offset to apply
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  ){
//  C_EnD
  char *tmp = (char *) ref ;
  while(szeof > 1) { offset <<= 1 ; szeof >>= 1 ; }
  return (void *)(tmp + offset) ;
}

//  C_StArT
//! print heap statistics
//! @return none
void ShmemHeapDumpInfo(
     ){
//  C_EnD
  int i ;
  printf("======== local heap table contents ========\n");
  for(i = 0 ; i < MAX_HEAPS ; i++) {
    if( heap_table[i].bot != NULL) {
      printf("heap %2d, (%p : %p), high point: %ld bytes, allocated %ld blocks (%ld bytes)\n",
             i, heap_table[i].bot, heap_table[i].top , 
             (heap_table[i].inf)->max * sizeof(heap_element) , 
             (heap_table[i].inf)->nblk, (heap_table[i].inf)->nbyt) ;
    }
  }
  printf("===========================================\n");
}

//  C_StArT
//! get heap statistics
//! @return 0 if O.K., nonzero if error
int ShmemHeapGetInfo(
  int index,          //!< [in]  heap index in registered heap table
  int64_t *size,      //!< [out] size of heap (bytes)
  int64_t *max,       //!< [out] high water mark in heap  (highest allocation point) (bytes)
  int64_t *nblk,      //!< [out] number of blocks that have been allocated
  int64_t *nbyt       //!< [out] total number of bytes used by allocated blocks
     ){
//  C_EnD

  *size = 0;
  *max  = 0;
  *nblk = 0;
  *nbyt = 0;
  if( (index < 0) || (index >= MAX_HEAPS)) return -1 ;  // bad index
  if(heap_table[index].bot == NULL)   return -1 ;  // not registered

  *size = (heap_table[index].top - heap_table[index].bot) * sizeof(heap_element) ;
  *max  = sizeof(heap_element) ;
  *max  *= (heap_table[index].inf)->max ;
  *nblk = (heap_table[index].inf)->nblk ;
  *nbyt = (heap_table[index].inf)->nbyt ;
  return 0 ;
}

//  C_StArT
//! get address of the default heap
//! @return default heap address (NULL if none)
void * ShmemHeapGetDefault(
  ){
//  C_EnD
  return (default_heap) ;
}

//  C_StArT
//! set this heap as the default heap
//! @return index in heap table if a known heap, -1 otherwise
int32_t ShmemHeapSetDefault(
  void *addr                          //!< [in]  address possibly of a known heap
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  int i;

  if(addr == NULL) return -1 ;        // obviously not a heap

  for(i=0 ; i<nheaps ; i++){
    if( h == heap_table[i].bot ){     // does the heap base address match ?
      default_heap = addr ;           // set this heap as the default heap
      return i ;                      // index in heap table
    }
  }
  return -1 ;                         // not a known heap
}

//  C_StArT
//! is this a known heap ?
//! @return index in heap table if a known heap, -1 otherwise
int32_t ShmemHeapIndex(
  void *addr                          //!< [in]  address possibly of a known heap
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  int i;

  if(addr == NULL) addr = default_heap ;   // use default heap if address id NULL
  if(addr == NULL) return -1 ;        // obviously not a heap

  for(i=0 ; i<nheaps ; i++){
    if( b == heap_table[i].bot ){     // does the heap base address match ?
      return i ;                      // index in heap table
    }
  }
  return -1 ;                         // not a known heap
}

//  C_StArT
//! is this a known heap ?
//! @return size if a known heap, -1 otherwise
heap_element ShmemHeapSize(
  void *addr                    //!< [in]  address possibly of a known heap
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  int i;

  if(addr == NULL) return -1 ;        // obviously not a heap

  for(i=0 ; i<nheaps ; i++){
    if( b == heap_table[i].bot ){     // does the heap base address match ?
      return b[0] ;                   // size of heap
    }
  }
  return -1 ;                         // not a known heap
}

//  C_StArT
//! which known heap does this address belong to ?
//! @return heap base address if within a known heap, NULL otherwise
heap_element *ShmemHeapContains(
  void *addr                    //!< [in]  address possibly in a known heap
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  int i;

  for(i=0 ; i<nheaps ; i++){
    if( (b >= heap_table[i].bot ) && (b < heap_table[i].top) ){ // address is within heap boundaries
      return heap_table[i].bot ;                          // base address of heap this address belongs to
    }
  }
  return NULL ;    // not within a known heap
}

//  C_StArT
//! translate address to offset within a heap
//! @return offset with respect to base of heap in heap_element units (NOT bytes)
heap_element ShmemHeapPtr2Offset(
  void *addr                    //!< [in]  address to translate to index
  ){
//  C_EnD
  heap_element *p = (heap_element *) addr ;
  heap_element *h ;

  h = ShmemHeapContains(addr) ; // address is within a heap if h != NULL
  if( h != NULL) {              // base of a known heap ?
    return (p - h) ;            // yes, return displacement with respect to base of heap
  }

  return -1 ; // address not within bounds of known heap
}

//  C_StArT
//! is this the address of a block belonging to a known heap ?
//! @return 0 if valid block from known heap,<br>
//!        -1 if unknown heap,<br>
//!         1 if inside a known heap but not a valid block pointer
int32_t ShmemHeapValidBlock(
  void *addr                    //!< [in]  putative valid block address
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  heap_element *h ;
  heap_element sz ;

  if( (h = ShmemHeapContains(addr)) != NULL) { // inside a known heap ?
    b = b - 2 ;                                // base of block structure (2 elements below user block address)
    if(b[1] != HEAD)         return 1 ;        // invalid HEAD marker below block 
    sz = b[0] > 0 ? b[0] : -b[0] ;             // get block size (negative means block is in use)
    if(sz < 5)               return 1 ;        // invalid block size (cannot be less than 5)
    if(b + sz >= h + h[0] )  return 1 ;        // top of block would be out of heap
    if(b[sz-2] != TAIL)      return 1 ;        // invalid TAIL marker above block
    if(b[sz-1] != sz)        return 1 ;        // wrong trailer size marker
    return 0 ;                                 // this looks like a valid block
  }
  return -1 ; // address not within bounds of known heap
}

//  C_StArT
//! is this the address of a block belonging to a known heap ?<br>
//! same as ShmemHeapValidBlock but returns block size code instead of true/false information
//! @return block size code if valid block from known heap,<br>
//!        -1 if unknown heap,<br>
//!         1 if inside known heap but not a proper block
heap_element ShmemHeapBlockSizeCode(
  void *addr                    //!< [in]  putative valid block address
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  heap_element *h ;
  heap_element sz ;

  if( (h = ShmemHeapContains(addr)) != NULL) { // inside a known heap ?
    b = b - 2 ;                                // base of block structure (2 elements below user block address)
    if(b[1] != HEAD)         return 1 ;        // invalid HEAD marker below block 
    sz = b[0] > 0 ? b[0] : -b[0] ;             // get block size (negative means block is in use)
    if(sz < 5)               return 1 ;        // invalid block size (cannot be less than 5)
    if(b + sz >= h + h[0] )  return 1 ;        // top of block would be out of heap
    if(b[sz-2] != TAIL)      return 1 ;        // invalid TAIL marker above block
    if(b[sz-1] != sz)        return 1 ;        // wrong trailer size marker
    return b[0] ;                              // this looks like a valid block, return block size code
  }
  return -1 ; // address not within bounds of known heap
}

//  C_StArT
//! find the size of a used memory block (in bytes)<br>
//! uses either address of block or address of heap and offset
//! @return size of used block in bytes, 0 if not a block or block not in use
size_t ShmemHeapBlockSize(
  void *heap,                   //!< [in]  heap address (if NULL, only addr is used, offset is ignored)
  void *addr,                   //!< [in]  block address (if NULL, heap address and offset must be valid)
  heap_element offset           //!< [in]  offset into heap (ignored if heap is NULL or addr is not NULL)
  ){
//  C_EnD
  heap_element *h = (heap_element *) heap ;
  heap_element *b = (heap_element *) addr ;
  heap_element sz ;
  size_t bsz ;

  if(h == NULL){                           // no heap address specified
    if(b == NULL)      return 0 ;          // block address is mandatory if h is NULL
    sz = ShmemHeapBlockSizeCode(addr) ;   // valid block ?
  }else{
    if(offset <= 0)    return 0 ;          // offset is mandatory if h is not NULL
    b = h + offset ;                       // putative block address
    sz = ShmemHeapBlockSizeCode(b) ;
  }
  if(sz == -1)       return 0 ; // address not found in any known heap
  if(sz >=  0)       return 0 ; // cannot be a used block (marker should be negative)
  sz = -sz ;
  bsz = sz ;
  return (bsz - 4) * sizeof(heap_element) ;  // returned size is in bytes
}

//  C_StArT
//! translate offset from base of heap into actual address
//! @return address, NULL if offset out of heap
void *ShmemHeapPtr(
  void *addr,                   //!< [in]  heap address
  heap_element offset           //!< [in]  offset into heap
  ){
//  C_EnD
  heap_element *h = addr ;               // base of putative heap
  heap_element sz = ShmemHeapSize(addr);

  if(sz <= 0) return NULL ;       // not a known heap
  if(offset <= 0) return NULL ;   // invalid offset

  return ( offset > (sz -2) ) ? NULL : (h + offset) ;  // offset is too large (out of heap)
}

//  C_StArT
//! register a  Heap in the heap table
//! @return number of registered heaps if successful, -1 otherwise
int32_t ShmemHeapRegister(
  void *addr                    //!< [in]  heap address
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  int i;
  int target = -1 ;

  if(nheaps == 0){                    // first time around, initialize heaps
    for(i=0 ; i<MAX_HEAPS ; i++) {
      heap_table[i].bot = NULL ;
      heap_table[i].top = NULL ;
      heap_table[i].inf = NULL ;
    }
  }

  for(i=0 ; i<MAX_HEAPS ; i++) if(heap_table[i].bot == NULL) {
    target = i ;                  // unused entry found in table ?
    break;
  }
  if(target == -1) return -1 ;    // table is full, sorry !

  heap_table[target].bot = h ;          // base of heap
  heap_table[target].top = h + h[0] ;   // 1 element beyond top of heap
  heap_table[target].inf = (heap_stats *) heap_table[target].top ;
  if(target >= nheaps)nheaps++ ;  // bump heaps counter if not recycling an entry
  return nheaps ;                 // number of registered heaps
}

//  C_StArT
//! initialize a Server Heap
//! @return address of Server Heap if successful, NULL otherwise
void *ShmemHeapInit(
  void *addr,                    //!< [in]  desired heap address, if NULL, allocate space with malloc
  size_t sz                      //!< [in]  size in bytes of space pointed to by addr
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  heap_element heap_sz ;
  heap_stats *stats ;

  if(h == NULL) h = (heap_element *) malloc(sz) ;
  if(h == NULL) return NULL ;

  sz = sz - sizeof(heap_stats) ;          // subtract size of stas area
  heap_sz = sz / sizeof(heap_element) ;   // convert to heap element units
  h[0] = heap_sz ;              // size of heap
  h[1] = heap_sz -2 ;           // size of first block (head)
  h[2] = HEAD ;
  h[heap_sz - 3] = TAIL ;
  h[heap_sz - 2] = heap_sz-2 ;  // size of first block (tail)
  h[heap_sz - 1] = 0 ;          // last block (not locked)

  stats = (heap_stats *) (h + heap_sz) ;
  stats->max  = 0 ;             // high water mark
  stats->nblk = 0 ;             // allocated blocks
  stats->nbyt = 0 ;             // bytes allocated
  

  ShmemHeapRegister(h) ;        // register Heap for block validation purpose
  return h ;                    // O.K. return address of Heap
}

//  C_StArT
//! check integrity of Server Heap
//! @return 0 : O.K.<br>
//!         1 : bad address<br>
//!         2 : not a valid heap or corrupted information<br>
//!         3 : size marker is invalid<br>
//!         4 : tail size marker not consistent
int32_t ShmemHeapCheck(
  void *addr,                     //!< [in]  address of Server Heap to check
  int32_t *free_blocks,           //!< [out] number of free blocks
  size_t *free_space,             //!< [out] available space in bytes
  int32_t *used_blocks,           //!< [out] number of used blocks
  size_t *used_space              //!< [out] used space in bytes
  ){
//  C_EnD
  heap_element *h ;
  heap_element sz ;
  heap_element cur, limit ;
  int32_t free, used ;
  size_t space_used, space_free ;

  *free_blocks = 0 ; free = 0 ;
  *free_space  = 0 ; used = 0 ;
  *used_blocks = 0 ; space_used = 0 ;
  *used_space  = 0 ; space_free = 0 ;

  if(addr == NULL) addr = default_heap ;   // use default heap if address id NULL
  if(addr == NULL) return 1;
  h = (heap_element *) addr ;

  sz    = h[0] ;
  limit = sz - 1 ;

  if(h[limit] > 1  || h[limit] < -1) return 2  ;  // not a valid Heap or corrupted information

  free = 0 ; space_free = 0 ;
  used = 0 ; space_used = 0 ;
  for(cur = 1 ; cur < limit ; cur = cur + sz){  // go through block chain
    if(h[cur] == 0) break;                      // top of heap

    sz = (h[cur] > 0) ? h[cur] : - h[cur] ;     // size of block (negative value means block in use)

    if(sz < 5 && sz > 0) return 3 ;             // valid size must be > 4
    if(h[cur+1] != HEAD || h[cur+sz-2] != TAIL){
      printf("trampled block bounds marker(s), expected %8.8x %8.8x, found %8.8x %8.8x\n",
             HEAD, TAIL, h[cur+1], h[cur+sz-2]);
      return 4 ; 
    }
    if(h[cur+sz-1] != sz) {                     // check tail size marker
      printf("bad tail size marker, got %d, expected %d\n",h[cur+sz-1], sz);
      return 4 ;                                // tail size marker not consistent
    }
    if(h[cur] > 0){                             // free block
      free++ ;
      space_free = space_free + sz - 4 ;
    }else{                                      // block in use
      used++ ;
      space_used = space_used + sz - 4 ;
    }
  }
  *free_blocks = free ;
  *free_space  = space_free * sizeof(heap_element) ;   // convert size into bytes
  *used_blocks = used ;
  *used_space  = space_used * sizeof(heap_element) ;   // convert size into bytes
  return 0 ;
}

//  C_StArT
//! allocate space on a Server Heap
//! @return address of block, NULL in case of failure to allocate
void *ShmemHeapAllocBlock(
  void *addr,                      //!< [in]  address of Server Heap
  size_t bsz,                      //!< [in]  size in bytes of block to allocate
  int32_t safe                     //!< [in]  if nonzero, perform operation under lock
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  heap_element sz, limit, cur, next ;
  heap_element *t ;
  int32_t index ;
  size_t nbyt = bsz ;

  index = ShmemHeapIndex(addr) ;
  if(index == -1) return NULL ;

  sz = h[0] ;
  limit = sz - 1 ;
// printf("request block size = %d, sz = %d, limit = %d\n",bsz,sz,limit);
  bsz = (bsz + sizeof(heap_element) -1) / sizeof(heap_element) ;  // round size UP (size in heap elements)
  t = NULL ;
  if(h[limit] > 1  || h[limit] < -1) return NULL  ;  // not a Server Heap or corrupted information
  if(safe){                                          // lock heap
//     while(! __sync_bool_compare_and_swap(h + limit, 0, -1) ) ;  // wait for 0, then set to -1 to indicate lock
    while(! __sync_bool_compare_and_swap(h + limit, 0, 1) ) ;  // wait for 0, then set to 1 to indicate lock
  }
  bsz += 4 ;                                         // add head + tail elements
  for(cur = 1 ; cur < limit ; cur += sz){            // scan block list to find/make a large enough free block
    sz = (h[cur] < 0) ? -h[cur] : h[cur] ;           // abs(h[cur])
    if(h[cur] < 0) continue ;                        // block is not free
    next = cur + sz ;                                // next block
    while(next < limit && h[next] > 2) {             // next block is free
//       printf("coalescing blocks\n");
      sz += h[next] ;                                // coalesce blocks
      next = cur + sz ;                              // new next after block coalescing
      h[cur]      = sz ;                             // head size marker
      h[cur+1]    =  HEAD ;
      h[next - 2] = TAIL ;
      h[next - 1] = sz ;                             // tail size marker
    }
    if(bsz <= sz){                                   // block large enough to satisfy request
      t = h + cur + 2 ;                              // point to element following size marker
      (heap_table[index].inf)->nblk += 1 ;
      (heap_table[index].inf)->nbyt += nbyt ;
      if(sz - bsz > 64) {             //  split block if worth it (more than 64 extra elements)
        h[cur]       =  -bsz ;        // head count (lower block)
        h[cur+1]     =  HEAD ;        // low  marker
        h[cur+bsz-2] =  TAIL ;        // tail marker
        h[cur+bsz-1] =   bsz ;        // tail count (lower block)
        if( (cur + bsz) > (heap_table[index].inf)->max ) (heap_table[index].inf)->max = (cur + bsz) ;
        h[cur+bsz]      = sz - bsz ;  // head count (upper block)
        h[cur+bsz+1]    = HEAD ;      // low  marker
        h[cur + sz - 2] = TAIL ;      // tail marker
        h[cur + sz - 1] = sz - bsz ;  // tail count (upper block)
      }
      else {
        h[cur] = -sz;
        if( (cur + sz) > (heap_table[index].inf)->max ) (heap_table[index].inf)->max = (cur + sz) ;
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

//  C_StArT
//! set block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapSetBlockMeta(
  void *addr,                      //!< [in]  address of block
  unsigned char *meta,             //!< [in]  address of metadata
  int msz                          //!< [in]  size of metadata (bytes)
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;     // block address
  heap_element sz ;
  int32_t i ;
  unsigned char *p ;
// meta_c *m;

  if(addr == NULL || msz <= 0 || meta == NULL) return 1 ;
  if( ShmemHeapValidBlock(b) != 0 ) return 1 ;  // is block valid ?
  b = b - 2 ;                                   // start of block
  sz = -b[0] ;                                  // size of block
  if(sz < 0) return 1 ;                         // block is free
  b = b + sz - 2 ;                              // address of tail marker ;
  p = (unsigned char *) b ;                     // cast to byte pointer
// printf("b = %p, p = %p msz = %d, sz = %d\n",b,p,msz,sz);
  p = p - msz ;                                 // start of metadata in block (top of data in block - msz)
// m = (meta_c *) meta;
// printf("dim = %d %d %d %d %d, tkr = %x\n",m->d[0],m->d[1],m->d[2],m->d[3],m->d[4],m->tkr);
  for(i = 0 ; i < msz ; i++) p[i] = meta[i] ;   // copy meta at end of block
  return 0 ;
}

//  C_StArT
//! get block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapGetBlockMeta(
  void *addr,                      //!< [in]  address of block
  unsigned char *meta,             //!< [out]  address of metadata (user array to receive metadata)
  int msz                          //!< [in]  size of metadata (bytes)
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;     // block address
  heap_element sz ;
  int32_t i ;
  unsigned char *p ;

  if(addr == NULL || msz <= 0 || meta == NULL) return 1 ;
  if( ShmemHeapValidBlock(b) != 0 ) return 1 ;  // is block valid ?
  b = b - 2 ;                                   // start of block
  sz = -b[0] ;                                  // size of block
  if(sz < 0) return 1 ;                         // block is free
  b = b + sz - 2 ;                              // address of tail marker ;
  p = (unsigned char *) b ;                     // cast to byte pointer
  p = p - msz ;                                 // start of metadata in block (top of data in block - msz)
  for(i = 0 ; i < msz ; i++) meta[i] = p[i] ;   // copy metadata from end of data block into user array
  return 0 ;
}

//  C_StArT
//! free space on a Server Heap
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapFreeBlock(
  void *addr                       //!< [in]  address of block
    ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  heap_element nw ;
  int status ;

  status = ShmemHeapValidBlock(addr);

  if(status != 0) {   // is this the address of a valid block ?
    return -1 ;                                      // unknown heap or invalid block pointer 
  }

  h = h - 2;                       // point to count (two elements below block)
  nw = h[0];                       // if block is in use, nw will be negative
  if(nw >= 0) return -2 ;          // certainly not a block in use
  nw = -nw ;                       // make count positive
  memory_fence();                  // make sure every read operation within this block has been completed before freeing it
  h[0] = nw ;                      // mark memory block as free
  return 0;
}

//  C_StArT
//! check if Server Heap is locked
//! @return 0 if not locked, nonzero if locked
int32_t ShmemHeapIslocked(
  void *addr                      //!< [in]  address of Server Heap
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  return (h[sz-1] != 0) ;    // 0 normally, non zero if locked
}

//  C_StArT
//! lock Server Heap
//! @return none
void ShmemHeapLock(
  void *addr                      //!< [in]  address of Server Heap
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr ;
  heap_element sz ;

  sz = h[0] ;
  h = h + sz - 1 ;                                     // last element
//   while( ! __sync_bool_compare_and_swap(h, 0, -1) ) ;  // if zero, set to -1 to indicate lock
  while( ! __sync_bool_compare_and_swap(h, 0, 1) ) ;  // if zero, set to 1 to indicate lock
}

//  C_StArT
//! unlock Server Heap
//! @return none
void ShmemHeapUnlock(
  void *addr                      //!< [in]  address of Server Heap
  ){
//  C_EnD
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
  int32_t *t ;

  int rank, size, i, status;
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

    sm.heap = ShmemHeapInit(sm.heap, 32*1024) ; // create and initialize heap
    printf("registered heap 1\n");
    status = ShmemHeapRegister(sm.heap);
    printf("registered heap %d\n",status);  // re register heap
    // Heap check (pristine Heap)
    status = ShmemHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);

    for(i=0 ; i<64 ; i++){      // allocate 64 blocks if possible
      t = (int32_t *)ShmemHeapAllocBlock(sm.heap, 1025, 0) ;    // try to allocate block
      if(t == NULL) {                                            // not enough room left
        printf("allocation failed, block number = %d\n",i);
        break ;
      }
      sm.index[i] = t - sm.heap ;                                // index displacement to block from base of heap
      printf("block %i allocated at %p, heap = %p, index = %d\n",i,t,sm.heap,sm.index[i]);
    }
    // Heap check (Heap with data blocks)
    status = ShmemHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
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
  if(rank != 0){
    status = ShmemHeapRegister(sm.heap);
    printf("registered heap %d\n",status);
    status = ShmemHeapRegister(sm.heap);  // re register heap
    printf("registered heap %d\n",status);
    errors = 0 ;
    for(i=0 ; i<NINDEXES ; i++) if(sm.index[i] != 0 )errors++;
    printf("process %d detected %d block(s), index size = %d\n", rank, errors, sm.nindx) ;
  }

  MPI_Barrier(MPI_COMM_WORLD) ;    // all ranks now aware of existing heap

  if(rank == 1){      // rank 1 process
    errors = 0 ;
    for(i=0 ; i<NINDEXES ; i++) if(sm.index[i] != 0 ) {
      printf("index[%d] = %d, ", i, sm.index[i]) ;
      errors++;
    }
    printf("\n");
    status = ShmemHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);
    // now free all allocated blocks
    for(i=0 ; i < NINDEXES ; i++) {
      if(sm.index[i] != 0) {

        status = ShmemHeapFreeBlock(sm.heap + sm.index[i] + 1) ; // this is an invalid address
        if(status == 0) 
          printf("ERROR: phony free status = %d, should not be 0\n",status);

        ShmemHeapFreeBlock(sm.heap + sm.index[i]) ;
        printf("block %d freed at index %d\n",i,sm.index[i]);
      }
      sm.index[i] = 0 ;
    }
    // Heap check (Heap after all blocks have been freed)
    status = ShmemHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
    printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
           rank, free_blocks, free_space, used_blocks, used_space, status);
    t = (int32_t *)ShmemHeapAllocBlock(sm.heap, 10, 0) ;
    sm.index[0] = t - sm.heap ;   // lone block just allocated
  }

  MPI_Barrier(MPI_COMM_WORLD) ;  // make sure everything rank 1 did is complete

// index check and Heap check by all processes
  for(i=0 ; i<NINDEXES ; i++) if(sm.index[i] != 0 ) {
    printf("index[%d] = %d \n", i, sm.index[i]) ;
  }
  // Heap check (there should only be 1 free block and 1 used block because of free block coalescing)
  status = ShmemHeapCheck(sm.heap, &free_blocks, &free_space, &used_blocks, &used_space) ;
  printf("process %d, free_blocks = %d, free_space = %ld, used_blocks = %d, used_space = %ld, status = %d\n",
          rank, free_blocks, free_space, used_blocks, used_space, status);

//=======================================================================================//
  MPI_Barrier(MPI_COMM_WORLD) ;        // NEEDED for the previous part of the test code to work
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
