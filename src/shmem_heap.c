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

// \cond DOXYGEN_SHOULD_SKIP_THIS
//   C_StArT
#include <io-server/cb_data.h>

// remain consistent with io-server package

typedef int64_t heap_element ; //!< heap element (should be 64 bits, to be able to contain its own size)

//!> maximum number of registered heaps
#define MAX_HEAPS 128

const heap_element HEAD = 0xCAFEFADE; //!< HEAD marker below block
const heap_element TAIL = 0xBEBEFADA; //!< TAIL marker above block

const heap_element BLOCK_SPLIT_SIZE = 64; //!< Number of extra elements in a potential block to decide to split it
const heap_element MIN_HEAP_SIZE = BLOCK_SPLIT_SIZE + 4;

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
  heap_stats   *stats ;   //!< pointer to heap statistics
} heap_item ;

//!> metadata description for Fortran (up to 5D arrays)
typedef  struct {
    int d[5];             //!< the 5 allowed dimensions     (private information, DO NOT USE)
    int tkr ;             //!< type, kind, rank information (private information, DO NOT USE)
  } meta_c;
//    C_EnD
// \endcond

//!> number of registered heaps
static int32_t num_heaps = 0 ;

//!> table containing registered heap information
static heap_item heap_table[MAX_HEAPS] ;

//!> default heap (used by some functions if heap address is NULL)
static void *default_heap = NULL ;

//F_StArT
//  interface
//F_EnD

// F_StArT
// function Pointer_offset(ref, to, szeof) result(offset) bind(C,name='Pointer_offset')
//   import :: C_INTPTR_T, C_PTR, C_INT
//   implicit none
//   type(C_PTR), intent(IN), value    :: ref
//   type(C_PTR), intent(IN), value    :: to
//   integer(C_INT), intent(IN), value :: szeof
//   integer(C_INTPTR_T)               :: offset
// end function Pointer_offset
// F_EnD
//  C_StArT
//! get offset between 2 pointers in specified units (1/2/4/8/16 bytes)
//! @return offset between 2 pointers in specified units (1/2/4/8/16 bytes)
intptr_t Pointer_offset(
  void *ref,                //!< [in]  reference address
  void *to,                 //!< [in]  pointer for which a difference with ref is sought
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  ){
//  C_EnD
  intptr_t offset = (char *)to - (char *)ref;
  while(szeof > 1) { offset >>= 1 ; szeof >>= 1 ; }
  return offset;
}


// F_StArT
// function Pointer_add_offset(ref, offset, szeof) result(to) bind(C,name='Pointer_add_offset')
//   import :: C_INTPTR_T, C_PTR, C_INT
//   implicit none
//   type(C_PTR), intent(IN), value          :: ref
//   integer(C_INTPTR_T), intent(IN), value  :: offset
//   integer(C_INT), intent(IN), value       :: szeof
//   type(C_PTR)                             :: to
// end function Pointer_add_offset
// F_EnD
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

// F_StArT
// subroutine ShmemHeapDumpInfo() bind(C,name='ShmemHeapDumpInfo')
//   implicit none
// end subroutine ShmemHeapDumpInfo
// F_EnD
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
             i, (void*)heap_table[i].bot, (void*)heap_table[i].top, 
             (heap_table[i].stats)->max * sizeof(heap_element), 
             (heap_table[i].stats)->nblk, (heap_table[i].stats)->nbyt) ;
    }
  }
  printf("===========================================\n");
}

// F_StArT
// function ShmemHeapGetInfo(ix, sz, max, nblk, nbyt) result(status) bind(C,name='ShmemHeapGetInfo')
//   import :: C_INT, C_LONG_LONG
//   implicit none
//   integer(C_INT), intent(IN), value :: ix
//   integer(C_LONG_LONG), intent(OUT) :: sz
//   integer(C_LONG_LONG), intent(OUT) :: max
//   integer(C_LONG_LONG), intent(OUT) :: nblk
//   integer(C_LONG_LONG), intent(OUT) :: nbyt
//   integer(C_INT) :: status
// end function ShmemHeapGetInfo
// F_EnD
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
  *max  *= (heap_table[index].stats)->max ;
  *nblk = (heap_table[index].stats)->nblk ;
  *nbyt = (heap_table[index].stats)->nbyt ;
  return 0 ;
}

// F_StArT
// function ShmemHeapGetDefault() result(h) bind(C,name='ShmemHeapGetDefault')
//   import :: C_PTR
//   implicit none
//   type(C_PTR) :: h
// end function ShmemHeapGetDefault
// F_EnD
//  C_StArT
//! get address of the default heap
//! @return default heap address (NULL if none)
void * ShmemHeapGetDefault(
  ){
//  C_EnD
  return (default_heap) ;
}

// F_StArT
// function ShmemHeapSetDefault(heap) result(ix) bind(C,name='ShmemHeapSetDefault')
//   import :: C_PTR, C_INT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_INT) :: ix
// end function ShmemHeapSetDefault
// F_EnD
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

  for(i=0 ; i<num_heaps ; i++){
    if( h == heap_table[i].bot ){     // does the heap base address match ?
      default_heap = addr ;           // set this heap as the default heap
      return i ;                      // index in heap table
    }
  }
  return -1 ;                         // not a known heap
}

// F_StArT
// function ShmemHeapIndex(heap) result(ix) bind(C,name='ShmemHeapIndex')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_INT) :: ix
// end function ShmemHeapIndex
// F_EnD
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

  for(i=0 ; i<num_heaps ; i++){
    if( b == heap_table[i].bot ){     // does the heap base address match ?
      return i ;                      // index in heap table
    }
  }
  return -1 ;                         // not a known heap
}

// F_StArT
// function ShmemHeapSize(heap) result(s) bind(C,name='ShmemHeapSize')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(HEAP_ELEMENT) :: s
// end function ShmemHeapSize
// F_EnD
//  C_StArT
//! is this a known heap ?
//! @return size (in bytes) if a known heap, -1 otherwise
heap_element ShmemHeapSize(
  void *addr                    //!< [in]  address possibly of a known heap
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;
  int i;

  if(addr == NULL) return -1 ;        // obviously not a heap

  for(i=0 ; i<num_heaps ; i++){
    if( b == heap_table[i].bot ){         // does the heap base address match ?
      return b[0] * sizeof(heap_element); // size of heap
    }
  }
  return -1 ;                         // not a known heap
}

// F_StArT
// function ShmemHeapContains(addr) result(p) bind(C,name='ShmemHeapContains')
//   import :: C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: addr
//   type(C_PTR) :: p
// end function ShmemHeapContains
// F_EnD
//  C_StArT
//! which known heap does this address belong to ?
//! @return heap base address if within a known heap, NULL otherwise
heap_element *ShmemHeapContains(
  void *addr                    //!< [in]  address possibly in a known heap
  ){
//  C_EnD
  heap_element *b = (heap_element *) addr ;

  for(int i=0 ; i<num_heaps ; i++){
    if( (b >= heap_table[i].bot ) && (b < heap_table[i].top) ){ // address is within heap boundaries
      return heap_table[i].bot ;                          // base address of heap this address belongs to
    }
  }
  return NULL ;    // not within a known heap
}

// F_StArT
// function ShmemHeapPtrToOffset(addr) result(offset) bind(C,name='ShmemHeapPtrToOffset')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: addr
//   integer(HEAP_ELEMENT) :: offset
// end function ShmemHeapPtrToOffset
// F_EnD
//  C_StArT
//! translate address to offset within a heap
//! @return offset with respect to base of heap in heap_element units (NOT bytes)
// heap_element ShmemHeapPtrToOffset(
//   void *addr                    //!< [in]  address to translate to index
//   ){
// //  C_EnD
//   heap_element *p = (heap_element *) addr ;
//   heap_element *h ;

//   h = ShmemHeapContains(addr) ; // address is within a heap if h != NULL
//   if( h != NULL) {              // base of a known heap ?
//     return (p - h) ;            // yes, return displacement with respect to base of heap
//   }

//   return -1 ; // address not within bounds of known heap
// }

// F_StArT
// function ShmemHeapValidBlock(addr) result(status) bind(C,name='ShmemHeapValidBlock')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: addr
//   integer(C_INT) :: status
// end function ShmemHeapValidBlock
// F_EnD
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

    // Check a few attributes of the block
    if (b[1] != HEAD) {
      // invalid HEAD marker below block 
      // printf("HEAD wrong (%ld, should be %ld)\n", b[1], HEAD);
      return 1;
    }

    sz = b[0] > 0 ? b[0] : -b[0] ;             // get block size (negative means block is in use)
    if (sz < 5) {
      // printf("Size wrong (%ld, cannot be less than 5)\n", sz);
      return 1;
    }

    if (b + sz >= h + h[0]) {
      // printf("Block TOP is out of the heap!\n");
      return 1 ;
    }

    if (b[sz-2] != TAIL) {
      // printf("TAIL wrong (%ld, should be %ld)\n", b[sz-2], TAIL);
      return 1;
    }

    if (b[sz-1] != sz) {
      // printf("Trailing size marker is wrong (%ld, should be %ld)\n", b[sz-1], sz);
      return 1 ;
    }

    return 0 ;                                 // this looks like a valid block
  }

  return -1 ; // address not within bounds of known heap
}

//! is this the address of a block belonging to a known heap ?<br>
//! same as ShmemHeapValidBlock but returns block size code instead of true/false information
//! @return block size code if valid block from known heap,<br>
//!        -1 if unknown heap,<br>
//!         1 if inside known heap but not a proper block
heap_element ShmemHeapBlockSizeCode(
  void *addr                    //!< [in]  putative valid block address
  ){
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

// F_StArT
// function ShmemHeapBlockSize(heap, addr, offset) result(bsz) bind(C,name='ShmemHeapBlockSize')
//   import :: C_PTR, C_SIZE_T, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   type(C_PTR), intent(IN), value :: addr
//   integer(HEAP_ELEMENT), intent(IN) :: offset
//   integer(C_SIZE_T) :: bsz
// end function ShmemHeapBlockSize
// F_EnD
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

// F_StArT
// function ShmemHeapPtr(heap, offset) result(p) bind(C,name='ShmemHeapPtr')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(HEAP_ELEMENT), intent(IN), value :: offset
//   type(C_PTR) :: p
// end function ShmemHeapPtr
// F_EnD
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

// F_StArT
// function ShmemHeapOffsetFromPtr(heap, block) result(offset) bind(C, name='ShmemHeapOffsetFromPtr')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   type(C_PTR), intent(IN), value :: block
//   integer(HEAP_ELEMENT) :: offset
// end function ShmemHeapOffsetFromPtr
// F_EnD
//! Translate address inside heap into an offset (of size of #heap_element). The pointer must be inside the heap!
// C_StArT
heap_element ShmemHeapOffsetFromPtr(
    heap_element *heap, //!< [in] Heap address
    heap_element *block //!< [in] Address of the block for which we want the offset
  ) {
// C_EnD
  return block - heap;
}

// F_StArT
// function ShmemHeapRegister(heap) result(status) bind(C,name='ShmemHeapRegister')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_INT) :: status
// end function ShmemHeapRegister
// F_EnD
//  C_StArT
//! register a  Heap in the heap table
//! @return number of registered heaps if successful, -1 otherwise
int32_t ShmemHeapRegister(
  void *addr                    //!< [in]  heap address
  ){
//  C_EnD
  heap_element *h = (heap_element *) addr;

  if (h[0] < (int64_t)BLOCK_SPLIT_SIZE) return -1; // Heap is too small, can't be valid

  // first time around, initialize heaps
  if(num_heaps == 0){
    for(int i=0 ; i<MAX_HEAPS ; i++) {
      heap_table[i].bot = NULL ;
      heap_table[i].top = NULL ;
      heap_table[i].stats = NULL ;
    }
  }

  // Look for unused entries in the table
  int target = -1 ;
  for (int i = 0; i < MAX_HEAPS; i++) {
    if (heap_table[i].bot == NULL) {
      target = i; // Found one
      break;
    }
  }

  if(target == -1) return -1; // table is full, sorry !

  heap_table[target].bot   = h ;          // base of heap
  heap_table[target].top   = h + h[0] ;   // 1 element beyond top of heap
  heap_table[target].stats = (heap_stats *) heap_table[target].top ;
  if (target >= num_heaps) num_heaps++ ;  // bump heaps counter if not recycling an entry

  return num_heaps ;                      // number of registered heaps
}

// F_StArT
// function ShmemHeapInit(heap, nbytes) result(h) bind(C,name='ShmemHeapInit')
//   import :: C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_SIZE_T), intent(IN), value :: nbytes
//   type(C_PTR) :: h
// end function ShmemHeapInit
// F_EnD
//  C_StArT
//! initialize a Server Heap
//! @return address of Server Heap if successful, NULL otherwise
void *ShmemHeapInit(
  void *addr,                    //!< [in]  desired heap address, if NULL, allocate space with malloc
  size_t num_bytes               //!< [in]  size in bytes of space pointed to by addr
  ){
//  C_EnD
  heap_element *heap = (heap_element *) addr ;
  size_t num_elem;
  heap_stats *stats ;

  if(heap == NULL) heap = (heap_element *) malloc(num_bytes) ;
  if(heap == NULL) return NULL ;

  num_bytes = num_bytes - sizeof(heap_stats) ;    // subtract size of stas area
  num_elem = num_bytes / sizeof(heap_element) ;   // convert to heap element units
  heap[0] = num_elem ;              // size of heap
  heap[1] = num_elem -2 ;           // size of first block (head)
  heap[2] = HEAD ;
  heap[num_elem - 3] = TAIL ;
  heap[num_elem - 2] = num_elem-2 ; // size of first block (tail)
  heap[num_elem - 1] = 0 ;          // last block (not locked)

  stats = (heap_stats *) (heap + num_elem) ;
  stats->max  = 0 ;             // high water mark
  stats->nblk = 0 ;             // allocated blocks
  stats->nbyt = 0 ;             // bytes allocated

  ShmemHeapRegister(heap);        // register Heap for block validation purpose
  return heap;                    // O.K. return address of Heap
}

// F_StArT
// function ShmemHeapCheck(heap, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeapCheck')
//   import :: C_INT, C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
//   integer(C_SIZE_T), intent(OUT) :: free_space, used_space
//   integer(C_INT) :: status
// end function ShmemHeapCheck
// F_EnD
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
      printf("trampled block bounds marker(s), expected %8.8lx %8.8lx, found %8.8lx %8.8lx\n",
             HEAD, TAIL, h[cur+1], h[cur+sz-2]);
      return 4 ; 
    }
    if(h[cur+sz-1] != sz) {                     // check tail size marker
      printf("bad tail size marker, got %ld, expected %ld\n",h[cur+sz-1], sz);
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

// F_StArT
// function ShmemHeapAllocBlock(heap, nbytes, safe) result(b) bind(C,name='ShmemHeapAllocBlock')
//   import :: C_INT, C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_SIZE_T), intent(IN), value :: nbytes
//   integer(C_INT), intent(IN), value :: safe
//   type(C_PTR) :: b
// end function ShmemHeapAllocBlock
// F_EnD
//  C_StArT
//! allocate space on a Server Heap
//! @return address of block, NULL in case of failure to allocate
void *ShmemHeapAllocBlock(
  void          *addr,                //!< [in]  address of Server Heap
  const size_t  num_requested_bytes,  //!< [in]  size in bytes of block to allocate
  const int32_t safe                  //!< [in]  if nonzero, perform operation under lock
  ){
//  C_EnD
  heap_element *heap = (heap_element *) addr;

  // Verify that the heap is registered (and get its index)
  const int32_t heap_index = ShmemHeapIndex(addr);
  if (heap_index == -1) return NULL;

  const heap_element num_heap_elem = heap[0];
  const heap_element limit = num_heap_elem - 1;

  const heap_element num_wanted_block_elem =
      (num_requested_bytes + sizeof(heap_element) -1) / sizeof(heap_element)  // round size UP (size in heap elements)
      + 4;                                                                    // Add head + tail + size elements

  if (heap[limit] > 1  || heap[limit] < -1) return NULL;  // not a Server Heap or corrupted information

  // Lock the heap (if requested)
  if (safe) acquire_idlock((int32_t*)(heap + limit), 0);

  heap_element* allocated_block = NULL;
  heap_element num_current_block_elem = 0;

  // Scan block list to find/make a large enough free block
  for (heap_element block_index = 1; block_index < limit; block_index += num_current_block_elem) {

    num_current_block_elem = (heap[block_index] < 0) ? -heap[block_index] : heap[block_index];   // retrieve absolute block size (negative if block is not free)
    if (heap[block_index] < 0) continue; // block is not free, skip to next one

    // Coalesce blocks, if possible and necessary
    {
      heap_element next_index = block_index + num_current_block_elem;   // next block
      while(next_index < limit && heap[next_index] > 2 && num_current_block_elem < num_wanted_block_elem) {
        // Next block is free and within the heap, and the current block is still too small
        num_current_block_elem += heap[next_index];             // combined size
        next_index = block_index + num_current_block_elem;      // new next after block coalescing

        // Update (newly formed) current block
        heap[block_index]     = num_current_block_elem;         // head size marker
        heap[block_index + 1] = HEAD;
        heap[next_index - 2]  = TAIL;
        heap[next_index - 1]  = num_current_block_elem;         // tail size marker
      }
    }

    if (num_wanted_block_elem <= num_current_block_elem) {     // block large enough to satisfy request
      allocated_block = heap + block_index + 2;                 // point to element following size marker
      // printf("Found block at %p (block_index + 2 = %ld), heap at %p\n", (void*)allocated_block, block_index + 2, (void*)heap);
      (heap_table[heap_index].stats)->nblk += 1 ;
      (heap_table[heap_index].stats)->nbyt += num_requested_bytes;

      // Split block if worth it
      if (num_current_block_elem - num_wanted_block_elem > BLOCK_SPLIT_SIZE) {
        heap[block_index]                             = num_wanted_block_elem; // head count (lower block)
        heap[block_index + 1]                         = HEAD;
        heap[block_index + num_wanted_block_elem - 2] = TAIL;
        heap[block_index + num_wanted_block_elem - 1] = num_wanted_block_elem; // tail count (lower block)

        heap[block_index + num_wanted_block_elem]      = num_current_block_elem - num_wanted_block_elem ;  // head count (upper block)
        heap[block_index + num_wanted_block_elem + 1]  = HEAD;
        heap[block_index + num_current_block_elem - 2] = TAIL;
        heap[block_index + num_current_block_elem - 1] = num_current_block_elem - num_wanted_block_elem ;  // tail count (upper block)

        num_current_block_elem = num_wanted_block_elem;
        }

        heap[block_index] = -num_current_block_elem;

        // Update high-water mark if needed
        if ((uint64_t)(block_index + num_current_block_elem) > (heap_table[heap_index].stats)->max) {
          (heap_table[heap_index].stats)->max = (block_index + num_current_block_elem);
        }

      break; // We have our block, so we can stop iterating now
      }
  }

  // Unlock heap if needed
  if (safe) release_idlock((int32_t*)(heap + limit), 0);

  return allocated_block;
}

// F_StArT
// function ShmemHeapSetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapSetBlockMeta')
//   import :: C_PTR, C_INT, block_meta_c
//   implicit none
//   type(C_PTR), intent(IN), value    :: block
//   type(block_meta_c), intent(IN)    :: metadata
//   integer(C_INT), intent(IN), value :: msz
//   integer(C_INT) :: status
// end function ShmemHeapSetBlockMeta
// F_EnD
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

// F_StArT
// function ShmemHeapGetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapGetBlockMeta')
//   import :: C_PTR, C_INT, block_meta_c
//   implicit none
//   type(C_PTR), intent(IN), value    :: block
//   type(block_meta_c), intent(OUT)    :: metadata
//   integer(C_INT), intent(IN), value :: msz
//   integer(C_INT) :: status
// end function ShmemHeapGetBlockMeta
// F_EnD
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

// F_StArT
// function ShmemHeapFreeBlock(block) result(status) bind(C,name='ShmemHeapFreeBlock')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: block
//   integer(C_INT) :: status
// end function ShmemHeapFreeBlock
// F_EnD
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

  if (status != 0) {   // Not a valid block pointer (maybe bc the heap is unknown)
    return -1;
  }

  h = h - 2;                       // point to count (two elements below block)
  nw = h[0];                       // if block is in use, nw will be negative
  if(nw >= 0) return -2 ;          // certainly not a block in use
  nw = -nw ;                       // make count positive
  full_memory_fence();             // make sure every read operation within this block has been completed before freeing it
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

//F_StArT
//  end interface
//F_EnD
