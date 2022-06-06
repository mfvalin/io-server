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

// C_StArT
#include <stddef.h>
#include <stdint.h>

#include "io-server/rpn_extra.h"
// C_EnD

#include <math.h>
#include <stdio.h>
#include <stdlib.h>


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
typedef int64_t heap_element ; //!< heap element (should be 64 bits, to be able to contain its own size)
//    C_EnD

const heap_element HEAP_META_MARKER = 0x12345678; //!< Marker to indicate the start of heap metadata and that a heap is initialized
const heap_element HEAD             = 0xCAFEFADE; //!< HEAD marker below block
const heap_element TAIL             = 0xBEBEFADA; //!< TAIL marker above block

const heap_element BLOCK_SPLIT_SIZE = 64; //!< Number of extra elements in a potential block to decide to split it
const heap_element MIN_HEAP_SIZE = BLOCK_SPLIT_SIZE + 4;

//C_StArT
//!> heap statistics
typedef struct{
  uint64_t max_fill;              //!< high water of heap (highest allocation point, in number of elements)
  uint64_t total_num_allocations; //!< number of block allocations in the heap's lifetime
  uint64_t total_num_alloc_elems; //!< number of elements allocated in the heap's lifetime
} heap_stats;

typedef struct {
  heap_element marker;            //!< Marker to be able to verify (a little bit) that a heap exists
  size_t       full_size;         //!< Space taken by the entire heap and its metadata within the shared memory region, in number of #heap_element
  size_t       capacity;          //!< Number of data elements the heap can hold (maximum possible allocation)
  heap_element stats_index;       //!< Index of the stats struct within the shared memory region (in number of #heap_element)
  heap_element first_block_index; //!< Index of the start of the heap itself within the shared memory region (in number of #heap_element)
  heap_element limit;             //!< Index beyond the last valid index within the heap
  int32_t      lock;              //!< Variable used to lock/unlock the heap for certain operations
} heap_metadata;

/**
 * @brief Local access point to a heap whose data is located in shared memory and thus accessible to other processes.
 * 
 * This is basically a set of pointers to the data itself: some metadata, some stats, the heap contents
 * 
 */
typedef struct {
  heap_metadata* meta;        //!< General information about the state of the heap
  heap_stats*    stats;       //!< Usage statistics
  heap_element*  first_block; //!< Start of the heap itself
} shmem_heap;
//C_EnD

//!> metadata description for Fortran (up to 5D arrays)
typedef  struct {
    int d[5];             //!< the 5 allowed dimensions     (private information, DO NOT USE)
    int tkr ;             //!< type, kind, rank information (private information, DO NOT USE)
  } meta_c;
// \endcond

// //!> number of registered heaps
// static int32_t num_heaps = 0 ;

// //!> table containing registered heap information
// static heap_item heap_table[MAX_HEAPS] ;

// //!> default heap (used by some functions if heap address is NULL)
// static void *default_heap = NULL ;

//F_StArT
//  interface
//F_EnD

//! Compute the number of #heap_element needed to store the given number of bytes
static inline size_t num_elem_from_bytes(const size_t num_bytes) {
  return (num_bytes + sizeof(heap_element) - 1) / sizeof(heap_element);
}

//! Verify that the local struct points to a valid heap. If it does, initialize
//! the remaining local struct pointers.
//! @return Pointer to itself if heap is initialized, NULL otherwise
static inline shmem_heap* ShmemHeap_check_init(
  shmem_heap* heap //!< [in,out] Pointer to the local heap struct that we want to check
) {
  // printf("heap = %p\n", (void*)heap);
  // if (heap) printf("heap->meta = %p\n", (void*)heap->meta);
  // if (heap && heap->meta) printf("heap->meta->marker = %lx\n", heap->meta->marker);
  if (heap != NULL && heap->meta != NULL && heap->meta->marker == HEAP_META_MARKER) // Yay heap exists
  {
    if (heap->stats == NULL)       heap->stats       = (heap_stats*)((heap_element*)heap->meta + heap->meta->stats_index);
    if (heap->first_block == NULL) heap->first_block = (heap_element*)heap->meta + heap->meta->first_block_index;
    return heap;
  }

  return NULL;
}

//C_StArT
//!> Lock this heap for exclusive access
static inline void ShmemHeap_lock(
    shmem_heap* heap //!< [in,out] Pointer to a _valid_ heap
) {
  acquire_idlock(&heap->meta->lock, 0);
}

//!> Relinquish exclusive access to this heap
static inline void ShmemHeap_unlock(
    shmem_heap* heap //!< [in,out] Pointer to a _valid_ heap
) {
  release_idlock(&heap->meta->lock, 0);
}
//C_EnD

// F_StArT
// subroutine ShmemHeap_lock(heap) bind(C, name='ShmemHeap_lock_f')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: heap
// end subroutine ShmemHeap_lock
// subroutine ShmemHeap_unlock(heap) bind(C, name='ShmemHeap_unlock_f')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: heap
// end subroutine ShmemHeap_unlock
// F_EnD
void ShmemHeap_lock_f(shmem_heap* heap) { if (ShmemHeap_check_init(heap) != NULL) ShmemHeap_lock(heap); }
void ShmemHeap_unlock_f(shmem_heap* heap) { if (ShmemHeap_check_init(heap) != NULL) ShmemHeap_unlock(heap); }

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
// subroutine ShmemHeap_dump_info(heap) bind(C,name='ShmemHeap_dump_info')
//   import :: C_PTR
//   implicit none
//   type(C_PTR), intent(in), value :: heap
// end subroutine ShmemHeap_dump_info
// F_EnD
//  C_StArT
//! print heap statistics
//! @return none
void ShmemHeap_dump_info(
    shmem_heap *heap
){
//  C_EnD
  if (ShmemHeap_check_init(heap) != NULL) {
    printf("Heap shared mem at %p\nTotal heap size %ld bytes (can allocate %ld bytes)\n"
           "High point %ld bytes, allocated %ld blocks (%ld bytes)\n",
           (void*)heap->meta, heap->meta->full_size * sizeof(heap_element), heap->meta->capacity * (sizeof(heap_element)),
           heap->stats->max_fill * sizeof(heap_element), heap->stats->total_num_allocations, heap->stats->total_num_alloc_elems * (sizeof(heap_element)));
  }
  else {
    printf("Address %p does not contain a valid heap\n", (void*)heap);
  }
}

// F_StArT
// function ShmemHeap_get_info(heap, sz, max, nblk, nbyt) result(status) bind(C,name='ShmemHeap_get_info')
//   import :: C_INT, C_INT64_T, C_PTR
//   implicit none
//   type(C_PTR), intent(in), value    :: heap
//   integer(C_INT64_T), intent(OUT) :: sz
//   integer(C_INT64_T), intent(OUT) :: max
//   integer(C_INT64_T), intent(OUT) :: nblk
//   integer(C_INT64_T), intent(OUT) :: nbyt
//   integer(C_INT) :: status
// end function ShmemHeap_get_info
// F_EnD
//  C_StArT
//! get heap statistics
//! @return 0 if O.K., nonzero if error
int ShmemHeap_get_info(
  shmem_heap *heap,   //!< [in,out]  Pointer to heap
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

  if (ShmemHeap_check_init(heap) == NULL) return -1;

  *size = heap->meta->capacity * sizeof(heap_element);
  *max  = heap->stats->max_fill * sizeof(heap_element);
  *nblk = heap->stats->total_num_allocations;
  *nbyt = heap->stats->total_num_alloc_elems * sizeof(heap_element);

  return 0 ;
}

// F_StArT
// function ShmemHeap_get_size(heap) result(s) bind(C,name='ShmemHeap_get_size')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(HEAP_ELEMENT) :: s
// end function ShmemHeap_get_size
// F_EnD
//  C_StArT
//! Retrieve size of the heap
//! @return size (in bytes) if a known heap, -1 otherwise
heap_element ShmemHeap_get_size(
  shmem_heap *heap                    //!< [in,out]  Heap pointer
  ){
//  C_EnD
  if (ShmemHeap_check_init(heap) == NULL) return -1;
  return heap->meta->capacity * sizeof(heap_element);
}


// F_StArT
// function ShmemHeap_is_block_valid(heap, block) result(status) bind(C,name='ShmemHeap_is_block_valid')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   type(C_PTR), intent(IN), value :: block
//   integer(C_INT) :: status
// end function ShmemHeap_is_block_valid
// F_EnD
//  C_StArT
//! Check whether the given block is valid in this heap
//! @return 0 if the block is valid, -1 if there is an error
int32_t ShmemHeap_is_block_valid(
  shmem_heap   *heap,   //!< [in,out] The heap for which we want to check a block
  heap_element *block   //!< [in]     Pointer to the block we want to check
  ){
//  C_EnD
  if (ShmemHeap_check_init(heap) == NULL) return -1;
  if (block == NULL) return -1;

  const heap_element *block_start = block - 2;

  if (block_start[1] != HEAD) {
    // printf("HEAD wrong (%ld, should be %ld)\n", b[1], HEAD);
    return -1;
  }

  const heap_element sz = labs(block_start[0]);  // get block size (negative means block is in use)
  if (sz < 5) {
    // printf("Size wrong (%ld, cannot be less than 5)\n", sz);
    return -1;
  }

  if (sz >= heap->meta->limit) {
    // printf("Block TOP is out of the heap!\n");
    return -1;
  }

  if (block_start[sz-2] != TAIL) {
    // printf("TAIL wrong (%ld, should be %ld)\n", b[sz-2], TAIL);
    return -1;
  }

  if (block_start[sz-1] != sz) {
    // printf("Trailing size marker is wrong (%ld, should be %ld)\n", b[sz-1], sz);
    return -1;
  }

  return 0;                                 // this looks like a valid block
}

// F_StArT
// function ShmemHeap_ptr_from_offset(heap, offset) result(p) bind(C,name='ShmemHeap_ptr_from_offset')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(HEAP_ELEMENT), intent(IN), value :: offset
//   type(C_PTR) :: p
// end function ShmemHeap_ptr_from_offset
// F_EnD
//  C_StArT
//! translate offset from base of heap into actual address
//! @return address, NULL if offset out of heap
heap_element *ShmemHeap_ptr_from_offset(
  shmem_heap *heap,             //!< [in]  heap address
  heap_element offset           //!< [in]  offset into heap
  ){
//  C_EnD
  if (ShmemHeap_check_init(heap) == NULL) return NULL;        // Invalid heap
  if (offset < 0 || offset >= heap->meta->limit) return NULL; // Invalid offset

  return heap->first_block + offset;
}

// F_StArT
//  function ShmemHeap_block_size_from_pointer(heap, block) result(num_bytes) bind(C, name='ShmemHeap_block_size_from_pointer')
//    import :: C_PTR, C_SIZE_T
//    implicit none
//    type(C_PTR), intent(in), value :: heap
//    type(C_PTR), intent(in), value :: block
//    integer(C_SIZE_T) :: num_bytes
//  end function ShmemHeap_block_size_from_pointer
// F_EnD
size_t ShmemHeap_block_size_from_pointer(
    shmem_heap* heap,     //!< [in,out] Heap we are querying
    heap_element* block   //!< [in]     Address of block we are querying
) {
  if (ShmemHeap_is_block_valid(heap, block) != 0) return 0;
  return labs(block[-2]);
}

// F_StArT
//  function ShmemHeap_block_size_from_offset(heap, offset) result(num_bytes) bind(C, name='ShmemHeap_block_size_from_offset')
//    import :: C_PTR, HEAP_ELEMENT, C_SIZE_T
//    implicit none
//    type(C_PTR), intent(in), value :: heap
//    integer(HEAP_ELEMENT), intent(in), value :: offset
//    integer(C_SIZE_T) :: num_bytes
//  end function ShmemHeap_block_size_from_offset
// F_EnD
size_t ShmemHeap_block_size_from_offset(
    shmem_heap* heap,
    heap_element offset
) {
  const heap_element* block = ShmemHeap_ptr_from_offset(heap, offset);
  if (block == NULL) return 0;
  return labs(block[-2]);
}

// F_StArT
// function ShmemHeap_offset_from_pointer(heap, block) result(offset) bind(C, name='ShmemHeap_offset_from_pointer')
//   import :: C_PTR, HEAP_ELEMENT
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   type(C_PTR), intent(IN), value :: block
//   integer(HEAP_ELEMENT) :: offset
// end function ShmemHeap_offset_from_pointer
// F_EnD
//! Translate address inside heap into an offset (of size of #heap_element). The pointer must be inside the heap!
// C_StArT
heap_element ShmemHeap_offset_from_pointer(
    shmem_heap   *heap, //!< [in] Heap address (heap must be valid!)
    heap_element *block //!< [in] Address of the block for which we want the offset
  ) {
// C_EnD
  return block - heap->first_block;
}

// F_StArT
// function ShmemHeap_init_from_scratch(heap, nbytes) result(h) bind(C,name='ShmemHeap_init_from_scratch')
//   import :: C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_SIZE_T), intent(IN), value :: nbytes
//   type(C_PTR) :: h
// end function ShmemHeap_init_from_scratch
// F_EnD
//  C_StArT
//! initialize a Server Heap from a pointer to (empty) shared memory
//! @return Pointer to heap struct if successful, NULL otherwise
shmem_heap *ShmemHeap_init_from_scratch(
  void *shmem_ptr,               //!< [in]  desired heap address in shared memory
  size_t num_bytes               //!< [in]  size in bytes of space pointed to by shmem_ptr
  ){
//  C_EnD
  if (shmem_ptr == NULL) return NULL;

  shmem_heap *heap = (shmem_heap*)malloc(sizeof(shmem_heap));
  if (heap == NULL) return NULL;

  heap_metadata* meta = shmem_ptr;

  // Initialize metadata
  meta->stats_index       = num_elem_from_bytes(sizeof(heap_metadata));
  meta->first_block_index = meta->stats_index + num_elem_from_bytes(sizeof(heap_stats));
  meta->full_size         = num_bytes / sizeof(heap_element);
  meta->capacity          = meta->full_size - meta->first_block_index;
  meta->limit             = meta->capacity - 2;
  meta->lock              = 0;

  // Set up the pointers
  heap->meta = shmem_ptr;
  heap->stats       = (heap_stats*)((heap_element*)heap->meta + meta->stats_index);
  heap->first_block = (heap_element*)heap->meta + meta->first_block_index;

  // Init stats
  heap_stats* stats = heap->stats;
  stats->max_fill              = 0;   // high water mark
  stats->total_num_allocations = 0;   // allocated blocks
  stats->total_num_alloc_elems = 0;   // allocated elements

  // Init first block (empty, entire heap)
  heap->first_block[0] = meta->capacity;
  heap->first_block[1] = HEAD;
  heap->first_block[meta->capacity - 2] = TAIL;
  heap->first_block[meta->capacity - 1] = meta->capacity;

  // Indicate heap is initialized. Gotta do this last!
  meta->marker        = HEAP_META_MARKER;

  return heap;  // O.K. return address of Heap
}

// F_StArT
// function ShmemHeap_clone(shmem_ptr) result(status) bind(C, name='ShmemHeap_clone')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: shmem_ptr
//    type(C_PTR) :: status
// end function ShmemHeap_clone
// F_EnD
//!> Initialize local heap access from an existing heap in shared memory
//!> @return Pointer to heap struct if successful, NULL otherwise
shmem_heap *ShmemHeap_clone(void *shmem_ptr) {
  if (shmem_ptr == NULL) return NULL;
  shmem_heap *heap = (shmem_heap*)malloc(sizeof(shmem_heap));
  if (heap == NULL) return NULL;
  heap->meta        = shmem_ptr;
  heap->stats       = NULL;
  heap->first_block = NULL;

  ShmemHeap_check_init(heap);
  return heap;
}

// F_StArT
// subroutine ShmemHeap_delete(heap) bind(C, name='ShmemHeap_delete')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: heap
// end subroutine ShmemHeap_delete
// F_EnD
// C_StArT
//! Delete a heap. This just frees the local struct for now
void ShmemHeap_delete(
  shmem_heap* heap //!< [in,out] Address of the heap to delete
)
{
// C_EnD
  if (heap != NULL) free(heap);
}

// F_StArT
// function ShmemHeap_check(heap, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeap_check')
//   import :: C_INT, C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
//   integer(C_SIZE_T), intent(OUT) :: free_space, used_space
//   integer(C_INT) :: status
// end function ShmemHeap_check
// F_EnD
//  C_StArT
//! check integrity of Server Heap
//! @return 0 : O.K.<br>
//!         1 : bad address<br>
//!         2 : not a valid heap or corrupted information<br>
//!         3 : size marker is invalid<br>
//!         4 : tail size marker not consistent
int32_t ShmemHeap_check(
  shmem_heap *heap,               //!< [in]  Pointer to a shmem heap
  int32_t *free_blocks,           //!< [out] number of free blocks
  size_t *free_space,             //!< [out] available space in bytes
  int32_t *used_blocks,           //!< [out] number of used blocks
  size_t *used_space              //!< [out] used space in bytes
  ){
//  C_EnD
  // heap_element *h ;
  heap_element sz ;
  // heap_element cur, limit ;

  int32_t free, used ;
  size_t space_used, space_free ;

  *free_blocks = 0 ; free = 0 ;
  *free_space  = 0 ; used = 0 ;
  *used_blocks = 0 ; space_used = 0 ;
  *used_space  = 0 ; space_free = 0 ;

  if (ShmemHeap_check_init(heap) == NULL) return 2; // No heap exists there

  heap_element* data = heap->first_block;
  const heap_element limit = heap->meta->limit;
  free = 0 ; space_free = 0 ;
  used = 0 ; space_used = 0 ;
  for(heap_element block_index = 0; block_index < limit; block_index += sz) {  // go through block chain
    // if(h[cur] == 0) break;                      // top of heap

    sz = (data[block_index] > 0) ? data[block_index] : -data[block_index] ;     // size of block (negative value means block in use)

    // valid size must be > 4
    if(sz < 5 && sz > 0) return 3 ;

    // Check head/tail markers
    if(data[block_index+1] != HEAD || data[block_index+sz-2] != TAIL){
      printf("trampled block bounds marker(s), expected %8.8lx %8.8lx, found %8.8lx %8.8lx\n",
             HEAD, TAIL, data[block_index+1], data[block_index+sz-2]);
      return 4; 
    }

    // Check tail size marker
    if(data[block_index+sz-1] != sz) {
      printf("bad tail size marker, got %ld, expected %ld\n",data[block_index+sz-1], sz);
      return 4;
    }

    // Count free/used
    if (data[block_index] > 0) {
      free++;
      space_free = space_free + sz - 4;
    } else {
      used++;
      space_used = space_used + sz - 4;
    }
  }

  *free_blocks = free;
  *free_space  = space_free * sizeof(heap_element);   // convert size into bytes
  *used_blocks = used;
  *used_space  = space_used * sizeof(heap_element);   // convert size into bytes

  return 0 ;
}

// F_StArT
// function ShmemHeap_alloc_block(heap, nbytes, safe) result(b) bind(C,name='ShmemHeap_alloc_block')
//   import :: C_INT, C_PTR, C_SIZE_T
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   integer(C_SIZE_T), intent(IN), value :: nbytes
//   integer(C_INT), intent(IN), value :: safe
//   type(C_PTR) :: b
// end function ShmemHeap_alloc_block
// F_EnD
//  C_StArT
//! allocate space on a Server Heap
//! @return address of block, NULL in case of failure to allocate
void *ShmemHeap_alloc_block(
  shmem_heap    *heap,                //!< [in,out] Pointer to heap struct
  const size_t  num_requested_bytes,  //!< [in]  size in bytes of block to allocate
  const int32_t safe                  //!< [in]  if nonzero, perform operation under lock
  ){
//  C_EnD

  if (ShmemHeap_check_init(heap) == NULL) return NULL;

  const heap_element num_wanted_block_elem = num_elem_from_bytes(num_requested_bytes) + 4; // Add head + tail + size elements

  heap_element* data            = heap->first_block;
  heap_element* allocated_block = NULL;
  heap_element num_current_block_elem = 0;

  const heap_element limit = heap->meta->limit;

  if (num_wanted_block_elem >= limit) {
    printf("WARNING: Asking for %ld elements, which is more than the heap contains (%ld). You won't ever get a block that size.\n", num_wanted_block_elem, limit-1);
    return NULL;
  }

  // Lock the heap (if requested)
  if (safe) ShmemHeap_lock(heap);

  // Scan block list to find/make a large enough free block
  for (heap_element block_index = 0; block_index < limit; block_index += num_current_block_elem) {

    num_current_block_elem = (data[block_index] < 0) ? -data[block_index] : data[block_index];   // retrieve absolute block size (negative if block is not free)
    if (data[block_index] < 0) continue; // block is not free, skip to next one

    // Coalesce blocks, if possible and necessary
    {
      heap_element next_index = block_index + num_current_block_elem;   // next block
      while(next_index < limit && data[next_index] > 2 && num_current_block_elem < num_wanted_block_elem) {
        // Next block is free and within the heap, and the current block is still too small
        num_current_block_elem += data[next_index];             // combined size
        next_index = block_index + num_current_block_elem;      // new next after block coalescing

        // Update (newly formed) current block
        data[block_index]     = num_current_block_elem;         // head size marker
        data[block_index + 1] = HEAD;
        data[next_index - 2]  = TAIL;
        data[next_index - 1]  = num_current_block_elem;         // tail size marker
      }
    }

    if (num_wanted_block_elem <= num_current_block_elem) {     // block large enough to satisfy request

      // Split block if worth it
      if (num_current_block_elem - num_wanted_block_elem > BLOCK_SPLIT_SIZE) {
        data[block_index]                             = num_wanted_block_elem; // head count (lower block)
        data[block_index + 1]                         = HEAD;
        data[block_index + num_wanted_block_elem - 2] = TAIL;
        data[block_index + num_wanted_block_elem - 1] = num_wanted_block_elem; // tail count (lower block)

        data[block_index + num_wanted_block_elem]      = num_current_block_elem - num_wanted_block_elem ;  // head count (upper block)
        data[block_index + num_wanted_block_elem + 1]  = HEAD;
        data[block_index + num_current_block_elem - 2] = TAIL;
        data[block_index + num_current_block_elem - 1] = num_current_block_elem - num_wanted_block_elem ;  // tail count (upper block)

        num_current_block_elem = num_wanted_block_elem;
      }

      // We have our block! Point to the first element, following the size/head marker
      allocated_block = data + block_index + 2;
      data[block_index] = -num_current_block_elem;

      // Update stats
      heap->stats->total_num_allocations++;
      heap->stats->total_num_alloc_elems += num_current_block_elem;

      // Update high-water mark if needed
      if ((uint64_t)(block_index + num_current_block_elem) > heap->stats->max_fill) {
        heap->stats->max_fill = block_index + num_current_block_elem;
      }

      break; // We have our block, so we can stop iterating now
    }
  }

  // Unlock heap if needed
  if (safe) ShmemHeap_unlock(heap);

  return allocated_block;
}

// F_StArT
// function ShmemHeap_set_block_meta(heap, block, block_metadata, block_meta_size) result(status) bind(C,name='ShmemHeap_set_block_meta')
//   import :: C_PTR, C_INT, block_meta_c
//   implicit none
//   type(C_PTR), intent(IN), value    :: heap
//   type(C_PTR), intent(IN), value    :: block
//   type(block_meta_c), intent(IN)    :: block_metadata
//   integer(C_INT), intent(IN), value :: block_meta_size
//   integer(C_INT) :: status
// end function ShmemHeap_set_block_meta
// F_EnD
//  C_StArT
//! set block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_set_block_meta(
  shmem_heap    *heap,        //!< [in,out] The heap where the block belongs
  heap_element  *block,       //!< [in]     Address of block
  unsigned char *block_meta,  //!< [in]     Address of block metadata
  int           num_bytes     //!< [in]     Size of block metadata (bytes)
  ){
//  C_EnD

  // Check inputs
  if (ShmemHeap_is_block_valid(heap, block) != 0) return -1;
  if (block_meta == NULL || num_bytes <= 0) return -1;

  heap_element* block_start = block - 2;
  const heap_element block_size = -block_start[0];

  if (block_size < 0) return -1;  // block is free, can't use it!

  // Put block metadata at end of block
  unsigned char* block_meta_start = (unsigned char*)(block_start + block_size - 2) - num_bytes;
  for(int i = 0; i < num_bytes; i++) block_meta_start[i] = block_meta[i];

  return 0 ;
}

// F_StArT
// function ShmemHeap_get_block_meta(heap, block, metadata, msz) result(status) bind(C,name='ShmemHeap_get_block_meta')
//   import :: C_PTR, C_INT, block_meta_c
//   implicit none
//   type(C_PTR), intent(IN), value    :: heap
//   type(C_PTR), intent(IN), value    :: block
//   type(block_meta_c), intent(OUT)    :: metadata
//   integer(C_INT), intent(IN), value :: msz
//   integer(C_INT) :: status
// end function ShmemHeap_get_block_meta
// F_EnD
//  C_StArT
//! get block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_get_block_meta(
    shmem_heap    *heap,          //!< [in,out] Heap where the block is located
    heap_element  *block,         //!< [in]   address of block
    unsigned char *meta_dest,     //!< [out]  address of metadata (user array to receive metadata)
    int           num_bytes       //!< [in]   size of metadata (bytes)
  ){
//  C_EnD

  // Check inputs
  if (ShmemHeap_is_block_valid(heap, block) != 0) return -1;
  if (num_bytes <= 0 || meta_dest == NULL) return -1;

  heap_element* block_start = block - 2;
  const heap_element block_size = -block_start[0];
  if(block_size < 0) return -1; // block is free, can't take its metadata!

  // Get metadata at the end of block
  unsigned char *meta_start = (unsigned char*)(block_start + block_size - 2) - num_bytes;
  for (int i = 0; i < num_bytes; i++) meta_dest[i] = meta_start[i];

  return 0 ;
}

// F_StArT
// function ShmemHeap_free_block(heap, block) result(status) bind(C,name='ShmemHeap_free_block')
//   import :: C_INT, C_PTR
//   implicit none
//   type(C_PTR), intent(IN), value :: heap
//   type(C_PTR), intent(IN), value :: block
//   integer(C_INT) :: status
// end function ShmemHeap_free_block
// F_EnD
//  C_StArT
//! free space on a Server Heap
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_free_block(
    shmem_heap   *heap,     //!< [in,out] Heap where the block is located
    heap_element *block     //!< [in]  address of block
    ){
//  C_EnD
  if (ShmemHeap_is_block_valid(heap, block) != 0) return -1;

  heap_element* block_start = block - 2;
  heap_element block_size = block_start[0];

  if (block_size >= 0) return -1;  // This block is free! Can't free it more...

  block_size = -block_size ;       // Make count positive (to indicate free)
  full_memory_fence();             // Make sure every read operation within this block has been completed before freeing it
  block_start[0] = block_size;     // Actually mark memory block as free

  return 0;
}

//F_StArT
//  end interface
//F_EnD
