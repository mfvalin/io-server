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
// This file has been generated from shmem_heap.c
#ifndef IO_SERVER_shmem_heap_GEN_H
#define IO_SERVER_shmem_heap_GEN_H

/******************************************************************************
         INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
   --------------------------------------------------------------------------
 We use the '//C_StArT' and '//C_EnD' tags to indicate the beginning and end of
 extraction.
 To extract the entire function (for inline functions, for example), you must
 put the begin/end tags around the entire function code, and **MAKE SURE TO
 LEAVE A SPACE** between the closing parenthesis of the function header and the
 opening bracket of the function body. They have to be on the same line.

 For example:
     //C_StArT
     inline int my_function(type1 arg1, type2* arg2) {
         [function body]
     }
     //C_EnD

 or also:
     //C_StArT
     inline int my_function(type1 arg1, //!< Doxygen doc
                            type2* arg2 //!< More doc
       ) {
         [function body]
     }
     //C_EnD


 To extract the function interface only, you must put the begin/end tags around
 the header. The closing parenthesis/opening bracket must either
  - be on the same line, without a space between them
or
  - be on different lines, with the closing parenthesis by itself on a line

 For example:
     //C_StArT
     int my_function(
         type1 arg1, //! Doxygen doc
         type2* arg2 //! Moar doc
         )
     //C_EnD
     {
         [function body]
     }

 or also:
     //C_StArT
     int my_function(type1 arg1, type2* arg2){
     //C_EnD
         [function body]
     }
 ******************************************************************************/

#include <stddef.h>
#include <stdint.h>

#include "io-server/rpn_extra.h"
typedef int64_t heap_element ; //!< heap element (should be 64 bits, to be able to contain its own size)
//!> heap statistics
typedef struct{
  uint64_t max_fill;     //!< high water of heap (highest allocation point)
  uint64_t num_blocks;   //!< number of block allocations
  uint64_t num_elements; //!< number of elements allocated
} heap_stats;

typedef struct {
  heap_element marker;            //!< Marker to be able to verify (a little bit) that a heap exists
  size_t full_size;               //!< Space taken by the entire heap and its metadata within the shared memory region, in number of #heap_element
  size_t num_elements;            //!< Number of data elements the heap can hold (maximum possible allocation)
  heap_element stats_index;       //!< Index of the stats struct within the shared memory region (in number of #heap_element)
  heap_element first_block_index; //!< Index of the start of the heap itself within the shared memory region (in number of #heap_element)
  heap_element limit;             //!< Index beyond the last valid index within the heap
  int32_t lock;                   //!< Variable used to lock/unlock the heap for certain operations
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
//! add offset to pointer in specified units (1/2/4/8/16 bytes)
//! @return pointer after adding offset in specified units (1/2/4/8/16 bytes)
void *Pointer_add_offset(
  void *ref,                //!< [in]  reference address
  intptr_t offset,          //!< [in]  offset to apply
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  );
//! print heap statistics
//! @return none
void ShmemHeap_dump_info(
    shmem_heap *heap
);
//! get heap statistics
//! @return 0 if O.K., nonzero if error
int ShmemHeap_get_info(
  shmem_heap *heap,   //!< [in,out]  Pointer to heap
  int64_t *size,      //!< [out] size of heap (bytes)
  int64_t *max,       //!< [out] high water mark in heap  (highest allocation point) (bytes)
  int64_t *nblk,      //!< [out] number of blocks that have been allocated
  int64_t *nbyt       //!< [out] total number of bytes used by allocated blocks
     );
//! Retrieve size of the heap
//! @return size (in bytes) if a known heap, -1 otherwise
heap_element ShmemHeap_get_size(
  shmem_heap *heap                    //!< [in,out]  Heap pointer
  );
//! Check whether the given block is valid in this heap
//! @return 0 if the block is valid, -1 if there is an error
int32_t ShmemHeap_is_block_valid(
  shmem_heap   *heap,   //!< [in,out] The heap for which we want to check a block
  heap_element *block   //!< [in]     Pointer to the block we want to check
  );
//! translate offset from base of heap into actual address
//! @return address, NULL if offset out of heap
heap_element *ShmemHeap_ptr_from_offset(
  shmem_heap *heap,             //!< [in]  heap address
  heap_element offset           //!< [in]  offset into heap
  );
heap_element ShmemHeap_offset_from_pointer(
    shmem_heap   *heap, //!< [in] Heap address (heap must be valid!)
    heap_element *block //!< [in] Address of the block for which we want the offset
  ) {
//! initialize a Server Heap from a pointer to (empty) shared memory
//! @return Pointer to heap struct if successful, NULL otherwise
shmem_heap *ShmemHeap_init_from_scratch(
  void *shmem_ptr,               //!< [in]  desired heap address in shared memory
  size_t num_bytes               //!< [in]  size in bytes of space pointed to by shmem_ptr
  );
//! Delete a heap. This just frees the local struct for now
void ShmemHeap_delete(
  shmem_heap* heap //!< [in,out] Address of the heap to delete
);
{
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
  );
//! allocate space on a Server Heap
//! @return address of block, NULL in case of failure to allocate
void *ShmemHeap_alloc_block(
  shmem_heap    *heap,                //!< [in,out] Pointer to heap struct
  const size_t  num_requested_bytes,  //!< [in]  size in bytes of block to allocate
  const int32_t safe                  //!< [in]  if nonzero, perform operation under lock
  );
//! set block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_set_block_meta(
  shmem_heap    *heap,        //!< [in,out] The heap where the block belongs
  heap_element  *block,       //!< [in]     Address of block
  unsigned char *block_meta,  //!< [in]     Address of block metadata
  int           num_bytes     //!< [in]     Size of block metadata (bytes)
  );
//! get block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_get_block_meta(
    shmem_heap    *heap,          //!< [in,out] Heap where the block is located
    heap_element  *block,         //!< [in]   address of block
    unsigned char *meta_dest,     //!< [out]  address of metadata (user array to receive metadata)
    int           num_bytes       //!< [in]   size of metadata (bytes)
  );
//! free space on a Server Heap
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeap_free_block(
    shmem_heap   *heap,     //!< [in,out] Heap where the block is located
    heap_element *block     //!< [in]  address of block
    );

#endif // IO_SERVER_shmem_heap_GEN_H
