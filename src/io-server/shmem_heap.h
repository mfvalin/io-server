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

/**
 \file
 io-server/shmem_heap.h include file is needed to use shared memory heap functions<br>
 (extracted from file memory_arena.c)
*/
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
const heap_element HEAD = 0xCAFEFADE;

//!> TAIL marker above block
const heap_element TAIL = 0xBEBEFADA;

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
//! get offset between 2 pointers in specified units (1/2/4/8/16 bytes)
//! @return offset between 2 pointers in specified units (1/2/4/8/16 bytes)
intptr_t Pointer_offset(
  void *ref,                //!< [in]  reference address
  void *to,                 //!< [in]  pointer for whic a difference with ref is sought
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  );
//! add offset to pointer in specified units (1/2/4/8/16 bytes)
//! @return pointer after adding offset in specified units (1/2/4/8/16 bytes)
void *Pointer_add_offset(
  void *ref,                //!< [in]  reference address
  intptr_t offset,          //!< [in]  offset to apply
  uint32_t szeof            //!< [in]  size of element for offset purposes (power of 2)
  );
//! print heap statistics
//! @return none
void ShmemHeapDumpInfo(
     );
//! get heap statistics
//! @return 0 if O.K., nonzero if error
int ShmemHeapGetInfo(
  int index,          //!< [in]  heap index in registered heap table
  int64_t *size,      //!< [out] size of heap (bytes)
  int64_t *max,       //!< [out] high water mark in heap  (highest allocation point) (bytes)
  int64_t *nblk,      //!< [out] number of blocks that have been allocated
  int64_t *nbyt       //!< [out] total number of bytes used by allocated blocks
     );
//! get address of the default heap
//! @return default heap address (NULL if none)
void * ShmemHeapGetDefault(
  );
//! set this heap as the default heap
//! @return index in heap table if a known heap, -1 otherwise
int32_t ShmemHeapSetDefault(
  void *addr                          //!< [in]  address possibly of a known heap
  );
//! is this a known heap ?
//! @return index in heap table if a known heap, -1 otherwise
int32_t ShmemHeapIndex(
  void *addr                          //!< [in]  address possibly of a known heap
  );
//! is this a known heap ?
//! @return size if a known heap, -1 otherwise
heap_element ShmemHeapSize(
  void *addr                    //!< [in]  address possibly of a known heap
  );
//! which known heap does this address belong to ?
//! @return heap base address if within a known heap, NULL otherwise
heap_element *ShmemHeapContains(
  void *addr                    //!< [in]  address possibly in a known heap
  );
//! translate address to offset within a heap
//! @return offset with respect to base of heap in heap_element units (NOT bytes)
heap_element ShmemHeapPtr2Offset(
  void *addr                    //!< [in]  address to translate to index
  );
//! is this the address of a block belonging to a known heap ?
//! @return 0 if valid block from known heap,<br>
//!        -1 if unknown heap,<br>
//!         1 if inside a known heap but not a valid block pointer
int32_t ShmemHeapValidBlock(
  void *addr                    //!< [in]  putative valid block address
  );
//! is this the address of a block belonging to a known heap ?<br>
//! same as ShmemHeapValidBlock but returns block size code instead of true/false information
//! @return block size code if valid block from known heap,<br>
//!        -1 if unknown heap,<br>
//!         1 if inside known heap but not a proper block
heap_element ShmemHeapBlockSizeCode(
  void *addr                    //!< [in]  putative valid block address
  );
//! find the size of a used memory block (in bytes)<br>
//! uses either address of block or address of heap and offset
//! @return size of used block in bytes, 0 if not a block or block not in use
size_t ShmemHeapBlockSize(
  void *heap,                   //!< [in]  heap address (if NULL, only addr is used, offset is ignored)
  void *addr,                   //!< [in]  block address (if NULL, heap address and offset must be valid)
  heap_element offset           //!< [in]  offset into heap (ignored if heap is NULL or addr is not NULL)
  );
//! translate offset from base of heap into actual address
//! @return address, NULL if offset out of heap
void *ShmemHeapPtr(
  void *addr,                   //!< [in]  heap address
  heap_element offset           //!< [in]  offset into heap
  );
//! register a  Heap in the heap table
//! @return number of registered heaps if successful, -1 otherwise
int32_t ShmemHeapRegister(
  void *addr                    //!< [in]  heap address
  );
//! initialize a Server Heap
//! @return address of Server Heap if successful, NULL otherwise
void *ShmemHeapInit(
  void *addr,                    //!< [in]  desired heap address, if NULL, allocate space with malloc
  size_t sz                      //!< [in]  size in bytes of space pointed to by addr
  );
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
  );
//! allocate space on a Server Heap
//! @return address of block, NULL in case of failure to allocate
void *ShmemHeapAllocBlock(
  void *addr,                      //!< [in]  address of Server Heap
  size_t bsz,                      //!< [in]  size in bytes of block to allocate
  int32_t safe                     //!< [in]  if nonzero, perform operation under lock
  );
//! set block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapSetBlockMeta(
  void *addr,                      //!< [in]  address of block
  unsigned char *meta,             //!< [in]  address of metadata
  int msz                          //!< [in]  size of metadata (bytes)
  );
//! get block metadata
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapGetBlockMeta(
  void *addr,                      //!< [in]  address of block
  unsigned char *meta,             //!< [out]  address of metadata (user array to receive metadata)
  int msz                          //!< [in]  size of metadata (bytes)
  );
//! free space on a Server Heap
//! @return 0 if O.K., nonzero if error
int32_t ShmemHeapFreeBlock(
  void *addr                       //!< [in]  address of block
    );
//! check if Server Heap is locked
//! @return 0 if not locked, nonzero if locked
int32_t ShmemHeapIslocked(
  void *addr                      //!< [in]  address of Server Heap
  );
//! lock Server Heap
//! @return none
void ShmemHeapLock(
  void *addr                      //!< [in]  address of Server Heap
  );
//! unlock Server Heap
//! @return none
void ShmemHeapUnlock(
  void *addr                      //!< [in]  address of Server Heap
  );

#endif // IO_SERVER_shmem_heap_GEN_H
