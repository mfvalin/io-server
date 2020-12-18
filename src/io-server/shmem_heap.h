/*
 * Copyright (C) 2020  Environnement Canada
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
 *     M. Valin,   Recherche en Prevision Numerique, 2020
 *     V. Magnoux, Recherche en Prevision Numerique, 2020
 */
// This file has been generated from shmem_heap.c
#ifndef IO_SERVER_shmem_heap_GEN_H
#define IO_SERVER_shmem_heap_GEN_H

#include <io-server/common.h>

//!> heap element (see data_element in common.h)<br>
//! if data_element is int32_t, "small" heap, with no more than 2*1024*1024*1024 - 1 elements (8 GBytes)
typedef data_element heap_element ;     // remain consistent with io-server package

//!> maximum number of registered heaps
#define MAX_HEAPS 64

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

//!> metadata description for Fortran up to 5D arrays
typedef  struct {
    int d[5];
    int tkr ;
  } meta_c;
void ShmemHeapDumpInfo(
     );
int ShmemHeapGetInfo(
  int index,
  int64_t *size,
  int64_t *max,
  int64_t *nblk,
  int64_t *nbyt
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
int32_t ShmemHeapPtr2Offset(
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
