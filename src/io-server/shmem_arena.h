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
// This file has been generated from shmem_arena.c
#ifndef IO_SERVER_shmem_arena_GEN_H
#define IO_SERVER_shmem_arena_GEN_H

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
 \brief memory arena management package (C and Fortran)

 code extracted from file shmem_arena.c
 \verbatim
    set of routines to implement named block management in a memory pool
    possibly shared by multiple threads and processes
  
            memory arena layout
    +--------------------+---------------------+-------------------->
    | arena header       | symbol table        | data blocks
    +--------------------+---------------------+-------------------->
  
    indices are used instead of addresses because the memory arena might be mapped 
    at different addresses in different processes
  
            data block layout
        +----------------------------------------------------------------- +
        |                                                                  |
        |                                                                  v
    +-------+-------+-------+-------+.....................+-------+-------+
    |  FWD  |  IX   |  NWD  | SIGNL |  user data portion  | SIGNH |  BWD  |
    +-------+-------+-------+-------+.....................+-------+-------+
    ^                                                                 |
    |                                                                 |
    +-----------------------------------------------------------------+
    FWD   : index of start of next block
    IX    : index in symbol table of this block
    NWD   : size of data portion in 64 bit units
    SIGNL : low marker (used for checking data underruns)
    SIGNH : high marker (used for checking data overruns)
    BWD   : index of start of this block
    FWD of last allocated block will point to a non existent block with FWD = 0
    
    FWD and BWD are indices into a 64 bit unsigned integer array starting at the beginning of the memory arena
    FWD, IX, NWD, SIGNL, SIGNH, BWD are 64 bit unsigned integers

    indices are used instead of addresses because processes sharing a memory segment are more
    than likely to have said memory segment mapped at different addresses

 \endverbatim
*/
#if ! defined(SymtabEntry64Size64)
#include <stdint.h>
//!> \brief memory arena structures

//!> symbol table entry (64 bit mode)
typedef struct{
  uint32_t lock;             //!< to lock this memory block
  uint32_t flags;            //!< control flags
  uint64_t data_index;       //!< index relative to start of memory arena
  uint64_t data_size;        //!< size of data portion of block (64 bit units)
  uint64_t data_name;        //!< block name (max 8 characters)
} symtab_entry_64;
//!> size of a symbol table entry in arena table
#define SymtabEntry64Size64 (sizeof(symtab_entry_64) / sizeof(uint64_t))

//!> memory arena information (64 bit mode)
typedef struct{              //!< MUST BE CONSISTENT WITH arena_header
  uint32_t lock;             //!< to lock this memory arena
  uint32_t owner;            //!< MPI rank or PID of owner process
  uint32_t max_entries;      //!< max number of entries in t[]
  uint32_t n_entries;        //!< number of entries in use in t[]
  uint64_t first_free;       //!< index of first free location in arena
  uint64_t arena_size;       //!< size of memory arena (data + metadata) (64 bit units)
  symtab_entry_64 t[];          //!< symbol table entries for memory blocks in this arena
} shmem_arena;
//!> size of memory arena header (no symbol table)
#define ArenaHeader64Size64 (sizeof(shmem_arena) / sizeof(uint64_t))

//!> memory block header (64 bit mode)
typedef struct{
  uint64_t fwd;              //!< forward index to next block (64 bit units) (0 for last block)
  uint64_t ix;               //!< index to this block (64 bit units)
  uint64_t nwd;              //!< length of data portion of block (in 64 bit units)
  uint64_t sign;             //!< low marker signature
}block_header_64;
//!> size of memory block header
#define BlockHeader64Size64 (sizeof(block_header_64) / sizeof(uint64_t))

//!> memory block trailer (64 bit mode)
typedef struct{
  uint64_t sign;             //!< high marker signature
  uint64_t bwd;              //!< backward index to start of this block (in 64 bit units)
}block_tail_64;
//!> size of memory block trailer
#define BlockTail64Size64 (sizeof(block_tail_64) / sizeof(uint64_t))

// ======= memory "fencing" macros (X86 family) =======

//!> memory store fence
#define W_FENCE asm volatile("": : :"memory"); _mm_sfence();

//!> memory load fence
#define R_FENCE asm volatile("": : :"memory"); _mm_lfence();

//!> memory load+store fence
#define M_FENCE asm volatile("": : :"memory"); _mm_mfence();

#endif
//! set owner's id (usually MPI rank) for memory arenas<br>
//! me = shmem_arena_set_id(id)
//! @return -1 upon error, value > 0 otherwise
int32_t shmem_arena_set_id(
  int32_t id                      //!< [in] owner's id (usually MPI rank) 
);
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! shmem_arena_print_status(mem)
void shmem_arena_print_status(
  void *mem                                  //!< [in] pointer to memory arena (see  shmem_arena_init)
);
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! id = shmem_arena_init(mem, nsym, size)
//! @return id of current owner process (not necessarily me)
uint32_t shmem_arena_init(
  void *mem,                   //!< [in] pointer to memory arena
  uint32_t nsym,               //!< [in] size of symbol table to allocate (max number of blocks expected)
  size_t size                  //!< [in] size of memory area in bytes
);
//! find memory block called 'name'<br>
//! ptr = shmem_block_find(mem, size, flags, name)
//! @return local address of memory block (NULL if not found)
void *shmem_block_find(
  void *mem,                      //!< [in]  pointer to memory arena
  size_t  *size,                 //!< [OUT] size of memory block in bytes (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! same as shmem_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
//! ptr = shmem_block_find_wait(mem, size, flags, name, timeout)
//! @return local address of memory block (NULL if not found)
void *shmem_block_find_wait(
  void *mem,                      //!< [in]  pointer to memory arena (see  shmem_arena_init)
  size_t *size,                   //!< [OUT] size of memory block in bytes (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name,            //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  int timeout                     //!< [in]  timeout in milliseconds, -1 means practically forever
);
//! mark memory block 'name' as initialized<br>
//! ptr = shmem_block_mark_init(mem, name)
//! @return block address if found, NULL otherwise
void *shmem_block_mark_init(
  void *mem,                       //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
  unsigned char *name              //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! find the max size allowed for next block in a managed 'memory arena'<br>
//! int64_t size64
//! size64 = shmem_block_max_size(mem)
//! @return maximum size for next block allocated in bytes
int64_t shmem_block_max_size(
  void *mem                        //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
);
//! create a named block in a managed 'memory arena'<br>
//! ptr = shmem_block_create(mem, size, name)
//! @return local address of created block (NULL if error)
void *shmem_block_create(
  void *mem,                      //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
  size_t size,                    //!< [in]  desired size of block in bytes
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! allocate a shared memory segment<br>
//! ptr = shmem_allocate_shared(shmid, size)
//! @return local address of memory block
void *shmem_allocate_shared(
  int *shmid,                 //!< [out] shared memory id of segment (set by shmem_allocate_shared) (see shmget)
  uint64_t size               //!< [in]  size of segment in bytes
);
//! create a memory arena in user memory<br>
//! ptr = shmem_arena_create_from_address(memaddr, nsym, size)
//! @return  address of memory arena (NULL if error)
void *shmem_arena_create_from_address(
  void *memaddr,               //!< [in]  user memory address
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  size_t size                  //!< [in]  size of segment in 32 bit units
);
//! create a memory arena in shared memory<br>
//! ptr = shmem_arena_create_shared(shmid, nsym, size)
//! @return  local address of memory arena
void *shmem_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  size_t   size                //!< [in]  size of segment in bytes
);
//! get memory address associated with shared memory segment id<br>
//! ptr = shmem_address_from_id(shmid)
//! @return local memory addres of shared memory segment
void *shmem_address_from_id(
  int shmid                  //!< [in] shared memory id of segment (see shmget)
);

#endif // IO_SERVER_shmem_arena_GEN_H
