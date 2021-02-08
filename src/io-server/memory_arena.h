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
// This file has been generated from memory_arena.c
#ifndef IO_SERVER_memory_arena_GEN_H
#define IO_SERVER_memory_arena_GEN_H

/**
 \file
 \brief memory arena management package (C and Fortran)

 code extracted from file memory_arena.c
 \verbatim
    set of routines to implement named block management in a memory pool
    possibly shared by multiple threads and processes
  
            master arena layout (there must be one and only one)
           (the master arena can contain multiple memory arenas)
           (the master table has one entry per memory arena)
    +--------------------+--------------------+---------------------+-------------------->
    | master table       | arena header       | symbol table        | data blocks
    +--------------------+--------------------+---------------------+-------------------->
  
            memory arena layout (multiple arenas can coexist)
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
    FWD, IX, NWD, SIGNL, SIGNH, BWD are 32 bit unsigned integers

    indices are used instead of addresses because processes sharing a memory segment are more
    than likely to have said memory segment mapped at different addresses

 \endverbatim
*/
#if ! defined(MAX_MASTER)
#include <stdint.h>
//!> \brief memory arena structures

//!> symbol table entry
typedef struct{
  uint32_t lock;             //!< to lock this memory block
  uint32_t flags;            //!< control flags
  uint32_t data_index;       //!< index relative to start of memory arena
  uint32_t data_size;        //!< size of data portion of block (64 bit units)
  uint64_t data_name;        //!< block name (max 8 characters)
} symtab_entry;
//!> size of a symbol table entry in arena table
#define SymtabEntrySize64 (sizeof(symtab_entry) / sizeof(uint64_t))

//!> memory arena information
typedef struct{              //!< MUST BE CONSISTENT WITH arena_header
  uint32_t lock;             //!< to lock this memory arena
  uint32_t owner;            //!< MPI rank or PID of owner process
  uint32_t max_entries;      //!< max number of entries in t[]
  uint32_t first_free;       //!< index of first free location in arena
  uint32_t n_entries;        //!< number of entries in use in t[]
  uint32_t arena_size;       //!< size of memory arena (data + metadata) (64 bit units)
  symtab_entry t[];          //!< symbol table entries for memory blocks in this arena
} memory_arena;
//!> size of memory arena header (no symbol table)
#define ArenaHeaderSize64 (sizeof(memory_arena) / sizeof(uint64_t))

//!> description of a memory arena
typedef struct{
  uint64_t arena_name;       //!< name of segment (max 8 characters, stops at null or space)
  size_t   arena_sz;         //!< size of segment
  int32_t  arena_id;         //!< shared memory id of shared memory segment
  int32_t  owner_id;         //!< id of process that owns the segment
}master_entry;               //!< there will be one entry per memory arena in master table

//!> max number of arenas in master arena
#define MAX_MASTER 256

#if 0
//!> header of master arena (only used to compute the master header size)
typedef struct{
  uint32_t lock;             //!< to lock master arena
  int32_t  arena_id;         //!< shared memory id of master arena
  uint64_t arena_name;       //!< name of master arena  (max 8 characters, stops at null or space)
  size_t   arena_sz;         //!< size of master arena segment
  master_entry me[MAX_MASTER];
} master_header;
//!> size of master arena header (old way)
#define OldMasterHeaderSize64 (sizeof(master_header) / sizeof(uint64_t))
#endif

//!> master arena contains the master table, followed by a memory for its own arena
typedef struct{
  uint32_t lock;             //!< to lock master arena
  int32_t  arena_id;         //!< shared memory id of master arena
  uint64_t arena_name;       //!< name of master arena (max 8 characters, stops at null or space)
  size_t   arena_sz;         //!< size of master arena segment
  master_entry me[MAX_MASTER];  //!< size of the master table
  memory_arena ma;           //!< master memory arena, will contain the other arenas
} master_arena;
//!> size of master arena header (new way)
#define MasterHeaderSize64 ( (sizeof(master_arena) - sizeof(memory_arena) ) / sizeof(uint64_t))

//!> arena description in local memory (one entry per memory arena)
typedef struct{
  uint64_t arena_name;       //!< same as in associated master arena table
  size_t   arena_sz;         //!< same as in associated master arena table
  memory_arena *ma;          //!< pointer to memory arena in local process space
}local_entry;

//!> copy in local process memory pointing to memory arenas
typedef struct{
  uint32_t     lock;            //!< should not be necessary
  int32_t      master_id;       //!< shared memory id of master arena
  size_t       master_sz;       //!< size of segment
  master_arena *MA;             //!< pointer to master arena
  local_entry  le[MAX_MASTER];  //!< table in local memory describing memory arenas in the master arena
}local_arena;

//!> memory block header
typedef struct{
  uint32_t fwd;              //!< forward index to next block (64 bit units) (0 for last block)
  uint32_t ix;               //!< index to this block (64 bit units)
  uint32_t nwd;              //!< length of data portion of block (in 64 bit units)
  uint32_t sign;             //!< low marker signature
}block_header;
//!> size of memory block header
#define BlockHeaderSize64 (sizeof(block_header) / sizeof(uint64_t))

//!> memory block trailer
typedef struct{
  uint32_t sign;             //!< high marker signature
  uint32_t bwd;              //!< backward index to start of this block (in 64 bit units)
}block_tail;
//!> size of memory block trailer
#define BlockTailSize64 (sizeof(block_tail) / sizeof(uint64_t))

// ======= memory "fencing" macros (X86 family) =======

//!> memory store fence
#define W_FENCE asm volatile("": : :"memory"); _mm_sfence();

//!> memory load fence
#define R_FENCE asm volatile("": : :"memory"); _mm_lfence();

//!> memory load+store fence
#define M_FENCE asm volatile("": : :"memory"); _mm_mfence();

#endif
//! set owner's id (usually MPI rank) for memory arenas<br>
//! me = memory_arena_set_id(id)
//! @return -1 upon error, value > 0 otherwise
int32_t memory_arena_set_id(
  int32_t id                      //!< [in] owner's id (usually MPI rank) 
);
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! memory_arena_print_status(mem)
//! @return none
void memory_arena_print_status(
  void *mem                                  //!< [in] pointer to memory arena (see  memory_arena_init)
);
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! id = memory_arena_init(mem, nsym, size)
//! @return id of current owner process (not necessarily me)
uint32_t memory_arena_init(
  void *mem,                   //!< [in] pointer to memory arena
  uint32_t nsym,               //!< [in] size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in] size of memory area in bytes
);
//! update local arena control table from master arena<br>
//! nareas = update_local_table(mem)
//! @return number of arenas detected
uint32_t update_local_table(
  void *mem                     //!< [in] pointer to master memory arena
);
//! initialize an already allocated 'master arena' (node shared memory usually)<br>
//! id = master_arena_init(mem, nsym, size)
//! @return id of current process
uint32_t master_arena_init(
  void *mem,                     //!< [in] pointer to master memory arena
  uint32_t nsym,                 //!< [in] size of symbol table to allocate (max number of blocks expected)
  uint64_t size                  //!< [in] size of memory area for master arena in bytes
);
//! find memory block called 'name'<br>
//! ptr = memory_block_find(mem, size, flags, name)
//! @return local address of memory block (NULL if not found)
void *memory_block_find(
  void *mem,                      //!< [in]  pointer to memory arena
  uint32_t *size,                 //!< [OUT] size of memory block in 32 bit units (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! same as memory_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
//! ptr = memory_block_find_wait(mem, size, flags, name, timeout)
//! @return local address of memory block (NULL if not found)
void *memory_block_find_wait(
  void *mem,                      //!< [in]  pointer to memory arena (see  memory_arena_init)
  uint32_t *size,                 //!< [OUT] size of memory block in 32 bit units (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name,            //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  int timeout                     //!< [in]  timeout in milliseconds, -1 means practically forever
);
//! mark memory block 'name' as initialized<br>
//! ptr = memory_block_mark_init(mem, name)
//! @return block address if found, NULL otherwise
void *memory_block_mark_init(
  void *mem,                       //!< [in]  pointer to the managed 'memory arena' (see  memory_arena_init)
  unsigned char *name              //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! create a named block in a managed 'memory arena'<br>
//! ptr = memory_block_create(mem, size, name)
//! @return local address of created block (NULL if error)
void *memory_block_create(
  void *mem,                        //!< [in]  pointer to the managed 'memory arena' (see  memory_arena_init)
  uint32_t size,                    //!< [in]  desired size of block in 32 bit units
  unsigned char *name               //!< [in]  name of block to find (characters beyond the 8th will be ignored)
);
//! allocate a shared memory segment<br>
//! ptr = memory_allocate_shared(shmid, size)
//! @return local address of memory block
void *memory_allocate_shared(
  int *shmid,                 //!< [out] shared memory id of segment (set by memory_allocate_shared) (see shmget)
  uint64_t size               //!< [in]  size of segment in 32 bit units
);
//! create a memory arena in user memory<br>
//! ptr = memory_arena_create_from_address(memaddr, nsym, size)
//! @return  address of memory arena (NULL if error)
void *memory_arena_create_from_address(
  void *memaddr,               //!< [in]  user memory address
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in 32 bit units
);
//! create a memory arena in shared memory<br>
//! ptr = memory_arena_create_shared(shmid, nsym, size)
//! @return  local address of memory arena
void *memory_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in bytes
);
//! create master memory arena in shared memory<br>
//! ptr = master_arena_create_shared(shmid, nsym, size)
//! @return local address of master memory arena
void *master_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in bytes
);
//! get memory address associated with shared memory segment id<br>
//! ptr = memory_address_from_id(shmid)
//! @return local memory addres of shared memory segment
void *memory_address_from_id(
  int shmid                  //!< [in] shared memory id of segment (see shmget)
);
//! get memory arena address of master arena address<br>
//! ptr = memory_arena_from_master(mem)
//! @return local memory addres of memory arena of master arena
void *memory_arena_from_master(
  void *mem                        //!< [in]  pointer to the 'master memory arena'
);
//! get memory address associated with shared memory segment id of master arena<br>
//! ptr =  memory_arena_from_master_id(shmid)
//! @return local memory addres of memory arena of master arena
void *memory_arena_from_master_id(
  int shmid                    //!< [in]  master arena segment id (from master_arena_create_shared)
);

#endif // IO_SERVER_memory_arena_GEN_H
