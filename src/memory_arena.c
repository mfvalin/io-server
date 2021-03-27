/*
 * Copyright (C) 2021  Environnement Canada
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

// tell doxygen to ignore this file
// as the following condition should always be false
//! \cond DOXYGEN_SHOULD_NOT_SKIP_THIS
//C_StArT
/**
//C_EnD
//F_StArT //C_StArT
// !> \file
// !> \brief memory arena management package (C and Fortran)
// !>
// !> code extracted from file memory_arena.c
// !> \verbatim
// !>    set of routines to implement named block management in a memory pool
// !>    possibly shared by multiple threads and processes
// !>  
// !>            master arena layout (there must be one and only one)
// !>           (the master arena can contain multiple memory arenas)
// !>           (the master table has one entry per memory arena)
// !>    +--------------------+--------------------+---------------------+-------------------->
// !>    | master table       | arena header       | symbol table        | data blocks
// !>    +--------------------+--------------------+---------------------+-------------------->
// !>  
// !>            memory arena layout (multiple arenas can coexist)
// !>    +--------------------+---------------------+-------------------->
// !>    | arena header       | symbol table        | data blocks
// !>    +--------------------+---------------------+-------------------->
// !>  
// !>    indices are used instead of addresses because the memory arena might be mapped 
// !>    at different addresses in different processes
// !>  
// !>            data block layout
// !>        +----------------------------------------------------------------- +
// !>        |                                                                  |
// !>        |                                                                  v
// !>    +-------+-------+-------+-------+.....................+-------+-------+
// !>    |  FWD  |  IX   |  NWD  | SIGNL |  user data portion  | SIGNH |  BWD  |
// !>    +-------+-------+-------+-------+.....................+-------+-------+
// !>    ^                                                                 |
// !>    |                                                                 |
// !>    +-----------------------------------------------------------------+
// !>    FWD   : index of start of next block
// !>    IX    : index in symbol table of this block
// !>    NWD   : size of data portion in 64 bit units
// !>    SIGNL : low marker (used for checking data underruns)
// !>    SIGNH : high marker (used for checking data overruns)
// !>    BWD   : index of start of this block
// !>    FWD of last allocated block will point to a non existent block with FWD = 0
// !>    
// !>    FWD and BWD are indices into a 64 bit unsigned integer array starting at the beginning of the memory arena
// !>    FWD, IX, NWD, SIGNL, SIGNH, BWD are 32 bit unsigned integers
// !>
// !>    indices are used instead of addresses because processes sharing a memory segment are more
// !>    than likely to have said memory segment mapped at different addresses
// !>
// !> \endverbatim
//F_EnD //C_EnD
//C_StArT
*/
//C_EnD
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
// #include <immintrin.h>

#include "io-server/memory_arena.h"

// memory arena definitions follow, they should already have been included from memory_arena.h

//C_StArT
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
//C_EnD

// memory arena management code

// ===============  start of global data  ===============

static uint32_t me = 999999999;  // identifier for this process (usually MPI rank) (alternative : getpid() )

static local_arena LA;           // local information about master arena

// ===============  end of global data  ===============

//F_StArT
//  interface
//
//F_EnD

//F_StArT
// !> set owner's id (usually MPI rank) for memory arenas<br>
// !> me = memory_arena_set_id(id)
// function memory_arena_set_id(id) result(me) BIND(C,name='memory_arena_set_id')
//   import :: C_INT
//   integer(C_INT), intent(IN), value :: id                   !< owner's id (usually MPI rank) 
//   integer(C_INT) :: me                                      !< -1 upon error, value > 0 otherwise
// end function memory_arena_set_id
//
// !> set id for memory management arena, return identifier (-1 in case of error)
// !> id must be a POSITIVE INTEGER
//F_EnD
//C_StArT
//! set owner's id (usually MPI rank) for memory arenas<br>
//! me = memory_arena_set_id(id)
//! @return -1 upon error, value > 0 otherwise
int32_t memory_arena_set_id(
  int32_t id                      //!< [in] owner's id (usually MPI rank) 
  )
//C_EnD
{
  if(id < 0) return -1;
  me = id + 1;
  return me;
}

// translate char string (max 8 characters) into a 64 bit unsigned token
// translation will stop at first null or space character
// character string is CASE SENSITIVE (ASCII 128)
static inline uint64_t block_name(unsigned char *name //!< [in] string to be translated
                                  ){
  int i;
  uint64_t name64 = 0;

  for(i = 0 ; i < 8 ; i++){      // build 64 bit name token
    if(name[i] == '\0' || name[i] == ' ') break;
    name64 = (name64 << 8) | (name[i] & 0x7F);
  }
  return name64;
}

//F_StArT
// !> dump arena header and symbol table (description of contents of memory arena)<br>
// !> call memory_arena_print_status(mem)
// subroutine memory_arena_print_status(mem) BIND(C,name='memory_arena_print_status')
//   import :: C_PTR
//   type(C_PTR), intent(IN), value :: mem      !< pointer to memory arena (see  memory_arena_init)
// end subroutine memory_arena_print_status
//
//F_EnD
//C_StArT
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! memory_arena_print_status(mem)
//! @return none
void memory_arena_print_status(
  void *mem                                  //!< [in] pointer to memory arena (see  memory_arena_init)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  memory_arena *ma = (memory_arena *) mem;
  symtab_entry *sym = ma->t;
  int i, j, sane;
  char name[9];
  uint64_t dname, size64;
  uint64_t *dataptr64;
  block_header *bh, *bhnext;
  block_tail *bt;

  fprintf(stdout,"\n==============================================\n");
  fprintf(stdout,"Arena Header, id = %d, address = %p\n", me, ma);
  fprintf(stdout,"owner       = %8.8x\n",ma->owner);
  fprintf(stdout,"max entries = %d\n",ma->max_entries);
  fprintf(stdout,"max size    = %d\n",ma->arena_size);
  fprintf(stdout,"entries     = %d\n",ma->n_entries);
  fprintf(stdout,"first free  = %d\n",ma->first_free);

  fprintf(stdout,"\nSymbol table\n");
  for(i = 0 ; i < ma->n_entries ; i++){
    size64 = sym[i].data_size;
    dataptr64 = sym[i].data_index + mem64; 
    bh = (block_header *) (dataptr64);
    bt = (block_tail   *) (dataptr64 + BlockHeaderSize64 + size64);
    bhnext = (block_header *) (dataptr64 + BlockHeaderSize64 + size64 + BlockTailSize64);
    dname = sym[i].data_name;
    sane = ( bh->sign == 0xBEEFF00D ) & 
           ( bt->sign == 0xDEADBEEF ) & 
           (i == bh->ix) & 
           (bt->bwd == sym[i].data_index) &
           (bh->nwd == sym[i].data_size) &
           (bh->fwd == sym[i].data_index + sym[i].data_size + BlockHeaderSize64 + BlockTailSize64) ;
    for(j = 0; j < 9 ; j++) {
      name[j] = '\0';
      if( 0 == (dname >> 56) ) dname <<= 8;
    }
    for(j = 0; j < 8 ; j++) {
      name[j] = dname >> 56;
      dname <<= 8;
    }
    fprintf(stdout,"%4d: %4d F=%8.8x I=%8d S=%8d (%8d) %s %s %s FW=%8d FWNXT=%8d BW=%8d '%s'\n",
            i,bh->ix,sym[i].flags,sym[i].data_index,sym[i].data_size,bh->nwd,
            sane ? "T" : "F", ( bh->sign == 0xBEEFF00D ) ? "t" : "f", ( bt->sign == 0xDEADBEEF ) ? "t" : "f",
            bh->fwd, bhnext->fwd, bt->bwd, name);
  }
  fprintf(stdout,"==============================================\n");
  fflush(stdout);
}

//F_StArT
// !> initialize an already allocated 'memory arena' (usually node shared memory)<br>
// !> id = memory_arena_init(mem, nsym, size)
// function memory_arena_init(mem, nsym, size) result(id) BIND(C,name='memory_arena_init')
//   import :: C_PTR, C_INT, C_INT64_T
//   type(C_PTR), intent(IN), value :: mem                 !< pointer to memory arena
//   integer(C_INT), intent(IN), value :: nsym             !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size         !< size of memory area in bytes (max 32GBytes)
//   integer(C_INT) :: id                                  !< id of current owner process (not necessarily me)
// end function memory_arena_init
//
//F_EnD
//C_StArT
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! id = memory_arena_init(mem, nsym, size)
//! @return id of current owner process (not necessarily me)
uint32_t memory_arena_init(
  void *mem,                   //!< [in] pointer to memory arena
  uint32_t nsym,               //!< [in] size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in] size of memory area in bytes
  )
//C_EnD
{
  memory_arena *ma = (memory_arena *) mem;
  symtab_entry *sym = ma->t;
  uint64_t size64 = size >> 3 ;  // round size down to 64 bit element size
  uint32_t size32 = (size64 > 0xFFFFFFFFL) ? 0xFFFFFFFFU : size64 ;  // must fit in 32 bits
  int i;
  if(ma->owner != 0) {
    fprintf(stderr,"ma init %p, already owned by id = %d\n", ma, ma->owner);
    return ma->owner;                           // area already initialized, return id of initializer
  }else{
    fprintf(stderr,"ma init %p, not owned, id = %d\n", ma, ma->owner);
  }

  while(__sync_val_compare_and_swap(&(ma->lock), 0, me) != 0); // lock memory arena
// fprintf(stderr,"DEBUG: memory arena %p locked by %d\n", ma, me);

  ma->owner = 0;           // initialize arena header
  ma->max_entries = nsym;
  ma->first_free = ArenaHeaderSize64 + nsym * SymtabEntrySize64;
// fprintf(stderr,"ArenaHeaderSize64 = %d, SymtabEntrySize64 = %d, nsym = %d, base = %d\n",ArenaHeaderSize64,SymtabEntrySize64,nsym,ma->first_free);
  ma->n_entries = 0;
  ma->arena_size = size32;

  for(i = 0 ; i < nsym ; i++){   // initialize symbol table to null values
    sym[i].lock       = 0;
    sym[i].flags      = 0;
    sym[i].data_index = 0;
    sym[i].data_size  = 0;
    sym[i].data_name  = 0;
  }

  ma->owner = me;  // flag area as initialized by me

  i = __sync_val_compare_and_swap(&(ma->lock), me, 0); // unlock memory arena and return my id
fprintf(stderr,"DEBUG: memory arena %p UNlocked and owned by %d\n", ma, me);
  return i ;
}

//F_StArT
// !> update local arena control table from master arena<br>
// !> nareas = update_local_table(mem)
// function update_local_table(mem) result(nareas) BIND(C,name='update_local_table')
//   import :: C_PTR, C_INT
//   type(C_PTR), intent(IN), value :: mem            !< pointer to master memory arena (see  memory_arena_init)
//   integer(C_INT) :: nareas                         !< number of arenas detected
// end function update_local_table
//
//F_EnD
//C_StArT
//! update local arena control table from master arena<br>
//! nareas = update_local_table(mem)
//! @return number of arenas detected
uint32_t update_local_table(
  void *mem                     //!< [in] pointer to master memory arena
  )
//C_EnD
{
  master_arena *MA = (master_arena *) mem;
  memory_arena *ma = (memory_arena *) &(MA->ma);
  int i;

  while(__sync_val_compare_and_swap(&(MA->lock), 0, me) != 0); // lock master arena to get a consistent image

  LA.lock      = 0;
  LA.master_id = me;
  LA.master_sz = MA->arena_sz;
  LA.MA        = MA;

  LA.le[0].arena_sz    = ma->arena_size;                 // memory arena associated to master arena
  LA.le[0].arena_name  = block_name((unsigned char *)"MaStEr");
  LA.le[0].ma          = ma;
#if defined(DEBUG)
  fprintf(stderr,"local update, arena = %d, id = %d, address = %p, size = %ld\n",
          0, MA->me[0].arena_id, LA.le[0].ma, LA.le[0].arena_sz);
#endif

  for(i=1 ; i<MAX_MASTER ; i++){   // zero rest of local table
    if(LA.le[i].arena_sz == 0){                     // not initialized yet
      if(MA->me[i].arena_sz > 0) {                  // there is a shared memory segment
        LA.le[i].ma  = shmat(MA->me[i].arena_id, NULL, 0);    // attach segment, get address
        LA.le[i].arena_sz    = MA->me[i].arena_sz;
        LA.le[i].arena_name  = MA->me[i].arena_name;
      }else{                                        // no more segment, break
        break;
      }
    }
#if defined(DEBUG)
  fprintf(stderr,"local update, arena = %d, id = %d, address = %p, size = %ld\n",
          i, MA->me[i].arena_id, LA.le[i].ma, LA.le[i].arena_sz);
#endif
  }

  __sync_val_compare_and_swap(&(MA->lock), me, 0); // unlock master arena

  return i;
}

//F_StArT
// !> initialize an already allocated 'master arena' (node shared memory usually)<br>
// !> id = master_arena_init(mem, nsym, size)
// function master_arena_init(mem, nsym, size) result(id) BIND(C,name='master_arena_init')
//   import :: C_PTR, C_INT, C_INT64_T
//   type(C_PTR), intent(IN), value :: mem            !< pointer to master memory arena (see  memory_arena_init)
//   integer(C_INT), intent(IN), value :: nsym        !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size    !< size of memory area for master arena in bytes
//   integer(C_INT) :: id                             !< id of current process
// end function master_arena_init
//
//F_EnD
//C_StArT
//! initialize an already allocated 'master arena' (node shared memory usually)<br>
//! id = master_arena_init(mem, nsym, size)
//! @return id of current process
uint32_t master_arena_init(
  void *mem,                     //!< [in] pointer to master memory arena
  uint32_t nsym,                 //!< [in] size of symbol table to allocate (max number of blocks expected)
  uint64_t size                  //!< [in] size of memory area for master arena in bytes
  )
//C_EnD
{
  master_arena *MA = (master_arena *) mem;
  memory_arena *ma = (memory_arena *) &(MA->ma);
  int i, status;

  size = size - MasterHeaderSize64 * 2;     // space left for memory arena proper

  while(__sync_val_compare_and_swap(&(MA->lock), 0, me) != 0); // lock master arena while modifying it

  for(i=0 ; i<MAX_MASTER ; i++){   // nullify all entries in master table
    MA->me[i].arena_name =  0;
    MA->me[i].arena_sz   =  0;
    MA->me[i].arena_id   = -1;
    MA->me[i].owner_id   = -1;
  }
  MA->me[0].arena_name = block_name((unsigned char *)"MaStEr");  // special name for master arena
  MA->me[0].arena_sz   = size >> 3;             // fix size entry of area 0 (arena part of master arena) (64 bit units)
  MA->me[0].owner_id   = me;                    // creator id
// printf("MA = %p, ma = %p, delta = %ld\n",MA, ma, (void *)ma - (void *)MA);

  status = memory_arena_init(ma, nsym, size);   // initialize memory arena part of master arena

  __sync_val_compare_and_swap(&(MA->lock), me, 0); // unlock master arena

  return status;
}

// find entry in symbol table, return index if found, -1 otherwise
static inline int32_t find_block(memory_arena *ma, symtab_entry *sym, uint64_t name64){
  uint32_t i;

  for(i = 0 ; i < ma->n_entries ; i++){
    if(sym[i].data_name == name64){
      return i;
    }
  }
  return -1 ; // miserable failure
}

//F_StArT
// !> find memory block called 'name'<br>
// !> ptr = memory_block_find(mem, size, flags, name)
// function memory_block_find(mem, size, flags, name) result(ptr) BIND(C,name='memory_block_find')
//   import :: C_PTR, C_INT, C_CHAR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  memory_arena_init)
//   integer(C_INT), intent(OUT) :: size                      !< size of memory block in 32 bit units (0 if not found)
//   integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
// end function memory_block_find
//
//F_EnD
//C_StArT
//! find memory block called 'name'<br>
//! ptr = memory_block_find(mem, size, flags, name)
//! @return local address of memory block (NULL if not found)
void *memory_block_find(
  void *mem,                      //!< [in]  pointer to memory arena
  uint32_t *size,                 //!< [OUT] size of memory block in 32 bit units (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  memory_arena *ma = (memory_arena *) mem;
  symtab_entry *sym = ma->t;
  void *dataptr = NULL;
  uint64_t name64 = block_name(name);
  int32_t i;

  *size = 0;         // precondition to fail
  *flags = 0;
  if(ma == NULL || name == NULL) return NULL;
  name64 = block_name(name);
  if(name64 == 0) return NULL;

  i = find_block(ma, sym, name64);
  if(i < 0) return NULL;  // name not found in symbol table

  *size = sym[i].data_size * 2;            // return size in 32 bit units
  *flags = sym[i].flags;
  dataptr = &mem64[sym[i].data_index + BlockHeaderSize64];     // pointer to actual data
  return dataptr;
}

//F_StArT
// !> same as memory_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
// !> ptr = memory_block_find_wait(mem, size, flags, name, timeout)
// function memory_block_find_wait(mem, size, flags, name, timeout) result(ptr) BIND(C,name='memory_block_find_wait')
//   import :: C_PTR, C_INT, C_CHAR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  memory_arena_init)
//   integer(C_INT), intent(OUT) :: size                      !< size of memory block in 32 bit units (0 if not found)
//   integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   integer(C_INT), intent(IN), value :: timeout             !< timeout in milliseconds, -1 means practically forever
//   type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
// end function memory_block_find_wait
//
//F_EnD
//C_StArT
//! same as memory_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
//! ptr = memory_block_find_wait(mem, size, flags, name, timeout)
//! @return local address of memory block (NULL if not found)
void *memory_block_find_wait(
  void *mem,                      //!< [in]  pointer to memory arena (see  memory_arena_init)
  uint32_t *size,                 //!< [OUT] size of memory block in 32 bit units (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name,            //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  int timeout                     //!< [in]  timeout in milliseconds, -1 means practically forever
  )
//C_EnD
{
  void *p = NULL;
  useconds_t delay = 1000;  // 1000 microseconds = 1 millisecond

  p = memory_block_find(mem, size, flags, name);     // does the block exist ?
  while(p == NULL && timeout > 0) {                  // no, sleep a bit and retry
    usleep(delay); timeout --;                       // decrement timeout
    p = memory_block_find(mem, size, flags, name);   // does the block exist ?
  }
// fprintf(stderr,"timeout = %d\n",timeout);
  return p;
}

//F_StArT
// !> mark memory block 'name' as initialized<br>
// !> ptr = memory_block_mark_init(mem, name)
// function memory_block_mark_init(mem, name) result(ptr) BIND(C,name='memory_block_mark_init')
//   import :: C_PTR, C_CHAR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  memory_arena_init)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< block address if found, NULL otherwise
// end function memory_block_mark_init
//
//F_EnD
//C_StArT
//! mark memory block 'name' as initialized<br>
//! ptr = memory_block_mark_init(mem, name)
//! @return block address if found, NULL otherwise
void *memory_block_mark_init(
  void *mem,                       //!< [in]  pointer to the managed 'memory arena' (see  memory_arena_init)
  unsigned char *name              //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  memory_arena *ma = (memory_arena *) mem;
  symtab_entry *sym = ma->t;
  uint64_t name64 = block_name(name);
  int32_t i;
  void *dataptr = NULL;

  i = find_block(ma, sym, name64);
  if(i < 0) return NULL;  // name not found in symbol table

  while(__sync_val_compare_and_swap(&(sym[i].lock), 0, me) != 0); // lock block
  if(sym[i].flags == 0) sym[i].flags = me;                        // mark as initialized by me
  __sync_val_compare_and_swap(&(sym[i].lock), me, 0);         // unlock block

  dataptr = &mem64[sym[i].data_index + BlockHeaderSize64];        // pointer to actual data
  return dataptr;
}

//F_StArT
// !> create a named block in a managed 'memory arena'<br>
// !> ptr = memory_block_create(mem, size, name)
// function memory_block_create(mem, size, name) result(ptr) BIND(C,name='memory_block_create')
//   import :: C_PTR, C_INT, C_CHAR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  memory_arena_init)
//   integer(C_INT), intent(IN), value :: size                !< desired size of block in 32 bit units
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to create (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< local address of created block (NULL if error)
// end function memory_block_create
//
//F_EnD
//C_StArT
//! create a named block in a managed 'memory arena'<br>
//! ptr = memory_block_create(mem, size, name)
//! @return local address of created block (NULL if error)
void *memory_block_create(
  void *mem,                        //!< [in]  pointer to the managed 'memory arena' (see  memory_arena_init)
  uint32_t size,                    //!< [in]  desired size of block in 32 bit units
  unsigned char *name               //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  memory_arena *ma = (memory_arena *) mem;
  symtab_entry *sym = ma->t;
  uint32_t i, next;
  uint32_t fail;
  uint32_t size64 = (size + 1) >> 1;  // round size up to 64 bit element size
  uint32_t block64 = size64 + BlockHeaderSize64 + BlockTailSize64;
  block_header *bh;
  block_tail *bt;
  char *dataptr;
  uint64_t name64 = block_name(name);

  while(__sync_val_compare_and_swap(&(ma->lock), 0, me) != 0); // lock memory area
// fprintf(stderr,"memory_block_create LOCK by %d\n",me);
  fail  = ma->first_free + block64 + 1 > ma->arena_size;    // block larger than what we have left
  fail |= ma->n_entries == ma->max_entries;                 // symbol table is full

  if(fail){
    dataptr = NULL;
  }else{
    i = ma->n_entries;

    sym[i].lock  = 0;                     // keep lock as unlocked
    sym[i].flags = 0;                     // keep flag as uninitialized
    sym[i].data_index = ma->first_free;   // start of block
    sym[i].data_size = size64;            // data size for block
    sym[i].data_name = name64;            // data block name

    next = ma->first_free + block64;
    mem64[next] = 0;                      // fwd for next block will be 0

    bh = (block_header *) (mem64 + ma->first_free);    // start of block
    dataptr = (char *) (mem64 + ma->first_free + BlockHeaderSize64) ; // start of data in block
    bh->fwd = next;                       // next block will start there
    bh->ix = i;                           // index of this block in symbol table
    bh->nwd = size64;                     // size of data portion
    bh->sign = 0xBEEFF00D;                // marker below data

    bt = (block_tail *) (mem64 + ma->first_free + BlockHeaderSize64 + size64);
    bt->sign = 0xDEADBEEF;                // marker above data
    bt->bwd = sym[i].data_index;          // back pointer, index of start of current block

    ma->first_free = next;                // bump index of next free position
    ma->n_entries++;                      // bump number of valid entries
  }

  i = __sync_val_compare_and_swap(&(ma->lock), me, 0);         // unlock memory area
// fprintf(stderr,"memory_block_create UNLOCK by %d\n",me);
  return dataptr;
}

//F_StArT
// !> allocate a shared memory segment<br>
// !> ptr = memory_allocate_shared(shmid, size)
// function memory_allocate_shared(shmid, size) result(ptr) BIND(C,name='memory_allocate_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (set by memory_allocate_shared) (see shmget)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of segment in bytes
//   type(C_PTR) :: ptr                             !< local address of memory segment
// end function memory_allocate_shared
//
//F_EnD
//C_StArT
//! allocate a shared memory segment<br>
//! ptr = memory_allocate_shared(shmid, size)
//! @return local address of memory block
void *memory_allocate_shared(
  int *shmid,                 //!< [out] shared memory id of segment (set by memory_allocate_shared) (see shmget)
  uint64_t size               //!< [in]  size of segment in 32 bit units
  )
//C_EnD
{
  int id = -1;
  void *shmaddr = NULL;
  size_t shmsz = size ;                     // size in bytes of memory segment
  int err;
  struct shmid_ds dummy;

  if(me == 999999999) me = getpid();        // if not initialized, set to pid

  id = shmget(IPC_PRIVATE, shmsz, 0600);    // get a memory block, only accessible by user
  *shmid = id;                              // shared memory block id returned to caller
  if(id == -1) return NULL;                 // miserable failure

  shmaddr = shmat(id, NULL, 0);             // get local address of shared memory block
  if(shmaddr == NULL) return NULL;          // miserable failure

  err = shmctl(id, IPC_RMID, &dummy);       // mark block "to be deleted when no process attached"
  if(err == -1) {                           // miserable failure
    err = shmdt(shmaddr);
    return NULL;
  }

  return shmaddr;     // return local address of memory block
}

//F_StArT
// !> create a memory arena in user memory<br>
// !> ptr = memory_arena_create_from_address(memaddr, nsym, size)
// function memory_arena_create_from_address(memaddr, nsym, size) result(ptr) BIND(C,name='memory_arena_create_from_address')
//   import :: C_PTR, C_INT, C_INT64_T
//   type(C_PTR), intent(IN), value :: memaddr      !< user memory address
//   integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
//   type(C_PTR) :: ptr                             !< address of memory arena (NULL if error)
// end function memory_arena_create_from_address
//
//F_EnD
//C_StArT
//! create a memory arena in user memory<br>
//! ptr = memory_arena_create_from_address(memaddr, nsym, size)
//! @return  address of memory arena (NULL if error)
void *memory_arena_create_from_address(
  void *memaddr,               //!< [in]  user memory address
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in 32 bit units
  )
//C_EnD
{
  int err;

  if(memaddr == NULL) return memaddr;             // invalid address

  err = memory_arena_init(memaddr, nsym, size);   // initialize memory arena
  if(err < 0) return NULL;

  return memaddr;
}

//F_StArT
// !> create a memory arena in shared memory<br>
// !> ptr = memory_arena_create_shared(shmid, nsym, size)
// function memory_arena_create_shared(shmid, nsym, size) result(ptr) BIND(C,name='memory_arena_create_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (see shmget)
//   integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
//   type(C_PTR) :: ptr                             !< local address of memory arena
// end function memory_arena_create_shared
//
//F_EnD
//C_StArT
//! create a memory arena in shared memory<br>
//! ptr = memory_arena_create_shared(shmid, nsym, size)
//! @return  local address of memory arena
void *memory_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in bytes
  )
//C_EnD
{
  void *shmaddr = memory_allocate_shared(shmid, size);    // request shared memory block
  int err;

  if(shmaddr == NULL) return shmaddr;             // request failed

  err = memory_arena_init(shmaddr, nsym, size);   // initialize memory arena
  if(err < 0) return NULL;

  return shmaddr;
}

//F_StArT
// !> create master memory arena in shared memory<br>
// !> ptr = master_arena_create_shared(shmid, nsym, size)
// function master_arena_create_shared(shmid, nsym, size) result(ptr) BIND(C,name='master_arena_create_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid                  !< shared memory id of segment (see shmget)
//   integer(C_INT), intent(IN), value :: nsym             !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
//   type(C_PTR) :: ptr                                    !< local address of master memory arena
// end function master_arena_create_shared
//
//F_EnD
//C_StArT
//! create master memory arena in shared memory<br>
//! ptr = master_arena_create_shared(shmid, nsym, size)
//! @return local address of master memory arena
void *master_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in bytes
  )
//C_EnD
{
  void *shmaddr = memory_allocate_shared(shmid, size);    // request shared memory block
  int err;
  master_arena *MA;

  MA = (master_arena *) shmaddr;
  MA->lock       = 0;
  MA->arena_id   = *shmid;
  MA->arena_sz   = size >> 1;             // 64 bit units
  MA->arena_name = block_name((unsigned char *)"MaStEr");  // special name
  
printf("MA = %p, id = %d\n",MA, MA->arena_id);
  err = master_arena_init(shmaddr, nsym, size);
  if(err < 0) return NULL;
  MA->me[0].arena_id   = MA->arena_id;                    // segment id

  return MA;                       // return address of master arena
}

//F_StArT
// !> get memory address associated with shared memory segment id<br>
// !> ptr = memory_address_from_id(shmid)
// function memory_address_from_id(shmid) result(ptr) BIND(C,name='memory_address_from_id')
//   import :: C_PTR, C_INT
//   integer(C_INT), intent(IN), value :: shmid           !< shared memory id of segment (see shmget)
//   type(C_PTR) :: ptr                             !< local memory addres of shared memory segment
// end function memory_address_from_id
//
//F_EnD
//C_StArT
//! get memory address associated with shared memory segment id<br>
//! ptr = memory_address_from_id(shmid)
//! @return local memory addres of shared memory segment
void *memory_address_from_id(
  int shmid                  //!< [in] shared memory id of segment (see shmget)
  )
//C_EnD
{
  void *p = shmat(shmid, NULL, 0) ;
//   printf("shmid = %d, address = %p\n",shmid, p);
  return p;
}

//F_StArT
// !> get memory arena address of master arena address<br>
// !> ptr = memory_arena_from_master(mem)
// function memory_arena_from_master(mem) result(ptr) BIND(C,name='memory_arena_from_master')
//   import :: C_PTR
//   type(C_PTR), intent(IN), value :: mem         !< pointer to the 'master memory arena'
//   type(C_PTR) :: ptr                            !< local memory addres of memory arena of master arena
// end function memory_arena_from_master
//
//F_EnD
//C_StArT
//! get memory arena address of master arena address<br>
//! ptr = memory_arena_from_master(mem)
//! @return local memory addres of memory arena of master arena
void *memory_arena_from_master(
  void *mem                        //!< [in]  pointer to the 'master memory arena'
  )
//C_EnD
{
  master_arena *MA = (master_arena *) mem;
  return &(MA->ma);
}

//F_StArT
// !> get memory address associated with shared memory segment id of master arena<br>
// !> 
// function memory_arena_from_master_id(shmid) result(ptr) BIND(C,name='memory_arena_from_master_id')
//   import :: C_PTR, C_INT
//   integer(C_INT), intent(IN), value :: shmid    !< master arena segment id (from master_arena_create_shared)
//   type(C_PTR) :: ptr                            !< local memory addres of memory arena of master arena
// end function memory_arena_from_master_id
//
//F_EnD
//C_StArT
//! get memory address associated with shared memory segment id of master arena<br>
//! ptr =  memory_arena_from_master_id(shmid)
//! @return local memory addres of memory arena of master arena
void *memory_arena_from_master_id(
  int shmid                    //!< [in]  master arena segment id (from master_arena_create_shared)
  )
//C_EnD
{
  void *shmaddr = shmat(shmid, NULL, 0);
  master_arena *MA;

  if(shmaddr == NULL) return NULL;

  MA = (master_arena *) shmaddr;
  return &(MA->ma);
}

//F_StArT
//  end interface
//F_EnD

#if defined(SELF_TEST)
#include <errno.h>

#include <mpi.h>

#define NSYM 128
#define DBLK 20

int main(int argc, char **argv){
  int err, rank, size, id, disp_unit, id2;
  int shmid = -1;
  void *shmaddr = NULL;
  void *p = NULL;
  int shmsz = 1024 * 1024 * 4;  // 4 MBytes
  uint64_t shmsz64 = shmsz;
  struct shmid_ds dummy;
  MPI_Win win ;
  MPI_Aint winsize ;

  err = MPI_Init(&argc, &argv);
  err = MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  err = MPI_Comm_size(MPI_COMM_WORLD,&size);
  id = memory_arena_set_id(rank);

#if defined(MPI_SHARED)
  winsize = (rank == 0) ? shmsz : 0 ;     // alloctate size not zero only on PE 0
  err = MPI_Win_allocate_shared (winsize, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &shmaddr, &win) ;
  err = MPI_Win_shared_query (win, 0, &winsize, &disp_unit, &shmaddr) ;  // get address of segment for rank 0
#endif

  if(rank == 0) {
//     shmid = shmget(IPC_PRIVATE, shmsz, 0600);
//     shmaddr = shmat(shmid, NULL, 0);
//     err = shmctl(shmid, IPC_RMID, &dummy);
//     system("ipcs -m");
//     if(shmaddr == (void *) -1) {
//       perror("shmat");
//       fprintf(stderr,"error attaching segment %d, %p, errno = %d\n",shmid,shmaddr,errno);
//       exit(1);
//     }
#if defined(MPI_SHARED)
    fprintf(stderr,"creating memory arena from address\n");
    shmaddr = memory_arena_create_from_address(shmaddr, NSYM, shmsz64);
#else
    fprintf(stderr,"creating memory arena from shared memory id\n");
    shmaddr = memory_arena_create_shared(&shmid, NSYM, shmsz64);
#endif
    id2 = memory_arena_init(shmaddr, NSYM, shmsz64);
    fprintf(stderr,"id2 = %d, rank = %d\n", id2, rank);
    memory_arena_print_status(shmaddr);
    p = memory_block_create(shmaddr, DBLK*1, "BLOCK000"); p = memory_block_mark_init(shmaddr, "BLOCK000");
    p = memory_block_create(shmaddr, DBLK*2, "BLOCK001"); p = memory_block_mark_init(shmaddr, "BLOCK001");
    p = memory_block_create(shmaddr, DBLK*3, "BLOCK002"); p = memory_block_mark_init(shmaddr, "BLOCK002");
    p = memory_block_create(shmaddr, DBLK*4, "BLOCK003"); p = memory_block_mark_init(shmaddr, "BLOCK003");
    p = memory_block_create(shmaddr, DBLK*5, "BLOCK004"); p = memory_block_mark_init(shmaddr, "BLOCK004");
  }
#if ! defined(MPI_SHARED)
  err = MPI_Bcast(&shmid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD);
  if(rank != 0) shmaddr = shmat(shmid, NULL, 0);
#endif
  fprintf(stderr,"I am process %d of %d, id = %d, shmid = %d, addr = %p\n",rank+1,size,id,shmid,shmaddr);
  err = MPI_Barrier(MPI_COMM_WORLD);
  if(rank == 1) {
//     system("ipcs -m");
    memory_arena_print_status(shmaddr);
  }
  err = MPI_Finalize();
}
#endif
//! \endcond
