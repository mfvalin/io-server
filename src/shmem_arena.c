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
// !> code extracted from file shmem_arena.c
// !> \verbatim
// !>    set of routines to implement named block management in a memory pool
// !>    possibly shared by multiple threads and processes
// !>  
// !>            memory arena layout
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
// !>    FWD, IX, NWD, SIGNL, SIGNH, BWD are 64 bit unsigned integers
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

#include "io-server/shmem_arena.h"
#include "io-server/rpn_extra.h"

// memory arena definitions follow, they should already have been included from shmem_arena.h

//C_StArT
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
//C_EnD

// memory arena management code

// ===============  start of global data  ===============

static uint32_t me = 999999999;  // identifier for this process (usually MPI rank) (alternative : getpid() )

// ===============  end of global data  ===============

//F_StArT
//  interface
//
//F_EnD

//F_StArT
// !> set owner's id (usually MPI rank) for memory arenas<br>
// !> me = shmem_arena_set_id(id)
// function shmem_arena_set_id(id) result(me) BIND(C,name='shmem_arena_set_id')
//   import :: C_INT
//   integer(C_INT), intent(IN), value :: id                   !< owner's id (usually MPI rank) 
//   integer(C_INT) :: me                                    !< -1 upon error, value > 0 otherwise
// end function shmem_arena_set_id
//
// !> set id for memory management arena, return identifier (-1 in case of error)
// !> id must be a POSITIVE INTEGER
//F_EnD
//C_StArT
//! set owner's id (usually MPI rank) for memory arenas<br>
//! me = shmem_arena_set_id(id)
//! @return -1 upon error, value > 0 otherwise
int32_t shmem_arena_set_id(
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
static inline uint64_t block_name_64(unsigned char *name //!< [in] string to be translated
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
// !> call shmem_arena_print_status(mem)
// subroutine shmem_arena_print_status(mem) BIND(C,name='shmem_arena_print_status')
//   import :: C_PTR
//   type(C_PTR), intent(IN), value :: mem      !< pointer to memory arena (see  shmem_arena_init)
// end subroutine shmem_arena_print_status
//
//F_EnD
//C_StArT
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! shmem_arena_print_status(mem)
//! @return none
void shmem_arena_print_status(
  void *mem                                  //!< [in] pointer to memory arena (see  shmem_arena_init)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  shmem_arena *ma = (shmem_arena *) mem;
  symtab_entry_64 *sym = ma->t;
  char name[9];
  uint64_t dname, size64;
  uint64_t *dataptr64;
  block_header_64 *bh, *bhnext;
  block_tail_64 *bt;
  uint64_t owner64 = ma->owner & 0x3FFFFFFF;

  if((ma->owner & 0x80000000u) == 0) {
    fprintf(stdout,"ERROR : NOT a 64 bit arena\n");
    fflush(stdout);
    return;
  }
  fprintf(stdout,"\n==============================================\n");
  fprintf(stdout,"Arena Header, id = %d, address = %p\n", me - 1 , (void*)ma);  // me -1 is the id
  fprintf(stdout,"owner       = %8.8lx\n",owner64 - 1);                     // id + 1 was stored, print id
  fprintf(stdout,"max entries = %d\n",ma->max_entries);
  fprintf(stdout,"max size    = %ld bytes\n",ma->arena_size*sizeof(uint64_t));
  fprintf(stdout,"entries     = %d\n",ma->n_entries);
  fprintf(stdout,"first free  = byte(%ld)\n",ma->first_free*sizeof(uint64_t));

  fprintf(stdout,"\nSymbol table\n");
  for(unsigned int i = 0 ; i < ma->n_entries ; i++){
    size64 = sym[i].data_size;
    dataptr64 = sym[i].data_index + mem64; 
    bh = (block_header_64 *) (dataptr64);
    bt = (block_tail_64   *) (dataptr64 + BlockHeader64Size64 + size64);
    bhnext = (block_header_64 *) (dataptr64 + BlockHeader64Size64 + size64 + BlockTail64Size64);
    dname = sym[i].data_name;
    const int sane =
           ( bh->sign == 0xBEEFF00D ) & 
           ( bt->sign == 0xDEADBEEF ) & 
           (i == bh->ix) & 
           (bt->bwd == sym[i].data_index) &
           (bh->nwd == sym[i].data_size) &
           (bh->fwd == sym[i].data_index + sym[i].data_size + BlockHeader64Size64 + BlockTail64Size64) ;
    for(int j = 0; j < 9 ; j++) {
      name[j] = '\0';
      if( 0 == (dname >> 56) ) dname <<= 8;
    }
    for(int j = 0; j < 8 ; j++) {
      name[j] = dname >> 56;
      dname <<= 8;
    }
    fprintf(stdout,"%4d: %8ld F=%8.8x I=%12ld S=%12ld (%12ld) %s %s %s FW=%12ld FWNXT=%12ld BW=%12ld '%s'\n",
            i,bh->ix,sym[i].flags,sym[i].data_index,sym[i].data_size*sizeof(uint64_t),bh->nwd,
            sane ? "T" : "F", ( bh->sign == 0xBEEFF00D ) ? "t" : "f", ( bt->sign == 0xDEADBEEF ) ? "t" : "f",
            bh->fwd, bhnext->fwd, bt->bwd, name);
  }
  fprintf(stdout,"==============================================\n");
  fflush(stdout);
}

//F_StArT
// !> initialize an already allocated 'memory arena' (usually node shared memory)<br>
// !> id = shmem_arena_init(mem, nsym, size)
// function shmem_arena_init(mem, nsym, size) result(id) BIND(C,name='shmem_arena_init')
//   import :: C_PTR, C_INT, C_INT64_T
//   type(C_PTR), intent(IN), value :: mem                 !< pointer to memory arena
//   integer(C_INT), intent(IN), value :: nsym             !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size         !< size of memory area in bytes (max 32GBytes)
//   integer(C_INT) :: id                                  !< id of current owner process (not necessarily me)
// end function shmem_arena_init
//
//F_EnD
//C_StArT
//! dump arena header and symbol table (description of contents of memory arena)<br>
//! id = shmem_arena_init(mem, nsym, size)
//! @return id of current owner process (not necessarily me)
uint32_t shmem_arena_init(
  void *mem,                   //!< [in] pointer to memory arena
  uint32_t nsym,               //!< [in] size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in] size of memory area in bytes
  )
//C_EnD
{
  shmem_arena *ma = (shmem_arena *) mem;
  symtab_entry_64 *sym = ma->t;
  uint64_t size64 = size >> 3 ;  // round size down to 64 bit element size
  // uint32_t size32 = (size64 > 0xFFFFFFFFL) ? 0xFFFFFFFFU : size64 ;  // must fit in 32 bits
  if(ma->owner != 0) {
    fprintf(stderr,"ma init %p, already owned by id = %d\n", (void*)ma, ma->owner & 0x3FFFFFFF);
    return (ma->owner & 0x3FFFFFFF);                           // area already initialized, return id of initializer
  }else{
//     fprintf(stderr,"ma init %p, not owned, id = %d\n", ma, ma->owner);
    fprintf(stderr," DEBUG: ma init %p, not owned, ", (void*)ma);
  }

  while(__sync_val_compare_and_swap(&(ma->lock), 0, me) != 0); // lock memory arena
// fprintf(stderr,"DEBUG: memory arena %p locked by %d\n", ma, me);

  ma->owner = 0;           // initialize arena header
  ma->max_entries = nsym;
  ma->first_free = ArenaHeader64Size64 + nsym * SymtabEntry64Size64;
// fprintf(stderr,"ArenaHeader64Size64 = %d, SymtabEntry64Size64 = %d, nsym = %d, base = %d\n",ArenaHeader64Size64,SymtabEntry64Size64,nsym,ma->first_free);
  ma->n_entries = 0;
//   ma->arena_size = size32;
  ma->arena_size = size64;

  for(uint32_t i = 0 ; i < nsym ; i++){   // initialize symbol table to null values
    sym[i].lock       = 0;
    sym[i].flags      = 0;
    sym[i].data_index = 0;
    sym[i].data_size  = 0;
    sym[i].data_name  = 0;
  }

  ma->owner = me | 0x80000000u;  // flag area as initialized by me

  const uint32_t val = __sync_val_compare_and_swap(&(ma->lock), me, 0); // unlock memory arena and return my id
  // fprintf(stderr,"DEBUG: memory arena %p UNlocked and owned by %d\n", ma, me);
  fprintf(stderr," UNlocked and owned now by id = %d\n", me - 1);
  return val;
}


// find entry in symbol table, return index if found, -1 otherwise
static inline int32_t find_block_64(shmem_arena *ma, symtab_entry_64 *sym, uint64_t name64){
  uint32_t i;

  if((ma->owner & 0x80000000u) == 0) return -1;  // not a 64 bit arena

  for(i = 0 ; i < ma->n_entries ; i++){
    if(sym[i].data_name == name64){
      return i;
    }
  }
  return -1 ; // miserable failure
}

//F_StArT
// !> find memory block called 'name'<br>
// !> ptr = shmem_block_find(mem, size, flags, name)
// function shmem_block_find(mem, size, flags, name) result(ptr) BIND(C,name='shmem_block_find')
//   import :: C_PTR, C_INT, C_CHAR, C_INT64_T
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  shmem_arena_init)
//   integer(C_INT64_T), intent(OUT) :: size                  !< size of memory block in bytes (0 if not found)
//   integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
// end function shmem_block_find
//
//F_EnD
//C_StArT
//! find memory block called 'name'<br>
//! ptr = shmem_block_find(mem, size, flags, name)
//! @return local address of memory block (NULL if not found)
void *shmem_block_find(
  void *mem,                      //!< [in]  pointer to memory arena
  uint64_t *size,                 //!< [OUT] size of memory block in bytes (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  shmem_arena *ma = (shmem_arena *) mem;
  symtab_entry_64 *sym = ma->t;
  void *dataptr = NULL;
  uint64_t name64 = block_name_64(name);
  int32_t i;

  *size = 0;         // precondition to fail
  *flags = 0;
  if(ma == NULL || name == NULL) return NULL;
  name64 = block_name_64(name);
  if(name64 == 0) return NULL;

  i = find_block_64(ma, sym, name64);
  if(i < 0) return NULL;  // name not found in symbol table

  *size = sym[i].data_size * sizeof(uint64_t);                   // return size in bytes
  *flags = sym[i].flags;
  dataptr = &mem64[sym[i].data_index + BlockHeader64Size64];     // pointer to actual data
  return dataptr;
}

//F_StArT
// !> same as shmem_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
// !> ptr = shmem_block_find_wait(mem, size, flags, name, timeout)
// function shmem_block_find_wait(mem, size, flags, name, timeout) result(ptr) BIND(C,name='shmem_block_find_wait')
//   import :: C_PTR, C_INT, C_CHAR, C_INT64_T
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  shmem_arena_init)
//   integer(C_INT64_T), intent(OUT) :: size                  !< size of memory block in bytes (0 if not found)
//   integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   integer(C_INT), intent(IN), value :: timeout             !< timeout in milliseconds, -1 means practically forever
//   type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
// end function shmem_block_find_wait
//
//F_EnD
//C_StArT
//! same as shmem_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
//! ptr = shmem_block_find_wait(mem, size, flags, name, timeout)
//! @return local address of memory block (NULL if not found)
void *shmem_block_find_wait(
  void *mem,                      //!< [in]  pointer to memory arena (see  shmem_arena_init)
  uint64_t *size,                 //!< [OUT] size of memory block in bytes (0 if not found)
  uint32_t *flags,                //!< [OUT] block flags (0 if not found)
  unsigned char *name,            //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  int timeout                     //!< [in]  timeout in milliseconds, -1 means practically forever
  )
//C_EnD
{
  void *p = NULL;
  int delay = 1000;  // 1000 microseconds = 1 millisecond
  shmem_arena *ma = (shmem_arena *) mem;

  if((ma->owner & 0x80000000u) == 0) return NULL;  // not a 64 bit arena

  p = shmem_block_find(mem, size, flags, name);     // does the block exist ?
  while(p == NULL && timeout > 0) {                  // no, sleep a bit and retry
    rpn_usleep(delay); timeout --;                  // decrement timeout
    p = shmem_block_find(mem, size, flags, name);   // does the block exist ?
  }
// fprintf(stderr,"timeout = %d\n",timeout);
  return p;
}

//F_StArT
// !> mark memory block 'name' as initialized<br>
// !> ptr = shmem_block_mark_init(mem, name)
// function shmem_block_mark_init(mem, name) result(ptr) BIND(C,name='shmem_block_mark_init')
//   import :: C_PTR, C_CHAR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  shmem_arena_init)
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< block address if found, NULL otherwise
// end function shmem_block_mark_init
//
//F_EnD
//C_StArT
//! mark memory block 'name' as initialized<br>
//! ptr = shmem_block_mark_init(mem, name)
//! @return block address if found, NULL otherwise
void *shmem_block_mark_init(
  void *mem,                       //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
  unsigned char *name              //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  shmem_arena *ma = (shmem_arena *) mem;
  symtab_entry_64 *sym = ma->t;
  uint64_t name64 = block_name_64(name);
  int32_t i;
  void *dataptr = NULL;

  i = find_block_64(ma, sym, name64);
  if(i < 0) return NULL;  // name not found in symbol table

  while(__sync_val_compare_and_swap(&(sym[i].lock), 0, me) != 0); // lock block
  if(sym[i].flags == 0) sym[i].flags = me;                        // mark as initialized by me
  __sync_val_compare_and_swap(&(sym[i].lock), me, 0);         // unlock block

  dataptr = &mem64[sym[i].data_index + BlockHeader64Size64];        // pointer to actual data
  return dataptr;
}

//F_StArT
// !> find the max size allowed for next block in a managed 'memory arena'<br>
// !> size = shmem_block_max_size(mem)
// function shmem_block_max_size(mem) result(size) BIND(C,name='shmem_block_max_size')
//   import :: C_INT64_T, C_PTR
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  shmem_arena_init)
//   integer(C_INT64_T) :: size                               !< size of block in bytes
// end function shmem_block_max_size
//
//F_EnD
//C_StArT
//! find the max size allowed for next block in a managed 'memory arena'<br>
//! int64_t size64
//! size64 = shmem_block_max_size(mem)
//! @return maximum size for next block allocated in bytes
int64_t shmem_block_max_size(
  void *mem                        //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
    )
//C_EnD
{
  shmem_arena *ma = (shmem_arena *) mem;
  uint64_t head64 ;  // sum of block header and tail sizes in 64 bit units
  uint64_t free64 ;          // free 64 bit slots left in arena
  if((ma->owner & 0x80000000u) == 0) return -1;      // not a 64 bit arena
  head64 = BlockHeader64Size64 + BlockTail64Size64;  // sum of block header and tail sizes in 64 bit units
  free64 = ma->arena_size - ma->first_free;          // free 64 bit slots left in arena
  free64 = (free64 <= head64) ? 0 : free64 - head64;          // max data payload size in 64 bit units
  return (free64 * sizeof(uint64_t));                         // convert into bytes
}

//F_StArT
// !> create a named block in a managed 'memory arena'<br>
// !> ptr = shmem_block_create(mem, size, name)
// function shmem_block_create(mem, size, name) result(ptr) BIND(C,name='shmem_block_create')
//   import :: C_PTR, C_CHAR, C_SIZE_T
//   type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  shmem_arena_init)
//   integer(C_SIZE_T), intent(IN), value :: size             !< desired size of block in bytes
//   character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to create (characters beyond the 8th will be ignored)
//   type(C_PTR) :: ptr                                       !< local address of created block (NULL if error)
// end function shmem_block_create
//
//F_EnD
//C_StArT
//! create a named block in a managed 'memory arena'<br>
//! ptr = shmem_block_create(mem, size, name)
//! @return local address of created block (NULL if error)
void *shmem_block_create(
  void *mem,                      //!< [in]  pointer to the managed 'memory arena' (see  shmem_arena_init)
  size_t size,                    //!< [in]  desired size of block in bytes
  unsigned char *name             //!< [in]  name of block to find (characters beyond the 8th will be ignored)
  )
//C_EnD
{
  uint64_t *mem64 = (uint64_t *) mem;
  shmem_arena *ma = (shmem_arena *) mem;
  symtab_entry_64 *sym = ma->t;
  uint32_t i;
  uint64_t next;
  uint32_t fail;
  uint64_t size64 = (size + 7) >> 3;  // round size up to 64 bit element size
  uint64_t block64 = size64 + BlockHeader64Size64 + BlockTail64Size64;
  block_header_64 *bh;
  block_tail_64 *bt;
  char *dataptr;
  uint64_t name64 = block_name_64(name);

  if((ma->owner & 0x80000000u) == 0) return NULL;           // not a 64 bit arena

  while(__sync_val_compare_and_swap(&(ma->lock), 0, me) != 0); // lock memory area
// fprintf(stderr,"shmem_block_create LOCK by %d\n",me);
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

    bh = (block_header_64 *) (mem64 + ma->first_free);    // start of block
    dataptr = (char *) (mem64 + ma->first_free + BlockHeader64Size64) ; // start of data in block
    bh->fwd = next;                       // next block will start there
    bh->ix = i;                           // index of this block in symbol table
    bh->nwd = size64;                     // size of data portion
    bh->sign = 0xBEEFF00D;                // marker below data

    bt = (block_tail_64 *) (mem64 + ma->first_free + BlockHeader64Size64 + size64);
    bt->sign = 0xDEADBEEF;                // marker above data
    bt->bwd = sym[i].data_index;          // back pointer, index of start of current block

    ma->first_free = next;                // bump index of next free position
    ma->n_entries++;                      // bump number of valid entries
  }

  i = __sync_val_compare_and_swap(&(ma->lock), me, 0);         // unlock memory area
// fprintf(stderr,"shmem_block_create UNLOCK by %d\n",me);
  return dataptr;
}

//F_StArT
// !> allocate a shared memory segment<br>
// !> ptr = shmem_allocate_shared(shmid, size)
// function shmem_allocate_shared(shmid, size) result(ptr) BIND(C,name='shmem_allocate_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (set by shmem_allocate_shared) (see shmget)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of segment in bytes
//   type(C_PTR) :: ptr                             !< local address of memory segment
// end function shmem_allocate_shared
//
//F_EnD
//C_StArT
//! allocate a shared memory segment<br>
//! ptr = shmem_allocate_shared(shmid, size)
//! @return local address of memory block
void *shmem_allocate_shared(
  int *shmid,                 //!< [out] shared memory id of segment (set by shmem_allocate_shared) (see shmget)
  uint64_t size               //!< [in]  size of segment in bytes
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
// !> ptr = shmem_arena_create_from_address(memaddr, nsym, size)
// function shmem_arena_create_from_address(memaddr, nsym, size) result(ptr) BIND(C,name='shmem_arena_create_from_address')
//   import :: C_PTR, C_INT, C_INT64_T
//   type(C_PTR), intent(IN), value :: memaddr      !< user memory address
//   integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
//   type(C_PTR) :: ptr                             !< address of memory arena (NULL if error)
// end function shmem_arena_create_from_address
//
//F_EnD
//C_StArT
//! create a memory arena in user memory<br>
//! ptr = shmem_arena_create_from_address(memaddr, nsym, size)
//! @return  address of memory arena (NULL if error)
void *shmem_arena_create_from_address(
  void *memaddr,               //!< [in]  user memory address
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in 32 bit units
  )
//C_EnD
{
  int err;

  if(memaddr == NULL) return memaddr;             // invalid address

  err = shmem_arena_init(memaddr, nsym, size);   // initialize memory arena
  if(err < 0) return NULL;

  return memaddr;
}

//F_StArT
// !> create a memory arena in shared memory<br>
// !> ptr = shmem_arena_create_shared(shmid, nsym, size)
// function shmem_arena_create_shared(shmid, nsym, size) result(ptr) BIND(C,name='shmem_arena_create_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (see shmget)
//   integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
//   type(C_PTR) :: ptr                             !< local address of memory arena
// end function shmem_arena_create_shared
//
//F_EnD
//C_StArT
//! create a memory arena in shared memory<br>
//! ptr = shmem_arena_create_shared(shmid, nsym, size)
//! @return  local address of memory arena
void *shmem_arena_create_shared(
  int *shmid,                  //!< [out] shared memory id of segment (see shmget)
  uint32_t nsym,               //!< [in]  size of symbol table to allocate (max number of blocks expected)
  uint64_t size                //!< [in]  size of segment in bytes
  )
//C_EnD
{
  void *shmaddr = shmem_allocate_shared(shmid, size);    // request shared memory block
  int err;

  if(shmaddr == NULL) return shmaddr;             // request failed

  err = shmem_arena_init(shmaddr, nsym, size);   // initialize memory arena
  if(err < 0) return NULL;

  return shmaddr;
}

//F_StArT
// !> get memory address associated with shared memory segment id<br>
// !> ptr = shmem_address_from_id(shmid)
// function shmem_address_from_id(shmid) result(ptr) BIND(C,name='shmem_address_from_id')
//   import :: C_PTR, C_INT
//   integer(C_INT), intent(IN), value :: shmid           !< shared memory id of segment (see shmget)
//   type(C_PTR) :: ptr                             !< local memory addres of shared memory segment
// end function shmem_address_from_id
//
//F_EnD
//C_StArT
//! get memory address associated with shared memory segment id<br>
//! ptr = shmem_address_from_id(shmid)
//! @return local memory addres of shared memory segment
void *shmem_address_from_id(
  int shmid                  //!< [in] shared memory id of segment (see shmget)
  )
//C_EnD
{
  void *p = shmat(shmid, NULL, 0) ;
//   printf("shmid = %d, address = %p\n",shmid, p);
  return p;
}

//F_StArT
//  end interface
//F_EnD

#if defined(SELF_TEST)
#include <errno.h>

#include <mpi.h>

#define NSYM 128
#define DBLK 80

int main(int argc, char **argv){
  int err, rank, size, id, disp_unit, id2;
  int shmid = -1;
  void *shmaddr = NULL;
  void *p = NULL;
  int shmsz = 1024 * 128;  // 4 KBytes
  uint64_t shmsz64 = shmsz;
  uint64_t size;
  uint32_t size_32;
  struct shmid_ds dummy;
  MPI_Win win ;
  MPI_Aint winsize ;
  int64_t mbs[5] ;
  uint32_t flags;
  char cmd[1024];

  err = MPI_Init(&argc, &argv);
  err = MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  err = MPI_Comm_size(MPI_COMM_WORLD,&size);
  id = shmem_arena_set_id(rank);

#if defined(MPI_SHARED)
  winsize = (rank == 0) ? shmsz : 0 ;     // alloctate size not zero only on PE 0
  err = MPI_Win_allocate_shared (winsize, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &shmaddr, &win) ;
  err = MPI_Win_shared_query (win, 0, &winsize, &disp_unit, &shmaddr) ;  // get address of segment for rank 0
#endif

  if(rank == 0) {
#if defined(MPI_SHARED)
    shmaddr = shmem_arena_create_from_address(shmaddr, NSYM, shmsz64);
    fprintf(stderr,"rank 0 creating memory arena from address %p\n", shmaddr);
#else
    shmaddr = shmem_arena_create_shared(&shmid, NSYM, shmsz64);
    fprintf(stderr,"rank 0 creating memory arena in shared memory, id = %d\n", shmid);
#endif
    id2 = shmem_arena_init(shmaddr, NSYM, shmsz64);
    fprintf(stderr,"id2 = %d, rank = %d, free = %ld\n", id2, rank, shmem_block_max_size(shmaddr));
    shmem_arena_print_status(shmaddr);
    p = shmem_block_create(shmaddr, DBLK*1,   "BLOCK000"); p = shmem_block_mark_init(shmaddr, "BLOCK000"); mbs[0] = shmem_block_max_size(shmaddr);
    p = shmem_block_create(shmaddr, mbs[0]/2, "BLOCK001"); p = shmem_block_mark_init(shmaddr, "BLOCK001"); mbs[1] = shmem_block_max_size(shmaddr);
    p = shmem_block_create(shmaddr, mbs[1]/2, "BLOCK002"); p = shmem_block_mark_init(shmaddr, "BLOCK002"); mbs[2] = shmem_block_max_size(shmaddr);
    p = shmem_block_create(shmaddr, mbs[2]/2, "BLOCK003"); p = shmem_block_mark_init(shmaddr, "BLOCK003"); mbs[3] = shmem_block_max_size(shmaddr);
    p = shmem_block_create(shmaddr, mbs[3]/2, "BLOCK004"); p = shmem_block_mark_init(shmaddr, "BLOCK004"); mbs[4] = shmem_block_max_size(shmaddr);
    p = shmem_block_find(shmaddr, &size, &flags, "BLOCK004");
    if(p == NULL) {
      fprintf(stderr,"FAIL : BLOCK004 not found in 64 bit heap\n");
    }else{
      fprintf(stderr,"PASS : BLOCK004 was found in 64 bit heap\n");
    }
  }
#if ! defined(MPI_SHARED)
  err = MPI_Bcast(&shmid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD);
  if(rank != 0) shmaddr = shmat(shmid, NULL, 0);
#endif
  err = MPI_Barrier(MPI_COMM_WORLD);

  if(rank == 0) {
    fprintf(stderr,"I am process %d of %d, id = %d, shmid = %d, addr = %p free = %ld %ld %ld %ld %ld\n",
                   rank+1,size,id,shmid,shmaddr,mbs[0],mbs[1],mbs[2],mbs[3],mbs[4]);
  }else{
    fprintf(stderr,"I am process %d of %d, id = %d, shmid = %d, addr = %p free = %ld\n",
                   rank+1,size,id,shmid,shmaddr,shmem_block_max_size(shmaddr));
  }
  err = MPI_Barrier(MPI_COMM_WORLD);
  if(rank == 1) {
    snprintf(cmd,sizeof(cmd)-1,"ipcs -m | grep %d",shmid);
    fprintf(stderr,"\n%s\n",cmd);
    system("ipcs -m | head -3");
    system(cmd);
    shmem_arena_print_status(shmaddr);
  }
  err = MPI_Finalize();
}
#endif
//! \endcond
