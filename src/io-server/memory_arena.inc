! Copyright (C) 2021  Environnement et Changement climatique Canada
! 
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
! 
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

! This file has been generated from memory_arena.c

!******************************************************************************
!        INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
!  --------------------------------------------------------------------------
! We use the '//F_StArT' and '//F_EnD' tags to indicate the beginning and end
! of extraction. Anything that happens to be between these tags will be
! included in the output, with the leading '//' and trailing spaces removed.
!******************************************************************************

!> \file
!> \brief memory arena management package (C and Fortran)
!>
!> code extracted from file memory_arena.c
!> \verbatim
!>    set of routines to implement named block management in a memory pool
!>    possibly shared by multiple threads and processes
!>  
!>            master arena layout (there must be one and only one)
!>           (the master arena can contain multiple memory arenas)
!>           (the master table has one entry per memory arena)
!>    +--------------------+--------------------+---------------------+-------------------->
!>    | master table       | arena header       | symbol table        | data blocks
!>    +--------------------+--------------------+---------------------+-------------------->
!>  
!>            memory arena layout (multiple arenas can coexist)
!>    +--------------------+---------------------+-------------------->
!>    | arena header       | symbol table        | data blocks
!>    +--------------------+---------------------+-------------------->
!>  
!>    indices are used instead of addresses because the memory arena might be mapped 
!>    at different addresses in different processes
!>  
!>            data block layout
!>        +----------------------------------------------------------------- +
!>        |                                                                  |
!>        |                                                                  v
!>    +-------+-------+-------+-------+.....................+-------+-------+
!>    |  FWD  |  IX   |  NWD  | SIGNL |  user data portion  | SIGNH |  BWD  |
!>    +-------+-------+-------+-------+.....................+-------+-------+
!>    ^                                                                 |
!>    |                                                                 |
!>    +-----------------------------------------------------------------+
!>    FWD   : index of start of next block
!>    IX    : index in symbol table of this block
!>    NWD   : size of data portion in 64 bit units
!>    SIGNL : low marker (used for checking data underruns)
!>    SIGNH : high marker (used for checking data overruns)
!>    BWD   : index of start of this block
!>    FWD of last allocated block will point to a non existent block with FWD = 0
!>    
!>    FWD and BWD are indices into a 64 bit unsigned integer array starting at the beginning of the memory arena
!>    FWD, IX, NWD, SIGNL, SIGNH, BWD are 32 bit unsigned integers
!>
!>    indices are used instead of addresses because processes sharing a memory segment are more
!>    than likely to have said memory segment mapped at different addresses
!>
!> \endverbatim
interface

!> set owner's id (usually MPI rank) for memory arenas<br>
!> me = memory_arena_set_id(id)
function memory_arena_set_id(id) result(me) BIND(C,name='memory_arena_set_id')
import :: C_INT
integer(C_INT), intent(IN), value :: id                   !< owner's id (usually MPI rank) 
integer(C_INT) :: me                                      !< -1 upon error, value > 0 otherwise
end function memory_arena_set_id

!> set id for memory management arena, return identifier (-1 in case of error)
!> id must be a POSITIVE INTEGER
!> dump arena header and symbol table (description of contents of memory arena)<br>
!> call memory_arena_print_status(mem)
subroutine memory_arena_print_status(mem) BIND(C,name='memory_arena_print_status')
import :: C_PTR
type(C_PTR), intent(IN), value :: mem      !< pointer to memory arena (see  memory_arena_init)
end subroutine memory_arena_print_status

!> initialize an already allocated 'memory arena' (usually node shared memory)<br>
!> id = memory_arena_init(mem, nsym, size)
function memory_arena_init(mem, nsym, size) result(id) BIND(C,name='memory_arena_init')
import :: C_PTR, C_INT, C_INT64_T
type(C_PTR), intent(IN), value :: mem                 !< pointer to memory arena
integer(C_INT), intent(IN), value :: nsym             !< size of symbol table to allocate (max number of blocks expected)
integer(C_INT64_T), intent(IN), value :: size         !< size of memory area in bytes (max 32GBytes)
integer(C_INT) :: id                                  !< id of current owner process (not necessarily me)
end function memory_arena_init

!> update local arena control table from master arena<br>
!> nareas = update_local_table(mem)
function update_local_table(mem) result(nareas) BIND(C,name='update_local_table')
import :: C_PTR, C_INT
type(C_PTR), intent(IN), value :: mem            !< pointer to master memory arena (see  memory_arena_init)
integer(C_INT) :: nareas                         !< number of arenas detected
end function update_local_table

!> initialize an already allocated 'master arena' (node shared memory usually)<br>
!> id = master_arena_init(mem, nsym, size)
function master_arena_init(mem, nsym, size) result(id) BIND(C,name='master_arena_init')
import :: C_PTR, C_INT, C_INT64_T
type(C_PTR), intent(IN), value :: mem            !< pointer to master memory arena (see  memory_arena_init)
integer(C_INT), intent(IN), value :: nsym        !< size of symbol table to allocate (max number of blocks expected)
integer(C_INT64_T), intent(IN), value :: size    !< size of memory area for master arena in bytes
integer(C_INT) :: id                             !< id of current process
end function master_arena_init

!> find memory block called 'name'<br>
!> ptr = memory_block_find(mem, size, flags, name)
function memory_block_find(mem, size, flags, name) result(ptr) BIND(C,name='memory_block_find')
import :: C_PTR, C_INT, C_CHAR
type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  memory_arena_init)
integer(C_INT), intent(OUT) :: size                      !< size of memory block in 32 bit units (0 if not found)
integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
end function memory_block_find

!> same as memory_block_find, but wait until block is created or timeout (in milliseconds) expires<br>
!> ptr = memory_block_find_wait(mem, size, flags, name, timeout)
function memory_block_find_wait(mem, size, flags, name, timeout) result(ptr) BIND(C,name='memory_block_find_wait')
import :: C_PTR, C_INT, C_CHAR
type(C_PTR), intent(IN), value :: mem                    !< pointer to memory arena (see  memory_arena_init)
integer(C_INT), intent(OUT) :: size                      !< size of memory block in 32 bit units (0 if not found)
integer(C_INT), intent(OUT) :: flags                     !< block flags (0 if not found)
character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
integer(C_INT), intent(IN), value :: timeout             !< timeout in milliseconds, -1 means practically forever
type(C_PTR) :: ptr                                       !< local address of memory block (NULL if not found)
end function memory_block_find_wait

!> mark memory block 'name' as initialized<br>
!> ptr = memory_block_mark_init(mem, name)
function memory_block_mark_init(mem, name) result(ptr) BIND(C,name='memory_block_mark_init')
import :: C_PTR, C_CHAR
type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  memory_arena_init)
character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
type(C_PTR) :: ptr                                       !< block address if found, NULL otherwise
end function memory_block_mark_init

!> create a named block in a managed 'memory arena'<br>
!> ptr = memory_block_create(mem, size, name)
function memory_block_create(mem, size, name) result(ptr) BIND(C,name='memory_block_create')
import :: C_PTR, C_INT, C_CHAR
type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  memory_arena_init)
integer(C_INT), intent(IN), value :: size                !< desired size of block in 32 bit units
character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to create (characters beyond the 8th will be ignored)
type(C_PTR) :: ptr                                       !< local address of created block (NULL if error)
end function memory_block_create

!> allocate a shared memory segment<br>
!> ptr = memory_allocate_shared(shmid, size)
function memory_allocate_shared(shmid, size) result(ptr) BIND(C,name='memory_allocate_shared')
import :: C_PTR, C_INT, C_INT64_T
integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (set by memory_allocate_shared) (see shmget)
integer(C_INT64_T), intent(IN), value :: size  !< size of segment in bytes
type(C_PTR) :: ptr                             !< local address of memory segment
end function memory_allocate_shared

!> create a memory arena in user memory<br>
!> ptr = memory_arena_create_from_address(memaddr, nsym, size)
function memory_arena_create_from_address(memaddr, nsym, size) result(ptr) BIND(C,name='memory_arena_create_from_address')
import :: C_PTR, C_INT, C_INT64_T
type(C_PTR), intent(IN), value :: memaddr      !< user memory address
integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
type(C_PTR) :: ptr                             !< address of memory arena (NULL if error)
end function memory_arena_create_from_address

!> create a memory arena in shared memory<br>
!> ptr = memory_arena_create_shared(shmid, nsym, size)
function memory_arena_create_shared(shmid, nsym, size) result(ptr) BIND(C,name='memory_arena_create_shared')
import :: C_PTR, C_INT, C_INT64_T
integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (see shmget)
integer(C_INT), intent(IN), value :: nsym      !< size of symbol table to allocate (max number of blocks expected)
integer(C_INT64_T), intent(IN), value :: size  !< size of arena in bytes
type(C_PTR) :: ptr                             !< local address of memory arena
end function memory_arena_create_shared

!> create master memory arena in shared memory<br>
!> ptr = master_arena_create_shared(shmid, nsym, size)
function master_arena_create_shared(shmid, nsym, size) result(ptr) BIND(C,name='master_arena_create_shared')
import :: C_PTR, C_INT, C_INT64_T
integer(C_INT), intent(OUT) :: shmid                  !< shared memory id of segment (see shmget)
integer(C_INT), intent(IN), value :: nsym             !< size of symbol table to allocate (max number of blocks expected)
integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
type(C_PTR) :: ptr                                    !< local address of master memory arena
end function master_arena_create_shared

!> get memory address associated with shared memory segment id<br>
!> ptr = memory_address_from_id(shmid)
function memory_address_from_id(shmid) result(ptr) BIND(C,name='memory_address_from_id')
import :: C_PTR, C_INT
integer(C_INT), intent(IN), value :: shmid           !< shared memory id of segment (see shmget)
type(C_PTR) :: ptr                             !< local memory addres of shared memory segment
end function memory_address_from_id

!> get memory arena address of master arena address<br>
!> ptr = memory_arena_from_master(mem)
function memory_arena_from_master(mem) result(ptr) BIND(C,name='memory_arena_from_master')
import :: C_PTR
type(C_PTR), intent(IN), value :: mem         !< pointer to the 'master memory arena'
type(C_PTR) :: ptr                            !< local memory addres of memory arena of master arena
end function memory_arena_from_master

!> get memory address associated with shared memory segment id of master arena<br>
!> 
function memory_arena_from_master_id(shmid) result(ptr) BIND(C,name='memory_arena_from_master_id')
import :: C_PTR, C_INT
integer(C_INT), intent(IN), value :: shmid    !< master arena segment id (from master_arena_create_shared)
type(C_PTR) :: ptr                            !< local memory addres of memory arena of master arena
end function memory_arena_from_master_id

end interface
