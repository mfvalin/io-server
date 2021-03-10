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

! This file has been generated from circular_buffer.c
!> \file
!> \brief circular buffer package (C and Fortran)
!>
!> \verbatim
!>           circular buffer data layout
!>
!>   (IN = OUT) (bufer empty) (LIMIT - FIRST -1 free slots)
!>
!> FIRST                                                   LIMIT
!>   |                                                       |
!>   v                                                       v
!>   +------------------------------------------------------+
!>   ........................................................
!>   ^------------------------------------------------------+
!>   |
!> IN/OUT
!>   +------------------------------------------------------+
!>   ........................................................
!>   +--------------------^---------------------------------+
!>                        |
!>                      IN/OUT
!>
!>   (IN = OUT - 1) (buffer full)
!>
!> FIRST                                                   LIMIT
!>   |                                                       |
!>   v                                                       v
!>   +------------------------------------------------------+
!>   xxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!>   +-------------------^^---------------------------------+
!>                       ||
!>                     IN  OUT
!>   +------------------------------------------------------+
!>   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
!>   ^------------------------------------------------------^
!>   |                                                      |
!>  OUT                                                     IN
!>
!>   (OUT < IN) (LIMIT - IN -1) free, (IN - OUT) data
!> FIRST                                                   LIMIT
!>   |                                                       |
!>   v                                                       v
!>   +------------------------------------------------------+
!>   xxxxxxxxxxxxxx..........................................
!>   ^-------------^----------------------------------------+
!>   |             |
!>  OUT            IN
!>
!>   (IN < OUT) (OUT - IN -1) free, (LIMIT - OUT + IN - FIRST) data
!> FIRST                                                   LIMIT
!>   |                                                       |
!>   v                                                       v
!>   +------------------------------------------------------+
!>   xxxxxxxxxxxxxx................................xxxxxxxxxx
!>   +-------------^-------------------------------^--------+
!>                 |                               |
!>                 IN                             OUT
!>   x = useful data       . = free space
!> \endverbatim
include 'io-server/common.inc'
interface
subroutine CB_print_header(buffer) bind(C, name = 'CB_print_header')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< C pointer to the buffer we want to print
end subroutine CB_print_header
!> initialize a circular buffer<br>
!> buffer = CB_init(p, nwords)
function CB_init(p, nwords) result(buffer) bind(C,name='CB_init')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p           !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nwords   !< the size in elements of the circular buffer
type(C_PTR) :: buffer                         !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
end function CB_init
!> create and initialize a circular buffer of size nwords in "shared memory"<br>
!> p = CB_create_shared(shmid, nwords)
function CB_create_shared(shmid, nwords) result(p) BIND(C,name='CB_create_shared')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(OUT) :: shmid          !< identifier of shared memory area (see man shmget) (-1 upon error)
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function CB_create_shared
!> detach "shared memory segment" used by circular buffer <br>
!> status = CB_detach_shared(p)
function CB_detach_shared(p) result(status) BIND(C,name='CB_detach_shared')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT) :: status               !< 0 upon success, -1 upon error
end function CB_detach_shared
!> create and initialize a circular buffer of size nwords in process memory<br>
!> p = CB_create(nwords)
function CB_create(nwords) result(p) BIND(C,name='CB_create')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function CB_create
!> create and initialize a circular buffer of size nwords from user supplied memory<br>
!> p = CB_from_pointer(ptr, nwords)
function CB_from_pointer(ptr, nwords) result(p) BIND(C,name='CB_from_pointer')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: ptr         !< pointer to user supplied memory
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function CB_from_pointer
function CB_get_available_space(buffer) result(num_elements) BIND(C, name = 'CB_get_available_space')
import C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
integer(C_INT) :: num_elements           !< How many slots are free
end function CB_get_available_space
function CB_get_available_data(buffer) result(num_elements) BIND(C, name = 'CB_get_available_data')
import C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
integer(C_INT) :: num_elements           !< How many elements are stored in the buffer
end function CB_get_available_data
!> wait until at least na empty slots are available for inserting data<br>
!> n = CB_wait_space_available(p, na)
function CB_wait_space_available(p, na) result(n) BIND(C,name='CB_wait_space_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p          !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: na      !< needed number of available slots
integer(C_INT) :: n                          !< actual number of empty slots available, -1 on error
end function CB_wait_space_available
!> wait until at least n data tokens are available for extracting data<br>
!> p = CB_wait_data_available(p, na)
function CB_wait_data_available(p, na) result(n) BIND(C,name='CB_wait_data_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: na        !< needed number of available tokens
integer(C_INT) :: n                            !< actual number of data tokens available, -1 if error
end function CB_wait_data_available
!> get the address of the first position in the circular data buffer<br>
!> start = CB_start(p)
function CB_start(p) result(start) BIND(C,name='CB_start')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: start                   !< pointer to beginning of circular buffer
end function CB_start
!> get the address of the  insertion point in the circular data buffer (data snoop)<br>
!> inp = CB_data_in(p)
function CB_data_in(p) result(inp) BIND(C,name='CB_data_in')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: inp                     !< address of the  insertion point in the circular data buffer
end function CB_data_in
!> get the address of the extraction point in the circular data buffer (data snoop)<br>
!> outp = CB_data_out(p)
function CB_data_out(p) result(outp) BIND(C,name='CB_data_out')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: outp                    !< address of the extraction point in the circular data buffer
end function CB_data_out
!> get pointer to the in position, assume that the caller knows the start of data buffer<br>
!> inp = CB_advance_in(p, n1, n2)
function CB_advance_in(p, n1, n2) result(inp) BIND(C,name='CB_advance_in')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT), intent(OUT)    :: n1   !< number of slots available at the "in" position, -1 upon error
integer(C_INT), intent(OUT)    :: n2   !< number of slots available at the "start" of the buffer, -1 upon error
type(C_PTR)                    :: inp  !< pointer to the "in" position, C_NULL_PTR upon error
end function CB_advance_in
!> get pointer to the "out" position, assume that the caller knows the start of data buffer<br>
!> outp = CB_advance_out(p, n1, n2)
function CB_advance_out(p, n1, n2) result(outp) BIND(C,name='CB_advance_out')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT), intent(OUT)    :: n1   !< number of tokens available at the "out" position, -1 upon error
integer(C_INT), intent(OUT)    :: n2   !< number of tokens available at the "start" of the buffer, -1 upon error
type(C_PTR)                    :: outp !< pointer to the "out" position, C_NULL_PTR upon error
end function CB_advance_out
!> wait until ndst tokens are available then extract them into dst<br>
!> n = CB_atomic_get(p, dst, ndst)
function CB_atomic_get(p, dst, ndst) result(n) BIND(C,name='CB_atomic_get')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: ndst          !< number of tokens to extract
integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
integer(C_INT) :: n                                !< number of data tokens available after this operation, -1 if error
end function CB_atomic_get
!> wait until ndst tokens are available then copy to dst array<br>
!> DO NOT UPDATE "out" unless update flag is non zero<br>
!> n = CB_extract(p, dst, ndst, offset, update)
function CB_extract(p, dst, ndst, offset, update) result(n) BIND(C,name='CB_extract')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: ndst          !< number of tokens to copy to dst
integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array for data extraction
integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
integer(C_INT) :: n                                !< number of free slots available after this operation
end function CB_extract
!> wait until nsrc free slots are available then insert from src array<br>
!> n = CB_atomic_put(p, src, nsrc)
function CB_atomic_put(p, src, nsrc) result(n) BIND(C,name='CB_atomic_put')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
integer(DATA_ELEMENT), dimension(*), intent(IN) :: src    !< source array for data insertion
integer(C_INT) :: n                                !< number of free slots available after this operation
end function CB_atomic_put
!> wait until nsrc free slots are available then insert from src array<br>
!> DO NOT UPDATE the "in" pointer unless update flag is non zero<br>
!> n = CB_insert(p, src, nsrc, offset, update)
function CB_insert(p, src, nsrc, offset, update) result(n) BIND(C,name='CB_insert')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
integer(DATA_ELEMENT), dimension(*), intent(IN) :: src    !< source array for data insertion
integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
integer(C_INT) :: n                                !< number of free slots available after this operation
end function CB_insert
function CB_check_integrity(buffer) result(is_valid) BIND(C, name = 'CB_check_integrity')
import C_INT, C_PTR
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) is_valid
end function CB_check_integrity
end interface
