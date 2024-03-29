! Copyright (C) 2022  Environnement et Changement climatique Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

! This file has been generated from circular_buffer.c

!******************************************************************************
!        INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
!  --------------------------------------------------------------------------
! We use the '//F_StArT' and '//F_EnD' tags to indicate the beginning and end
! of extraction. Anything that happens to be between these tags will be
! included in the output, with the leading '//' and trailing spaces removed.
!
!******************************************************************************
!     INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A FORTRAN FILE
!  --------------------------------------------------------------------------
! Same as for .C files, except that we use the '!! F_StArT' and '!! F_EnD'
! tags. Don't forget to include parameter declarations too.
!
!******************************************************************************

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
!>
!>
!>      With partial insertion/extraction
!>
!>  avail. data            available space           avail. data
!>   ...________          _________________         ____...
!>              |        |                 |       |
!>   +------------------------------------------------------+
!>   xxxxxxxxxxxx::::::::...................;;;;;;;xxxxxxxx
!>   +-----------^-------^------------------^------^--------+
!>               |       |                  |      |
!>               IN      |                 OUT     |
!>                  PARTIAL_IN                PARTIAL_OUT
!>
!>   x = useful data    : = partially inserted data
!>   . = free space     ; = partially extracted data
!>
!> \endverbatim
type :: cb_stats
integer(C_INT64_T) :: num_reads            = 0
integer(C_INT64_T) :: num_unique_reads     = 0
integer(C_INT64_T) :: num_read_elems       = 0
integer(C_INT64_T) :: num_fractional_reads = 0
real(C_DOUBLE)     :: total_read_wait_time_ms = 0.0
real(C_DOUBLE)     :: total_read_time_ms      = 0.0
integer(C_INT64_T) :: max_fill       = 0

integer(C_INT64_T) :: num_writes            = 0
integer(C_INT64_T) :: num_write_elems       = 0
integer(C_INT64_T) :: num_fractional_writes = 0
real(C_DOUBLE)     :: total_write_wait_time_ms = 0.0
real(C_DOUBLE)     :: total_write_time_ms      = 0.0
end type cb_stats
interface
function CB_get_stats(buffer) result(stats_ptr) bind(C, name = 'CB_get_stats')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: buffer
type(C_PTR) :: stats_ptr
end function CB_get_stats
subroutine CB_print_header(buffer) bind(C, name = 'CB_print_header')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< C pointer to the buffer we want to print
end subroutine CB_print_header
subroutine CB_dump_data(buffer, num_bytes) bind(C, name = 'CB_dump_data')
import :: C_PTR, C_INT64_T
implicit none
type(C_PTR), intent(IN), value :: buffer !< C pointer to the buffer we want to print
integer(C_INT64_T), intent(IN), value :: num_bytes !< How many bytes of data to print
end subroutine CB_dump_data
!> initialize a circular buffer<br>
!> buffer = CB_init_bytes(p, num_bytes)
function CB_init_bytes(p, num_bytes) result(buffer) bind(C,name='CB_init_bytes')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
type(C_PTR),       intent(IN), value :: p           !< pointer to a circular buffer
integer(C_SIZE_T), intent(IN), value :: num_bytes   !< the size in bytes of the circular buffer
type(C_PTR) :: buffer                               !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
end function CB_init_bytes
!> create and initialize a circular buffer of size num_bytes in "shared memory"<br>
!> p = CB_create_shared_bytes(shmid, num_bytes)
function CB_create_shared_bytes(shmid, num_bytes) result(p) BIND(C,name='CB_create_shared_bytes')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
integer(C_INT), intent(OUT)       :: shmid       !< identifier of shared memory area (see man shmget) (-1 upon error)
integer(C_SIZE_T), intent(IN), value :: num_bytes   !< size in number of bytes of the circular buffer
type(C_PTR) :: p                                 !< pointer to created circular buffer
end function CB_create_shared_bytes
!> detach "shared memory segment" used by circular buffer <br>
!> status = CB_detach_shared(p)
function CB_detach_shared(p) result(status) BIND(C,name='CB_detach_shared')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT) :: status               !< 0 upon success, -1 upon error
end function CB_detach_shared
!> create and initialize a circular buffer of size num_bytes in process memory<br>
!> p = CB_create_bytes(num_bytes)
function CB_create_bytes(num_bytes) result(p) BIND(C,name='CB_create_bytes')
import :: C_PTR, C_SIZE_T
implicit none
integer(C_SIZE_T), intent(IN), value :: num_bytes  !< size in bytes of the circular buffer
type(C_PTR) :: p                                   !< pointer to created circular buffer
end function CB_create_bytes
!> create and initialize a circular buffer of size num_bytes from user supplied memory<br>
!> p = CB_from_pointer_bytes(ptr, num_bytes)
function CB_from_pointer_bytes(ptr, num_bytes) result(p) BIND(C,name='CB_from_pointer_bytes')
import :: C_PTR, C_SIZE_T
implicit none
type(C_PTR),       intent(IN), value :: ptr        !< pointer to user supplied memory
integer(C_SIZE_T), intent(IN), value :: num_bytes  !< size in bytes of the circular buffer
type(C_PTR) :: p                                   !< pointer to created circular buffer
end function CB_from_pointer_bytes
pure function CB_get_available_space_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_available_space_bytes')
import C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
integer(C_SIZE_T) :: num_bytes           !< How many slots are free
end function CB_get_available_space_bytes
pure function CB_get_available_data_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_available_data_bytes')
import C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(in), value :: buffer !< Pointer to a circular buffer
integer(C_SIZE_T) :: num_bytes           !< How many bytes are stored in the buffer
end function CB_get_available_data_bytes
function CB_get_capacity_bytes(buffer) result(num_bytes) BIND(C, name = 'CB_get_capacity_bytes')
import C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(in), value :: buffer !< Pointer to the circular buffer we want to query
integer(C_SIZE_T) :: num_bytes           !< How many total elements can potentially be store in the buffer
end function CB_get_capacity_bytes
!> wait until at least num_bytes_wanted empty slots are available for inserting data<br>
!> n = CB_wait_space_available_bytes(p, num_bytes_wanted, timeout_ms)
function CB_wait_space_available_bytes(p, num_bytes_wanted, timeout_ms) result(n) BIND(C,name='CB_wait_space_available_bytes')
import :: C_PTR, C_SIZE_T, C_INT64_T, C_INT
implicit none
type(C_PTR),       intent(IN), value :: p                 !< pointer to a circular buffer
integer(C_SIZE_T), intent(IN), value :: num_bytes_wanted  !< needed number of available bytes
integer(C_INT),    intent(IN), value :: timeout_ms        !< How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
integer(C_INT64_T) :: n                                   !< actual number of available bytes, a negative error code on error
end function CB_wait_space_available_bytes
!> wait until at least num_bytes_wanted are available for extracting data<br>
!> p = CB_wait_data_available_bytes(p, num_bytes_wanted, timeout_ms)
function CB_wait_data_available_bytes(p, num_bytes_wanted, timeout_ms) result(n) BIND(C,name='CB_wait_data_available_bytes')
import :: C_PTR, C_SIZE_T, C_INT64_T, C_INT
implicit none
type(C_PTR),       intent(IN), value :: p                !< pointer to a circular buffer
integer(C_SIZE_T), intent(IN), value :: num_bytes_wanted !< needed number of available bytes
integer(C_INT),    intent(IN), value :: timeout_ms       !< How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
integer(C_INT64_T) :: n                                  !< actual number of bytes available, a negative error code if error
end function CB_wait_data_available_bytes
!> wait until num_bytes are available then extract them into dst<br>
!> CB_get(p, dest, num_bytes, operation)
function CB_get(p, dest, num_bytes, operation, timeout_ms) result(status) BIND(C, name='CB_get')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
type(C_PTR),       intent(IN), value :: p             !< pointer to a circular buffer
integer(C_SIZE_T), intent(IN), value :: num_bytes     !< number of bytes to extract
type(C_PTR),       intent(IN), value :: dest          !< destination array to receive extracted data
integer(C_INT),    intent(IN), value :: operation     !< Whether to update the OUT index, partially read, or peek
integer(C_INT),    intent(IN), value :: timeout_ms    !< How long to wait (in ms) before giving up and returning an error
integer(C_INT) :: status                              !< 0 if success, -1 if error
end function CB_get
!> wait until num_bytes are available then insert from src array<br>
!> n = CB_put(p, src, num_bytes, commit_transaction)
function CB_put(p, src, num_bytes, commit_transaction, timeout_ms, thread_safe) result(status) BIND(C,name='CB_put')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
type(C_PTR),       intent(IN), value :: p                  !< pointer to a circular buffer
integer(C_SIZE_T), intent(IN), value :: num_bytes          !< number of bytes to insert from src
type(C_PTR),       intent(IN), value :: src                !< source array for data insertion
integer(C_INT),    intent(IN), value :: commit_transaction !< Whether to make the inserted data available immediately (1) or not (0)
integer(C_INT),    intent(IN), value :: timeout_ms         !< How long to wait (in ms) before giving up and returning an error
integer(C_INT),    intent(IN), value :: thread_safe        !< Whether to perform the operation in a thread-safe manner (when == 1)
integer(C_INT) :: status                                   !< 0 if success, -1 if failure
end function CB_put
pure function CB_check_integrity(buffer) result(status) BIND(C, name = 'CB_check_integrity')
import C_INT, C_PTR
implicit none
type(C_PTR),    intent(in), value :: buffer
integer(C_INT) :: status
end function CB_check_integrity
pure function CB_error_code_to_string(error_code) result(error_string) BIND(C, name = 'CB_error_code_to_string')
import C_INT, C_PTR
implicit none
integer(C_INT), intent(in), value :: error_code
type(C_PTR) :: error_string
end function CB_error_code_to_string
subroutine CB_print_stats(buffer, buffer_id, with_header) BIND(C, name = 'CB_print_stats')
import C_INT, C_PTR
implicit none
type(C_PTR),    intent(in), value :: buffer
integer(C_INT), intent(in), value :: buffer_id
integer(C_INT), intent(in), value :: with_header
end subroutine CB_print_stats
end interface
