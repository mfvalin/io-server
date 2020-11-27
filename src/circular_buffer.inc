
!> \file
!> \brief circular buffer package (C and Fortran)
!>
!> code extracted from circular_buffer.c
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


include 'common.inc'
interface


!> initialize a circular buffer<br>
!> buffer = circular_buffer_init(p, nwords)
function circular_buffer_init(p, nwords) result(buffer) bind(C,name='circular_buffer_init')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p           !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nwords   !< the size in elements of the circular buffer
type(C_PTR) :: buffer                         !< pointer(C_PTR) to buffer upon success, C_NULL_PTR upon error
end function circular_buffer_init


!> create and initialize a circular buffer of size nwords in "shared memory"<br>
!> p = circular_buffer_create_shared(shmid, nwords)
function circular_buffer_create_shared(shmid, nwords) result(p) BIND(C,name='circular_buffer_create_shared')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(OUT) :: shmid          !< identifier of shared memory area (see man shmget) (-1 upon error)
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function circular_buffer_create_shared


!> detach "shared memory segment" used by circular buffer <br>
!> status = circular_buffer_detach_shared(p)
function circular_buffer_detach_shared(p) result(status) BIND(C,name='circular_buffer_detach_shared')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT) :: status               !< 0 upon success, -1 upon error
end function circular_buffer_detach_shared


!> create and initialize a circular buffer of size nwords in process memory<br>
!> p = circular_buffer_create(nwords)
function circular_buffer_create(nwords) result(p) BIND(C,name='circular_buffer_create')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function circular_buffer_create


!> create and initialize a circular buffer of size nwords from user supplied memory<br>
!> p = circular_buffer_from_pointer(ptr, nwords)
function circular_buffer_from_pointer(ptr, nwords) result(p) BIND(C,name='circular_buffer_from_pointer')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: ptr         !< pointer to user supplied memory
integer(C_INT), intent(IN), value :: nwords   !< size in 32 bit elements of the circular buffer
type(C_PTR) :: p                              !< pointer to created circular buffer
end function circular_buffer_from_pointer


!> return the current number of empty slots available<br>
!> n = circular_buffer_space_available(p)
function circular_buffer_space_available(p) result(n) BIND(C,name='circular_buffer_space_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT) :: n                    !< current number of empty slots available, -1 if error
end function circular_buffer_space_available


!> wait until at least na empty slots are available for inserting data<br>
!> n = circular_buffer_wait_space_available(p, na)
function circular_buffer_wait_space_available(p, na) result(n) BIND(C,name='circular_buffer_wait_space_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p          !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: na      !< needed number of available slots
integer(C_INT) :: n                          !< actual number of empty slots available, -1 on error
end function circular_buffer_wait_space_available


!> get the current number of data tokens available<br>
!> p = circular_buffer_data_available(p)
function circular_buffer_data_available(p) result(n) BIND(C,name='circular_buffer_data_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer
integer(C_INT) :: n                            !< current number of data tokens available, -1 if error
end function circular_buffer_data_available


!> wait until at least n data tokens are available for extracting data<br>
!> p = circular_buffer_wait_data_available(p, na)
function circular_buffer_wait_data_available(p, na) result(n) BIND(C,name='circular_buffer_wait_data_available')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p            !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: na        !< needed number of available tokens
integer(C_INT) :: n                            !< actual number of data tokens available, -1 if error
end function circular_buffer_wait_data_available


!> get the address of the first position in the circular data buffer<br>
!> start = circular_buffer_start(p)
function circular_buffer_start(p) result(start) BIND(C,name='circular_buffer_start')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: start                   !< pointer to beginning of circular buffer
end function circular_buffer_start


!> get the address of the  insertion point in the circular data buffer (data snoop)<br>
!> inp = circular_buffer_data_in(p)
function circular_buffer_data_in(p) result(inp) BIND(C,name='circular_buffer_data_in')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: inp                     !< address of the  insertion point in the circular data buffer
end function circular_buffer_data_in


!> get the address of the extraction point in the circular data buffer (data snoop)<br>
!> outp = circular_buffer_data_out(p)
function circular_buffer_data_out(p) result(outp) BIND(C,name='circular_buffer_data_out')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
type(C_PTR) :: outp                    !< address of the extraction point in the circular data buffer
end function circular_buffer_data_out


!> get pointer to the in position, assume that the caller knows the start of data buffer<br>
!> inp = circular_buffer_advance_in(p, n1, n2)
function circular_buffer_advance_in(p, n1, n2) result(inp) BIND(C,name='circular_buffer_advance_in')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT), intent(OUT)    :: n1   !< number of slots available at the "in" position, -1 upon error
integer(C_INT), intent(OUT)    :: n2   !< number of slots available at the "start" of the buffer, -1 upon error
type(C_PTR)                    :: inp  !< pointer to the "in" position, C_NULL_PTR upon error
end function circular_buffer_advance_in


!> get pointer to the "out" position, assume that the caller knows the start of data buffer<br>
!> outp = circular_buffer_advance_out(p, n1, n2)
function circular_buffer_advance_out(p, n1, n2) result(outp) BIND(C,name='circular_buffer_advance_out')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: p    !< pointer to a circular buffer
integer(C_INT), intent(OUT)    :: n1   !< number of tokens available at the "out" position, -1 upon error
integer(C_INT), intent(OUT)    :: n2   !< number of tokens available at the "start" of the buffer, -1 upon error
type(C_PTR)                    :: outp !< pointer to the "out" position, C_NULL_PTR upon error
end function circular_buffer_advance_out


!> wait until ndst tokens are available then extract them into dst<br>
!> n = circular_buffer_atomic_get(p, dst, ndst)
function circular_buffer_atomic_get(p, dst, ndst) result(n) BIND(C,name='circular_buffer_atomic_get')
import :: C_PTR, C_INT, CB_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: ndst          !< number of tokens to extract
integer(CB_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
integer(C_INT) :: n                                !< number of data tokens available after this operation, -1 if error
end function circular_buffer_atomic_get


!> wait until ndst tokens are available then copy to dst array<br>
!> DO NOT UPDATE "out" unless update flag is non zero<br>
!> n = circular_buffer_extract(p, dst, ndst, offset, update)
function circular_buffer_extract(p, dst, ndst, offset, update) result(n) BIND(C,name='circular_buffer_extract')
import :: C_PTR, C_INT, CB_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: ndst          !< number of tokens to copy to dst
integer(CB_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array for data extraction
integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
integer(C_INT) :: n                                !< number of free slots available after this operation
end function circular_buffer_extract


!> wait until nsrc free slots are available then insert from src array<br>
!> n = circular_buffer_atomic_put(p, src, nsrc)
function circular_buffer_atomic_put(p, src, nsrc) result(n) BIND(C,name='circular_buffer_atomic_put')
import :: C_PTR, C_INT, CB_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
integer(CB_ELEMENT), dimension(*), intent(IN) :: src    !< source array for data insertion
integer(C_INT) :: n                                !< number of free slots available after this operation
end function circular_buffer_atomic_put


!> wait until nsrc free slots are available then insert from src array<br>
!> DO NOT UPDATE the "in" pointer unless update flag is non zero<br>
!> n = circular_buffer_insert(p, src, nsrc, offset, update)
function circular_buffer_insert(p, src, nsrc, offset, update) result(n) BIND(C,name='circular_buffer_insert')
import :: C_PTR, C_INT, CB_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: p                !< pointer to a circular buffer
integer(C_INT), intent(IN), value :: nsrc          !< number of tokens to insert from src
integer(CB_ELEMENT), dimension(*), intent(IN) :: src    !< source array for data insertion
integer(C_INT), intent(IN), value :: offset        !< offset from the "in" position
integer(C_INT), intent(IN), value :: update        !< if nonzero, update the "in" pointer
integer(C_INT) :: n                                !< number of free slots available after this operation
end function circular_buffer_insert


end interface

