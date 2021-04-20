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

! ====================================================
!> \file
!> \brief circular buffer Fortran module (object oriented)
module circular_buffer_module
  use ISO_C_BINDING
  use cb_common_module
  use rpn_extra_module, only: free_c_ptr
  implicit none
  include 'io-server/circular_buffer.inc'

  public :: DATA_ELEMENT
  private

  !> \brief circular_buffer user defined type
  type, public :: circular_buffer
    !> \private
    private
    type(C_PTR) :: p = C_NULL_PTR       !< pointer to storage used by circular buffer
    logical :: is_shared = .false.
    logical :: is_owner  = .false.
  contains

    procedure :: create_local        !< Create a circular buffer in local memory. \return Whether the buffer was created
    procedure :: create_shared       !< Create a circular buffer in shared memory. \return Whether the buffer was created
    procedure :: create_from_pointer !< Create a circular buffer from user supplied memory. \return Whether the pointer is valid
    procedure :: create_from_other   !< Consider the given pointer as a circular buffer. \return Whether the pointer is valid
    !> \return pointer to created circular buffer
    GENERIC   :: create => create_local, create_shared, create_from_pointer, create_from_other  !< generic create circular buffer
    procedure :: delete !< Free memory used by the buffer, if appropriate

    procedure :: get_num_spaces !< Get number of empty slots available. \return Number of slot, -1 on error
    procedure :: get_num_elements  !< Get current number of data elements available. \return Number of elements stored, -1 on error
    procedure :: get_capacity        !< \return Maximum number of data elements that can be stored in the buffer
    procedure :: peek !< Wait until enough data is available and just peek at it. \return number of elements in the buffer, -1 on error
    procedure :: atomic_get !< Wait until enough data is available then extract it. \return Number of elements left after operation, -1 on error
    procedure :: atomic_put !< Wait until enough free slots are available then insert data. \return Number of free slots after operation, -1 on error

    procedure :: is_valid !< Check whether the circular buffer is properly created. \return Whether the C pointer is associated
    procedure :: print_header !< Print the buffer header (to help debugging)
    procedure :: print_stats !< Print stats collected during the buffer's lifetime

  end type circular_buffer

contains

  function is_valid(this)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    logical :: is_valid
    is_valid = c_associated(this % p)
    if (is_valid) then
      is_valid = (CB_check_integrity(this % p) == 0)
    else
      print *, 'ERROR, CB pointer is not even associated'
    end if
  end function is_valid

  subroutine print_header(this)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    call CB_print_header(this % p)
  end subroutine print_header

  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local(nwords)
  !> <br>p = cb\%create(nwords)
  function create_local(this, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this   !< circular_buffer
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success                              !< Whether the operation was successful
    this % p = CB_create(nwords)
    this % is_owner = .true.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_local

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared(shmid, nwords)
  !> <br>p = cb\%create(shmid, nwords)
  function create_shared(this, shmid, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this   !< circular_buffer
    integer(C_INT), intent(OUT)           :: shmid  !< identifier of shared memory area (see man shmget)
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success
    this % p = CB_create_shared(shmid, nwords)
    this % is_owner = .false.
    this % is_shared = .true.
    success = this % is_valid()
  end function create_shared

  !> \brief create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer(ptr, nwords)
  !> <br>p = cb\%create(ptr, nwords)
  function create_from_pointer(this, ptr, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this   !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr    !< pointer to user supplied memory
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success                              !< Whether the operation was successful
    this % p = CB_from_pointer(ptr, nwords)
    this % is_owner = .false.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_from_pointer

  !> \brief create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(this, ptr) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr  !< pointer to user supplied memory
    logical :: success                            !< Whether the given pointer was OK
    this % p = ptr
    this % is_owner = .false.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_from_other
  
  !> \brief get number of empty slots available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%get_num_spaces()
  !> <br>n = cb\%space()
  function get_num_spaces(this) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: this           !< circular_buffer
    integer(C_INT) :: n                                     !< number of empty slots available, -1 if error
    n = CB_get_available_space(this % p)
  end function get_num_spaces

  !> \brief get current number of data tokens available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%get_num_elements()
  !> <br>n = cb\%data()
  function get_num_elements(this) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: this           !< circular_buffer
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = CB_get_available_data(this % p)
  end function get_num_elements

  function get_capacity(this) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    integer(C_INT) :: num_elements !< max number of elements that can fit
    num_elements = CB_get_capacity(this % p)
  end function get_capacity

  function peek(this, dest, num_elements) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)            :: this !< The circular_buffer
    integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dest !< Destination array to receive the data
    integer(C_INT), intent(IN), value                :: num_elements !< How many elements we want to look at

    integer(C_INT) :: n !< Number of elements available after this operation, -1 if error

    n = CB_atomic_get(this % p, dest, num_elements, CB_PEEK)
  end function peek

  !> \brief wait until ndst tokens are available then extract them into dst
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_get(dst, ndst, commit_transaction)
  function atomic_get(this, dst, ndst, commit_transaction) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)            :: this  !< circular_buffer
    integer(C_INT), intent(IN), value                :: ndst  !< number of tokens to extract
    integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
    logical, intent(IN), value                       :: commit_transaction !< Whether to update the buffer (ie _extract_ the data)
    integer(C_INT) :: n                                     !< number of data tokens available after this operation, -1 if error

    integer(C_INT) :: operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT
    n = CB_atomic_get(this % p, dst, ndst, operation)
  end function atomic_get

  !> \brief wait until nsrc free slots are available then insert from src array
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_put(src, nsrc)
  function atomic_put(this, src, nsrc, commit_transaction) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)           :: this  !< circular_buffer
    integer(C_INT), intent(IN), value               :: nsrc  !< number of tokens to insert from src
    integer(DATA_ELEMENT), dimension(*), intent(IN) :: src   !< source array for data insertion
    logical, intent(IN), value                      :: commit_transaction !< Whether to make the inserted data immediately available
    integer(C_INT) :: n                                    !< number of free slots available after this operation, -1 if error

    integer(C_INT) :: operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT
    n = CB_atomic_put(this % p, src, nsrc, operation)
  end function atomic_put

  function delete(this) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    logical :: status

    status = .true.
    if (this % is_owner) then
      call free_c_ptr(this % p)
    else if (this % is_shared) then
      status = CB_detach_shared(this % p) .ge. 0
    end if
    this % p = C_NULL_PTR
  end function delete

  subroutine print_stats(this, buffer_id, with_header)
    implicit none
    class(circular_buffer), intent(INOUT) :: this !< The CB we want to print
    integer(C_INT), intent(IN), value     :: buffer_id !< ID of the buffer (will be printed at the start of the data line)
    logical,        intent(IN), value     :: with_header !< Whether to print a line describing the data

    integer(C_INT) :: with_header_c = -1

    if (with_header) then
      with_header_c = 1
    else
      with_header_c = 0
    end if

    !print *, 'with_header = ', with_header, 'with_header_c = ', with_header_c

    if (this % is_valid()) call CB_print_stats(this % p, buffer_id, with_header_c)
  end subroutine print_stats

end module circular_buffer_module
