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

  private

  public :: CB_KIND_CHAR, CB_KIND_INTEGER_4, CB_KIND_INTEGER_8, CB_KIND_REAL_4, CB_KIND_REAL_8, num_char_to_num_int

  !> \brief circular_buffer user defined type
  type, public :: circular_buffer
    !> \private
    private
    type(C_PTR) :: p = C_NULL_PTR       !< pointer to storage used by circular buffer
    logical :: is_shared = .false.
    logical :: is_owner  = .false.
  contains

    procedure :: create_local_bytes  !< Create a circular buffer in local memory. \return Whether the buffer was successfully created
    procedure :: create_shared_bytes       !< Create a circular buffer in shared memory. \return Whether the buffer was successfully created
    procedure :: create_from_pointer_bytes !< Create a circular buffer from user supplied memory. \return Whether the pointer is valid
    procedure :: create_from_other   !< Consider the given pointer as a circular buffer. \return Whether the pointer is valid
    !> \return pointer to created circular buffer
    GENERIC   :: create_bytes => create_local_bytes, create_shared_bytes, create_from_pointer_bytes, create_from_other  !< generic create circular buffer
    procedure :: delete !< Free memory used by the buffer, if appropriate

    procedure :: get_num_spaces    !< Get number of empty slots available. \return Number of slot, negative number on error
    procedure :: get_num_elements  !< Get current number of elements available. \return Number of elements stored, negative number on error
    procedure :: get_capacity      !< \return Maximum number of data elements that can be stored in the buffer
    procedure :: peek !< Wait until enough data is available and just peek at it. \return .true. for success, .false. for failure
    procedure :: get  !< Wait until enough data is available then extract it. \return .true. for success, .false. for failure
    procedure :: put  !< Wait until enough free slots are available then insert data. \return .true. for success, .false. for failure

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
  !> p = cb\%create_local_bytes(num_bytes)
  !> <br>p = cb\%create(num_bytes)
  function create_local_bytes(this, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this      !< circular_buffer
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in bytes of the circular buffer
    logical :: success                                     !< Whether the operation was successful
    this % p = CB_create_bytes(num_bytes)
    this % is_owner = .true.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_local_bytes

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared_bytes(shmid, num_bytes)
  !> <br>p = cb\%create(shmid, num_bytes)
  function create_shared_bytes(this, shmid, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this   !< circular_buffer
    integer(C_INT),         intent(OUT)       :: shmid  !< identifier of shared memory area (see man shmget)
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success
    this % p = CB_create_shared_bytes(shmid, num_bytes)
    this % is_owner = .false.
    this % is_shared = .true.
    success = this % is_valid()
  end function create_shared_bytes

  !> \brief create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer_bytes(ptr, num_bytes)
  !> <br>p = cb\%create(ptr, num_bytes)
  function create_from_pointer_bytes(this, ptr, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this      !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr       !< pointer to user supplied memory
    integer(C_SIZE_T), intent(IN), value  :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success                                 !< Whether the operation was successful
    this % p = CB_from_pointer_bytes(ptr, num_bytes)
    this % is_owner = .false.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_from_pointer_bytes

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
  !> num_integers = cb\%get_num_spaces(CB_KIND_INTEGER_4)
  function get_num_spaces(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this     !< circular_buffer
    integer, intent(IN)                   :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements                 !< number of empty slots available, -1 if error

    integer            :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size    = get_type_size(type_id)
    num_bytes    = CB_get_available_space_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_spaces

  !> \brief get current number of data tokens available
  !> num_reals = cb\%get_num_elements(CB_KIND_REAL_8)
  function get_num_elements(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this     !< circular_buffer
    integer, intent(IN)                   :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements                !< number of data tokens available, -1 if error

    integer :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size = get_type_size(type_id)
    num_bytes = CB_get_available_data_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_elements

  !> \brief Get max number of elements this buffer can hold
  function get_capacity(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this         !< The circular buffer
    integer,                intent(IN)    :: type_id      !< ID of the type of elements we want to fit
    integer(C_INT64_T)                    :: num_elements !< max number of elements that can fit

    num_elements = CB_get_capacity_bytes(this % p) / get_type_size(type_id)
  end function get_capacity

  !> \brief Look at the next elements in this buffer without extracting them
  !> \return .true. if peeking was successful, .false. otherwise
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
  function peek(this, dest, num_elements, type_id) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this         !< The circular_buffer
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements !< How many elements we want to look at
    integer,                intent(IN), value :: type_id      !< ID of the type of elements we are looking for
    logical :: success

    type(C_PTR)    :: temp
    integer        :: type_size
    integer(C_INT) :: status

    success   = .false.
    temp      = C_LOC(dest)
    type_size = get_type_size(type_id)

    status = CB_get(this % p, temp, num_elements * type_size, CB_PEEK)
    if (status == 0) success = .true.
  end function peek

  !> \brief Wait until num_elements (of type type_id) are available then extract them into dest
  !> success = cb\%atomic_get(dest, num_elements, type_id, commit_transaction)
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
  function get(this, dest, num_elements, type_id, commit_transaction) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this               !< circular_buffer
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of elements to extract
    integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
    logical,                intent(IN), value :: commit_transaction !< Whether to update the buffer (ie _extract_ the data)
    logical :: success                                              !< Whether the operation was successful

    integer        :: type_size
    integer(C_INT) :: operation
    integer(C_INT) :: status
    type(C_PTR)    :: temp

    success   = .false.
    temp      = C_LOC(dest)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    status = CB_get(this % p, temp, num_elements * type_size, operation)
    if (status == 0) success = .true.
  end function get

  !> \brief Wait until num_elements of type type_id are available, then insert from src array
  !> success = cb\%put(src, num_elements, type_id, commit_transaction)
#define IgnoreTypeKindRank src
#define ExtraAttributes , target
  function put(this, src, num_elements, type_id, commit_transaction) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this               !< circular_buffer
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of tokens to insert from src
    integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
    logical,                intent(IN), value :: commit_transaction !< Whether to make the inserted data immediately available
    logical :: success                                              !< Whether the operation was successful

    integer        :: type_size
    integer(C_INT) :: operation
    integer(C_INT) :: status
    type(C_PTR)    :: temp

    success   = .false.
    temp      = C_LOC(src)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    status = CB_put(this % p, temp, num_elements * type_size, operation)
    if (status == 0) success = .true.
  end function put

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
    class(circular_buffer), intent(INOUT) :: this        !< The CB we want to print
    integer(C_INT), intent(IN), value     :: buffer_id   !< ID of the buffer (will be printed at the start of the data line)
    logical,        intent(IN), value     :: with_header !< Whether to print a line describing the data

    integer(C_INT) :: with_header_c
    with_header_c = 0
    if (with_header) with_header_c = 1

    if (this % is_valid()) call CB_print_stats(this % p, buffer_id, with_header_c)
  end subroutine print_stats

end module circular_buffer_module
