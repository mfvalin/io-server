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

  public :: CB_KIND_CHAR, CB_KIND_INTEGER_4, CB_KIND_INTEGER_8, CB_KIND_REAL_4, CB_KIND_REAL_8, num_char_to_num_int, CB_DATA_ELEMENT, CB_DATA_ELEMENT_KIND

  !> \brief User-defined type for a one-producer, one-consumer circular queue. See #circular_buffer
  type, public :: circular_buffer
    !> \private
    private
    type(C_PTR) :: p = C_NULL_PTR       !< Pointer to internal struct of the #circular_buffer
    logical :: is_shared = .false.    !< Whether the circular_buffer is stored in shared memory
    logical :: is_owner  = .false.    !< Whether the circular_buffer owns the memory where it resides (i.e. whether it is responsible for freeing it)
  contains

    procedure :: create_local_bytes         !< circular_buffer_module::create_local_bytes
    procedure :: create_shared_bytes        !< circular_buffer_module::create_shared_bytes
    procedure :: create_from_pointer_bytes  !< circular_buffer_module::create_from_pointer_bytes
    procedure :: create_from_other          !< circular_buffer_module::create_from_other
    GENERIC   :: create_bytes => create_local_bytes, create_shared_bytes, create_from_pointer_bytes, create_from_other  !< generic create circular buffer
    procedure :: delete !< circular_buffer_module::delete

    procedure :: get_num_spaces   !< circular_buffer_module::get_num_spaces
    procedure :: get_num_elements !< circular_buffer_module::get_num_elements
    procedure :: get_capacity     !< circular_buffer_module::get_capacity
    procedure :: peek             !< circular_buffer_module::peek
    procedure :: get              !< circular_buffer_module::get
    procedure :: put              !< circular_buffer_module::put

    procedure :: is_valid     !< circular_buffer_module::is_valid
    procedure :: print_header !< Print the buffer header (to help debugging). circular_buffer_module::print_header
    procedure :: print_stats  !< Print stats collected during the buffer's lifetime. circular_buffer_module::print_stats

  end type circular_buffer

contains

  !> Check integrity of the circular buffer: the pointer is valid and the integrity check on the underlying C struct passes.
  !> \sa CB_check_integrity
  !> \return Wether the circular buffer passes all checks
  function is_valid(this)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    logical :: is_valid
    is_valid = c_associated(this % p)
    if (is_valid) then
      is_valid = (CB_check_integrity(this % p) == 0)
    else
      print *, 'ERROR: CB pointer is not even associated'
    end if
  end function is_valid

  !> Print the C struct header of this circular buffer. See CB_print_header()
  !> \sa CB_print_header()
  subroutine print_header(this)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    call CB_print_header(this % p)
  end subroutine print_header

  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local_bytes(num_bytes)
  !> <br>p = cb\%create(num_bytes)
  !> \sa CB_create_bytes()
  function create_local_bytes(this, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this      !< circular_buffer
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in bytes of the circular buffer
    logical :: success                                     !< Whether the created buffer is valid
    this % p = CB_create_bytes(num_bytes)
    this % is_owner = .true.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_local_bytes

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared_bytes(shmid, num_bytes)
  !> <br>p = cb\%create(shmid, num_bytes)
  !> \sa CB_create_shared_bytes()
  function create_shared_bytes(this, shmid, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this   !< circular_buffer
    integer(C_INT),         intent(OUT)       :: shmid  !< identifier of shared memory area (see man shmget)
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success                                  !< Whether the created buffer is valid
    this % p = CB_create_shared_bytes(shmid, num_bytes)
    this % is_owner = .false.
    this % is_shared = .true.
    success = this % is_valid()
  end function create_shared_bytes

  !> \brief Create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer_bytes(ptr, num_bytes)
  !> <br>p = cb\%create(ptr, num_bytes)
  !> \sa CB_from_pointer_bytes
  function create_from_pointer_bytes(this, ptr, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this      !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr       !< pointer to user supplied memory
    integer(C_SIZE_T), intent(IN), value  :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success                                 !< Whether the created buffer is valid
    this % p = CB_from_pointer_bytes(ptr, num_bytes)
    this % is_owner = .false.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_from_pointer_bytes

  !> \brief Create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(this, ptr) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr  !< pointer to user supplied memory
    logical :: success                            !< Whether the resulting buffer is valid
    this % p = ptr
    this % is_owner = .false.
    this % is_shared = .false.
    success = this % is_valid()
  end function create_from_other
  
  !> \brief Get number of empty element slots available in the buffer
  !> num_integers = cb\%get_num_spaces(CB_KIND_INTEGER_4)
  !> \sa CB_get_available_space_bytes()
  function get_num_spaces(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this     !< circular_buffer
    integer, intent(IN)                   :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements                !< Number of empty slots available, -1 if error

    integer            :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size    = get_type_size(type_id)
    num_bytes    = CB_get_available_space_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_spaces

  !> \brief Get current number of data elements from the given type stored in the buffer
  !> <br> num_reals = cb\%get_num_elements(CB_KIND_REAL_8)
  !> \sa CB_get_available_data_bytes()
  function get_num_elements(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this     !< circular_buffer
    integer, intent(IN)                   :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements                !< Number of (full) data elements stored, -1 if error

    integer :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size = get_type_size(type_id)
    num_bytes = CB_get_available_data_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_elements

  !> \brief Get max number of elements this buffer can hold
  !> \sa CB_get_capacity_bytes()
  function get_capacity(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this         !< The circular buffer
    integer,                intent(IN)    :: type_id      !< ID of the type of elements we want to fit
    integer(C_INT64_T)                    :: num_elements !< Max number of elements that can fit in the buffer

    num_elements = CB_get_capacity_bytes(this % p) / get_type_size(type_id)
  end function get_capacity

  !> \brief Look at the next elements in this buffer without extracting them
  !> \return .true. if peeking was successful, .false. otherwise
  !> \sa CB_get()
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
  !> success = cb\%get(dest, num_elements, type_id, commit_transaction)
  !> \sa CB_get()
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
  !> \sa CB_put()
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

  !> Delete the buffer by disassociating from the underlying C pointer and, if applicable, releasing the memory
  !> \return .true. if there were no issues, .false. if detaching the shared memory failed
  !> \sa CB_detach_shared
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

  !> Print usage statistics collected during the lifetime of the buffer
  !> \sa CB_print_stats
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
