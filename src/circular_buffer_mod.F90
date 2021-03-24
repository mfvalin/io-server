! Copyright (C) 2021  Environnement Canada
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
  implicit none
  include 'io-server/circular_buffer.inc'

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

    procedure :: get_available_space !< Get number of empty slots available. \return Number of slot, -1 on error
    procedure :: get_available_data  !< Get current number of data elements available. \return Number of elements stored, -1 on error
    procedure :: atomic_get !< Wait until enough data is available then extract it. \return Number of elements left after operation, -1 on error
    procedure :: atomic_put !< Wait until enough free slots are available then insert data. \return Number of free slots after operation, -1 on error

    procedure :: is_valid !< Check whether the circular buffer is properly created. \return Whether the C pointer is associated
    procedure :: print_header !< Print the buffer header (to help debugging)

  end type circular_buffer

contains

  function is_valid(cb)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    logical :: is_valid
    is_valid = c_associated(cb % p)
    if (is_valid) is_valid = (CB_check_integrity(cb % p) == 0)
  end function is_valid

  subroutine print_header(cb)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    call CB_print_header(cb % p)
  end subroutine print_header

  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local(nwords)
  !> <br>p = cb\%create(nwords)
  function create_local(cb, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb     !< circular_buffer
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success                              !< Whether the operation was successful
    cb%p = CB_create(nwords)
    cb%is_owner = .true.
    cb%is_shared = .false.
    success = cb % is_valid()
  end function create_local

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared(shmid, nwords)
  !> <br>p = cb\%create(shmid, nwords)
  function create_shared(cb, shmid, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb     !< circular_buffer
    integer(C_INT), intent(OUT)           :: shmid  !< identifier of shared memory area (see man shmget)
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success
    cb%p = CB_create_shared(shmid, nwords)
    cb%is_owner = .false.
    cb%is_shared = .true.
    success = cb%is_valid()
  end function create_shared

  !> \brief create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer(ptr, nwords)
  !> <br>p = cb\%create(ptr, nwords)
  function create_from_pointer(cb, ptr, nwords) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb     !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr    !< pointer to user supplied memory
    integer(C_INT), intent(IN), value     :: nwords !< size in 32 bit elements of the circular buffer
    logical :: success                              !< Whether the operation was successful
    cb%p = CB_from_pointer(ptr, nwords)
    cb%is_owner = .false.
    cb%is_shared = .false.
    success = cb%is_valid()
  end function create_from_pointer

  !> \brief create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(cb, ptr) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb   !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr  !< pointer to user supplied memory
    logical :: success                            !< Whether the given pointer was OK
    cb%p = ptr
    cb%is_owner = .false.
    cb%is_shared = .false.
    success = cb%is_valid()
  end function create_from_other
  
  !> \brief get number of empty slots available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%get_available_space()
  !> <br>n = cb\%space()
  function get_available_space(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of empty slots available, -1 if error
    n = CB_get_available_space(cb%p)
  end function get_available_space

  !> \brief get current number of data tokens available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%get_available_data()
  !> <br>n = cb\%data()
  function get_available_data(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = CB_get_available_data(cb%p)
  end function get_available_data

  !> \brief wait until ndst tokens are available then extract them into dst
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_get(dst, ndst)
  function atomic_get(cb, dst, ndst) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)            :: cb    !< circular_buffer
    integer(C_INT), intent(IN), value                :: ndst  !< number of tokens to extract
    integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
    integer(C_INT) :: n                                     !< number of data tokens available after this operation, -1 if error
    n = CB_atomic_get(cb%p, dst, ndst)
  end function atomic_get

  !> \brief wait until nsrc free slots are available then insert from src array
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_put(src, nsrc)
  function atomic_put(cb, src, nsrc) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)           :: cb    !< circular_buffer
    integer(C_INT), intent(IN), value               :: nsrc  !< number of tokens to insert from src
    integer(DATA_ELEMENT), dimension(*), intent(IN) :: src   !< source array for data insertion
    integer(C_INT) :: n                                    !< number of free slots available after this operation, -1 if error
    n = CB_atomic_put(cb%p, src, nsrc)
  end function atomic_put

  function delete(cb) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    logical :: status

    status = .true.
    if (cb%is_owner) then
      call free_c_ptr(cb%p)
    else if (cb%is_shared) then
      status = CB_detach_shared(cb%p) .ge. 0
    end if
    cb % p = C_NULL_PTR
  end function delete

end module circular_buffer_module
