!/* useful routines for C and FORTRAN programming
! * Copyright (C) 2020  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This software is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This software is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! */
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

    procedure :: is_valid !< check if the circular buffer is properly created
    procedure :: print_header !< Print the buffer header (to help debugging)

    !> \return pointer to created circular buffer
    procedure :: create_local           !< create a circular buffer in local memory
    !> \return pointer to created circular buffer
    procedure :: create_shared          !< create a circular buffer in shared memory
    !> \return pointer to created circular buffer
    procedure :: create_from_pointer    !< create a circular buffer from user supplied memory
    !> \return pointer to created circular buffer
    procedure :: create_from_other      !< public creaator
    !> \return pointer to created circular buffer
    GENERIC   :: create => create_local, create_shared, create_from_pointer, create_from_other  !< generic create circular buffer
    !> \return 0 if success, -1 if error
    procedure :: detach_shared          !< detach shared memory segment used by circular buffer
    !> \return number of empty slots available, -1 on error
    procedure :: space_available        !< get number of empty slots available
    !> \return number of empty slots available, -1 on error
    procedure :: wait_space_available   !< wait until at least na empty slots
    !> \return number of empty slots available, -1 on error
    GENERIC   :: space => space_available, wait_space_available  !< generic wait for free space
    !> \return number of data tokens available, -1 if error
    procedure :: data_available         !< get current number of data tokens available
    !> \return number of data tokens available, -1 if error
    procedure :: wait_data_available    !< wait until at least na data tokens are available
    !> \return number of data tokens available, -1 if error
    GENERIC   :: data => data_available, wait_data_available     !< generic wait for available data
    !> \return address of the beginning of the buffer
    procedure :: buffer_start           !< get address of the beginning ot the buffer
    !> \return address of the insertion point
    procedure :: data_in                !< get address of the insertion point
    !> \return address of the extraction point
    procedure :: data_out               !< get address of the extraction point
    !> \return pointer to the "in" position
    procedure :: advance_in             !< get pointer to the in position, and space available
    !> \return pointer to the "out" position
    procedure :: advance_out            !< get pointer to the out position, and data available
    !> \return number of data tokens available after this operation, -1 if error
    procedure :: atomic_get             !< wait until enough data is available then extract data
    !> \return number of free slots available after this operation, -1 if error
    procedure :: atomic_put             !< wait until enough free slots are available then insert data

    procedure :: delete
  end type circular_buffer
contains

  function is_valid(cb)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    logical :: is_valid
    is_valid = c_associated(cb % p)
  end function is_valid

  subroutine print_header(cb)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    call circular_buffer_print_header(cb % p)
  end subroutine print_header

  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local(nwords)
  !> <br>p = cb\%create(nwords)
  function create_local(cb, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create(nwords)
    cb%is_owner = .true.
    cb%is_shared = .false.
    p = cb%p
  end function create_local

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared(shmid, nwords)
  !> <br>p = cb\%create(shmid, nwords)
  function create_shared(cb, shmid, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT) :: shmid                    !< identifier of shared memory area (see man shmget)
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create_shared(shmid, nwords)
    cb%is_owner = .false.
    cb%is_shared = .true.
    p = cb%p
  end function create_shared

  !> \brief create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer(ptr, nwords)
  !> <br>p = cb\%create(ptr, nwords)
  function create_from_pointer(cb, ptr, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR), intent(IN), value :: ptr                   !< pointer to user supplied memory
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_from_pointer(ptr, nwords)
    cb%is_owner = .false.
    cb%is_shared = .false.
    p = cb%p
  end function create_from_pointer

  !> \brief create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(cb, ptr) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR), intent(IN), value :: ptr                   !< pointer to user supplied memory
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = ptr
    cb%is_owner = .false.
    cb%is_shared = .false.
    p = cb%p
  end function create_from_other
  
  !> \brief detach shared memory segment used by circular buffer 
  !> <br>type(circular_buffer) :: cb<br>integer :: status<br>
  !> status = cb\%detach_shared()
  function detach_shared(cb) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: status                                !< 0 if success, -1 if error
    status = circular_buffer_detach_shared(cb%p)
  end function detach_shared

  !> \brief get number of empty slots available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%space_available()
  !> <br>n = cb\%space()
  function space_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of empty slots available, -1 if error
    n = circular_buffer_get_available_space(cb%p)
  end function space_available

  !> \brief wait until at least na empty slots are available for inserting data
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%wait_space_available(na)
  !> <br>n = cb\%space(na)
  function wait_space_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available slots
    integer(C_INT) :: n                                     !< number of empty slots available, -1 on error
    n = circular_buffer_wait_space_available(cb%p, na)
  end function wait_space_available

  !> \brief get current number of data tokens available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%data_available()
  !> <br>n = cb\%data()
  function data_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = circular_buffer_get_available_data(cb%p)
  end function data_available

  !> \brief wait until at least na data tokens are available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%wait_data_available(na)
  !> <br>n = cb\%data(na)
  function wait_data_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available tokens
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = circular_buffer_wait_data_available(cb%p, na)
  end function wait_data_available

  !> \brief get address of the beginning of the buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: start<br>
  !> start = cb\%buffer_start()
  function buffer_start(cb) result(start)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: start                                    !< address of the start of the circular data buffer
    start = circular_buffer_start(cb%p)
  end function buffer_start

  !> \brief get address of the insertion point
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: inp<br>
  !> inp = cb\%data_in()
  function data_in(cb) result(inp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: inp                                      !< address of the insertion point in the circular data buffer
    inp = circular_buffer_data_in(cb%p)
  end function data_in

  !> \brief get address of the extraction point
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: outp<br>
  !> outp = cb\%data_out()
  function data_out(cb) result(outp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: outp                                     !< address of the extraction point in the circular data buffer
    outp = circular_buffer_data_out(cb%p)
  end function data_out

  !> \brief get pointer to the in position
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: inp<br>
  !> inp = cb\%advance_in(n1, n2)
  function advance_in(cb, n1, n2) result(inp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of slots available at the "in" position, -1 if error
    integer(C_INT), intent(OUT)    :: n2                    !< number of slots available at the "start" of the buffer, -1 if error
    type(C_PTR)                    :: inp                   !< pointer to the "in" position, C_NULL_PTR if error
    inp = circular_buffer_advance_in(cb%p, n1, n2)
  end function advance_in

  !> \brief get pointer to the "out" position
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: outp<br>
  !> outp = cb\%advance_out(n1, n2)
  function advance_out(cb, n1, n2) result(outp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of tokens available at the "out" position, -1 if error
    integer(C_INT), intent(OUT)    :: n2                    !< number of tokens available at the "start" of the buffer, -1 if error
    type(C_PTR)                    :: outp                  !< pointer to the "out" position, C_NULL_PTR if error
    outp = circular_buffer_advance_out(cb%p, n1, n2)
  end function advance_out

  !> \brief wait until ndst tokens are available then extract them into dst
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_get(dst, ndst)
  function atomic_get(cb, dst, ndst) result(n)
    implicit none
    class(circular_buffer), intent(INOUT)            :: cb    !< circular_buffer
    integer(C_INT), intent(IN), value                :: ndst  !< number of tokens to extract
    integer(DATA_ELEMENT), dimension(*), intent(OUT) :: dst   !< destination array to receive extracted data
    integer(C_INT) :: n                                     !< number of data tokens available after this operation, -1 if error
    n = circular_buffer_atomic_get(cb%p, dst, ndst)
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
    n = circular_buffer_atomic_put(cb%p, src, nsrc)
  end function atomic_put

  function delete(cb) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb
    logical :: status

    status = .true.
    if (cb%is_owner) then
      call free(cb%p)
    else if (cb%is_shared) then
      status = circular_buffer_detach_shared(cb%p)
    end if
  end function delete

end module circular_buffer_module
