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
module circular_buffers
  use ISO_C_BINDING
  implicit none
  include 'circular_buffer.inc'
  !> \brief circular_buffer user defined type
  type :: circular_buffer
    type(C_PTR) :: p                    !< pointer to storage used by circular buffer
  contains
    procedure :: init                   !< initialize a circular buffer
    procedure :: create_local           !< create a circular buffer in local memory
    procedure :: create_shared          !< create a circular buffer in shared memory
    procedure :: create_from_pointer    !< create a circular buffer from user supplied memory
    GENERIC   :: create => create_local, create_shared, create_from_pointer  !< generic create
    procedure :: detach_shared          !< detach shared memory segment used by circular buffer
    procedure :: space_available        !< get number of empty slots available
    procedure :: wait_space_available   !< wait until at least na empty slots
    GENERIC   :: wait_space => space_available, wait_space_available  !< generic wait for space
    procedure :: data_available         !< get current number of data tokens available
    procedure :: wait_data_available    !< wait until at least na data tokens are available
    GENERIC   :: wait_data => data_available, wait_data_available     !< generic wait for data
    procedure :: buffer_start           !< get address of the beginning ot the buffer
    procedure :: data_in                !< get address of the insertion point
    procedure :: data_out               !< get address of the extraction point
    procedure :: advance_in             !< get pointer to the in position, and space available
    procedure :: advance_out            !< get pointer to the out position, and data available
    procedure :: atomic_get             !< wait until data is available then extract data
    procedure :: atomic_put             !< wait until space is available then insert data
  end type circular_buffer
contains
  !> \brief initialize a circular buffer
  function init(cb, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_init(cb%p, nwords)
    p = cb%p
  end function init 
  !> \brief create a circular buffer in local memory
  function create_local(cb, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create(nwords)
    p = cb%p
  end function create_local
  !> \brief create a circular buffer in shared memory
  function create_shared(cb, shmid, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT) :: shmid                    !< identifier of shared memory area (see man shmget)
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create_shared(shmid, nwords)
    p = cb%p
   end function create_shared
  !> \brief create a circular buffer from user supplied memory
  function create_from_pointer(cb, ptr, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR), intent(IN), value :: ptr                   !< pointer to user supplied memory
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_from_pointer(ptr, nwords)
    p = cb%p
  end function create_from_pointer
  !> \brief detach shared memory segment used by circular buffer 
  function detach_shared(cb) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: status                                !< 0 upon success, -1 upon error
    status = circular_buffer_detach_shared(cb%p)
  end function detach_shared
  !> \brief get number of empty slots available
  function space_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< current number of empty slots available, -1 if error
    n = circular_buffer_space_available(cb%p)
  end function space_available
  !> \brief wait until at least na empty slots are available for inserting data
  function wait_space_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available slots
    integer(C_INT) :: n                                     !< actual number of empty slots available, -1 on error
    n = circular_buffer_wait_space_available(cb%p, na)
  end function wait_space_available
  !> \brief get current number of data tokens available
  function data_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< current number of data tokens available, -1 if error
    n = circular_buffer_data_available(cb%p)
  end function data_available
  !> \brief wait until at least na data tokens are available
  function wait_data_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available tokens
    integer(C_INT) :: n                                     !< actual number of data tokens available, -1 if error
    n = circular_buffer_wait_data_available(cb%p, na)
  end function wait_data_available
  !> \brief get address of the beginning ot the buffer
  function buffer_start(cb) result(start)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: start                                    !< address of the start of the circular data buffer
    start = circular_buffer_start(cb%p)
  end function buffer_start
  !> \brief get address of the insertion point
  function data_in(cb) result(inp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: inp                                      !< address of the insertion point in the circular data buffer
    inp = circular_buffer_data_in(cb%p)
  end function data_in
  !> \brief get address of the extraction point
  function data_out(cb) result(outp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: outp                                     !< address of the extraction point in the circular data buffer
    outp = circular_buffer_data_out(cb%p)
  end function data_out
  !> \brief get pointer to the in position
  function advance_in(cb, n1, n2) result(inp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of slots available at the "in" position, -1 upon error
    integer(C_INT), intent(OUT)    :: n2                    !< number of slots available at the "start" of the buffer, -1 upon error
    type(C_PTR)                    :: inp                   !< pointer to the "in" position, C_NULL_PTR upon error
    inp = circular_buffer_advance_in(cb%p, n1, n2)
  end function advance_in
  !> \brief get pointer to the "out" position
  function advance_out(cb, n1, n2) result(outp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of tokens available at the "out" position, -1 upon error
    integer(C_INT), intent(OUT)    :: n2                    !< number of tokens available at the "start" of the buffer, -1 upon error
    type(C_PTR)                    :: outp                  !< pointer to the "out" position, C_NULL_PTR upon error
    outp = circular_buffer_advance_out(cb%p, n1, n2)
  end function advance_out
  !> \brief wait until ndst tokens are available then extract them into dst
  function atomic_get(cb, dst, ndst) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: ndst               !< number of tokens to extract
    integer(C_INT), dimension(*), intent(OUT) :: dst        !< destination array to receive extracted data
    integer(C_INT) :: n                                     !< number of data tokens available after this operation, -1 if error
    n = circular_buffer_atomic_get(cb%p, dst, ndst)
  end function atomic_get
  !> \brief wait until nsrc free slots are available then insert from src array
  function atomic_put(cb, src, nsrc) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nsrc               !< number of tokens to insert from src
    integer(C_INT), dimension(*), intent(IN) :: src         !< source array for data insertion
    integer(C_INT) :: n                                     !< number of free slots available after this operation
    n = circular_buffer_atomic_put(cb%p, src, nsrc)
  end function atomic_put

end module circular_buffers

#ifndef DOXYGEN_SHOULD_SKIP_THIS
program demo
  use circular_buffers
  implicit none

  type(circular_buffer) :: b, c, d, e, f
  integer :: shmid, n, status
  type(C_PTR) :: p, q, r, s, t
  integer, dimension(16000), target :: local

  p = b%create(128000)
  p = b%init(128000)
  s = c%create_shared(shmid, 128000)
  q = d%create(128000)                  ! generic call
  r = d%create(shmid, 128000)           ! generic call
  t = d%create(C_LOC(local), 128000)    ! generic call
  status = c%detach_shared()
  status = b%space_available()
  n = b%space_available()
  n = b%wait_space()
  n = b%wait_space_available(256)
  n = b%wait_space()                    ! generic call
  n = b%wait_space(256)                 ! generic call
  n = b%data_available()
  n = b%wait_data_available(256)
  n = b%wait_data()                     ! generic call
  n = b%wait_data(256)                  ! generic call
  
end program demo
#endif
