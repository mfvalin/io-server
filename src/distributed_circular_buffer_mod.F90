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
!> \brief distributed circular buffer object Fortran module
module distributed_circular_buffer_module
  use ISO_C_BINDING
  implicit none
  include 'io-server/distributed_circular_buffer.inc'

  private

  !> A set of FIFO queues used by multiple pairs of processes, with their data stored on a single one of these processes
  type, public :: distributed_circular_buffer
    private
    type(C_PTR) :: c_buffer = C_NULL_PTR !< Pointer to the C struct containing all distributed circular buffer info
  contains
    procedure :: is_valid !< distributed_circular_buffer_module::is_valid
    procedure :: create   !< distributed_circular_buffer_module::create
    procedure :: delete   !< distributed_circular_buffer_module::delete
    procedure :: print    !< distributed_circular_buffer_module::print
    procedure :: put      !< distributed_circular_buffer_module::put
    procedure :: get      !< distributed_circular_buffer_module::get
    GENERIC :: get_num_elements => get_num_elements_local, get_num_elements_latest
    procedure :: get_num_elements_local
    procedure :: get_num_elements_latest
!    procedure :: get_num_spaces
  end type distributed_circular_buffer

contains

  !> Check if the buffer is valid: it has been successfully created and has not yet been destroyed
  function is_valid(this)
    implicit none
    class(distributed_circular_buffer), intent(in) :: this
    logical :: is_valid !< Whether this buffer is usable

    is_valid = c_associated(this % c_buffer)
  end function is_valid

  !> Create and initialize a distributed circular buffer. See DCB_create
  function create(this, communicator, num_producers, num_words) result(is_valid)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT), intent(in)                        :: communicator
    integer(C_INT), intent(in)                        :: num_producers
    integer(C_INT), intent(in)                        :: num_words
    logical :: is_valid !< .true. if the creation was a success, .false. otherwise

    if (this % is_valid()) then
      call this % delete()
    end if

    this % c_buffer = DCB_create(communicator, num_producers, num_words)
    is_valid = this % is_valid()
  end function create

  !> Free the memory used by a distributed circular buffer. See DCB_delete
  subroutine delete(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this

    if (this % is_valid()) then
      call DCB_delete(this % c_buffer)
    end if

    this % c_buffer = C_NULL_PTR
  end subroutine delete

  !> Print info about a distributed circular buffer. See DCB_print
  subroutine print(this)
    implicit none
    class(distributed_circular_buffer), intent(in)  :: this

    call DCB_print(this % c_buffer)
  end subroutine print

  !> Insert elements into a distributed circular buffer. See DCB_put
  function put(this, src_data, num_elements) result(num_space_available)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(DATA_ELEMENT), dimension(*), intent(in)     :: src_data
    integer(C_INT), intent(in)                          :: num_elements
    integer(C_INT) :: num_space_available !< The return value of DCB_put

    num_space_available = DCB_put(this % c_buffer, src_data, num_elements)
  end function put

  !> Extract elements from a distributed circular buffer. See DCB_get
  function get(this, buffer_id, dest_data, num_elements) result(num_data_available)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(C_INT), intent(in)                          :: buffer_id
    integer(DATA_ELEMENT), dimension(*), intent(inout)  :: dest_data
    integer(C_INT), intent(in)                          :: num_elements
    integer(C_INT) :: num_data_available !< The return value of DCB_get

    num_data_available = DCB_get(this % c_buffer, buffer_id, dest_data, num_elements)
  end function get

  function get_num_elements_local(this, buffer_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT), intent(in)                        :: buffer_id
    integer(C_INT) :: num_elements

    num_elements = DCB_get_num_elements(this % c_buffer, buffer_id)
  end function get_num_elements_local

  function get_num_elements_latest(this) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_elements

    num_elements = DCB_get_latest_num_elements(this % c_buffer)
  end function get_num_elements_latest

end module distributed_circular_buffer_module
