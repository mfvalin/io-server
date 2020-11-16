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
!> \brief Remote circular buffer object Fortran module
module remote_circular_buffers
  use ISO_C_BINDING
  implicit none
  include 'remote_circular_buffer.inc'

  type, public :: remote_circular_buffer
    private
    type(C_PTR) :: c_buffer = C_NULL_PTR
  contains
    procedure :: is_valid
    procedure :: create
    procedure :: delete
    procedure :: print
    procedure :: put
    procedure :: get
  end type remote_circular_buffer

contains

  function is_valid(this)
    implicit none
    class(remote_circular_buffer), intent(in) :: this
    logical :: is_valid

    is_valid = c_associated(this % c_buffer)
  end function is_valid

  function create(this, communicator, root, rank, comm_size, num_words) result(is_valid)
    implicit none
    class(remote_circular_buffer), intent(inout)  :: this
    integer(C_INT), intent(in)                    :: communicator
    integer(C_INT), intent(in)                    :: root
    integer(C_INT), intent(in)                    :: rank
    integer(C_INT), intent(in)                    :: comm_size
    integer(C_INT), intent(in)                    :: num_words
    logical :: is_valid

    if (this % is_valid()) then
      call this % delete()
    end if

    this % c_buffer = remote_circular_buffer_create(communicator, root, rank, comm_size, num_words)
    is_valid = this % is_valid()
  end function create

  subroutine delete(this)
    implicit none
    class(remote_circular_buffer), intent(inout)  :: this

    if (this % is_valid()) then
      call remote_circular_buffer_delete(this % c_buffer)
    end if

    this % c_buffer = C_NULL_PTR
  end subroutine delete

  subroutine print(this)
    implicit none
    class(remote_circular_buffer), intent(in) :: this

    call remote_circular_buffer_print(this % c_buffer)
  end subroutine print

  function put(this, src_data, num_elements) result(num_space_available)
    implicit none
    class(remote_circular_buffer), intent(inout)  :: this
    integer(C_INT), dimension(*), intent(in)      :: src_data
    integer(C_INT), intent(in)                    :: num_elements
    integer(C_INT) :: num_space_available

    num_space_available = remote_circular_buffer_put(this % c_buffer, src_data, num_elements)
  end function put

  function get(this, buffer_id, dest_data, num_elements) result(num_data_available)
    implicit none
    class(remote_circular_buffer), intent(inout)  :: this
    integer(C_INT), intent(in)                    :: buffer_id
    integer(C_INT), dimension(*), intent(inout)   :: dest_data
    integer(C_INT), intent(in)                    :: num_elements
    integer(C_INT) :: num_data_available

    num_data_available = remote_circular_buffer_get(this % c_buffer, buffer_id, dest_data, num_elements)
  end function get


end module remote_circular_buffers
