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
!> \brief distributed circular buffer object Fortran module
module distributed_circular_buffer_module
  use ISO_C_BINDING
  use mpi_f08
  use cb_common_module
  implicit none
  include 'io-server/distributed_circular_buffer.inc'

  public :: DATA_ELEMENT
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
    procedure :: peek     !< distributed_circular_buffer_module::peek
    procedure :: get_num_elements
    procedure :: get_num_spaces
    procedure :: sync_window
    procedure :: get_producer_id
    procedure :: get_channel_id
    procedure :: get_consumer_id
    procedure :: get_num_producers
    procedure :: get_num_consumers
    GENERIC :: get_capacity => get_capacity_local, get_capacity_server
    procedure :: get_capacity_local
    procedure :: get_capacity_server
    procedure :: start_listening
    procedure :: server_barrier
    procedure :: full_barrier
  end type distributed_circular_buffer

contains

  !> Check if the buffer is valid: it has been successfully created and has not yet been destroyed
  function is_valid(this)
    implicit none
    class(distributed_circular_buffer), intent(in) :: this
    logical :: is_valid !< Whether this buffer is usable

    is_valid = .false.

    if (c_associated(this % c_buffer)) then
      if (DCB_check_integrity(this % c_buffer, 1) == 0) then
        is_valid = .true.
      end if
    end if
  end function is_valid

  !> Create and initialize a distributed circular buffer. See DCB_create
  function create(this, communicator, server_communicator, num_producers, num_channels, num_words) result(is_valid)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    type(MPI_Comm), intent(in)                        :: communicator, server_communicator
    integer(C_INT), intent(in)                        :: num_producers
    integer(C_INT), intent(in)                        :: num_channels
    integer(C_INT), intent(in)                        :: num_words
    logical :: is_valid !< .true. if the creation was a success, .false. otherwise

    if (this % is_valid()) then
      call this % delete()
    end if

    this % c_buffer = DCB_create(communicator % mpi_val, server_communicator % mpi_val, num_producers, num_channels, num_words)
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
  subroutine print(this, dump_data)
    implicit none
    class(distributed_circular_buffer), intent(in)  :: this
    logical, intent(in) :: dump_data !< Whether to print buffer content

    integer(C_INT) :: c_dump_data

    c_dump_data = 0
    if (dump_data) c_dump_data = 1

    call DCB_print(this % c_buffer, c_dump_data)
  end subroutine print

  !> Insert elements into a distributed circular buffer. See DCB_put
#define IgnoreTypeKindRank src_data
#define ExtraAttributes , target
  function put(this, src_data, num_elements, commit_transaction) result(num_space_available)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    ! integer(DATA_ELEMENT), dimension(*), intent(in)     :: src_data
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_INT), intent(in)                          :: num_elements
    logical, intent(in)                                 :: commit_transaction
    integer(C_INT) :: num_space_available !< The return value of DCB_put (number of available spaces, if successful)

    type(C_PTR)    :: src_ptr
    integer(C_INT) :: operation

    src_ptr = C_LOC(src_data)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    num_space_available = DCB_put(this % c_buffer, src_ptr, num_elements, operation)
  end function put

  !> Extract elements from a distributed circular buffer. See DCB_get
#define IgnoreTypeKindRank dest_data
#define ExtraAttributes , target
  function get(this, buffer_id, dest_data, num_elements, commit_transaction) result(num_data_available)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(C_INT), intent(in)                          :: buffer_id
    ! integer(DATA_ELEMENT), dimension(*), intent(inout)  :: dest_data
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_INT), intent(in)                          :: num_elements
    logical, intent(in)                                 :: commit_transaction
    integer(C_INT) :: num_data_available !< The return value of DCB_get

    type(C_PTR)    :: dest_ptr
    integer(C_INT) :: operation

    dest_ptr = C_LOC(dest_data)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    num_data_available = DCB_get(this % c_buffer, buffer_id, dest_ptr, num_elements, operation)
  end function get

#define IgnoreTypeKindRank dest_data
#define ExtraAttributes , target
  function peek(this, buffer_id, dest_data, num_elements) result(num_data_available)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(C_INT), intent(in)                          :: buffer_id
    ! integer(DATA_ELEMENT), dimension(*), intent(inout)  :: dest_data
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_INT), intent(in)                          :: num_elements
    integer(C_INT) :: num_data_available !< The return value of DCB_get

    type(C_PTR) :: dest_ptr
    dest_ptr = C_LOC(dest_data)

    num_data_available = DCB_get(this % c_buffer, buffer_id, dest_ptr, num_elements, CB_PEEK)
  end function peek

  function get_num_elements(this, buffer_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT), intent(in)                        :: buffer_id
    integer(C_INT) :: num_elements

    num_elements = DCB_get_num_elements(this % c_buffer, buffer_id)
  end function get_num_elements

  function get_num_spaces(this, update_from_remote) result(num_spaces)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    logical, intent(in)                               :: update_from_remote
    integer(C_INT) :: num_spaces

    integer(C_INT) :: c_update
    c_update = 0
    if (update_from_remote) c_update = 1
    num_spaces = DCB_get_num_spaces(this % c_buffer, c_update)
  end function get_num_spaces

  subroutine sync_window(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    call DCB_sync_window(this % c_buffer)
  end subroutine sync_window

  function get_producer_id(this) result(producer_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: producer_id
    producer_id = DCB_get_producer_id(this % c_buffer)
  end function get_producer_id

  function get_channel_id(this) result(channel_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: channel_id
    channel_id = DCB_get_channel_id(this % c_buffer)
  end function get_channel_id

  function get_consumer_id(this) result(consumer_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: consumer_id
    consumer_id = DCB_get_consumer_id(this % c_buffer)
  end function get_consumer_id

  function get_num_producers(this) result(num_producers)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_producers
    num_producers = DCB_get_num_producers(this % c_buffer)
  end function get_num_producers

  function get_num_consumers(this) result(num_consumers)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_consumers
    num_consumers = DCB_get_num_consumers(this % c_buffer)
  end function get_num_consumers

  function get_capacity_local(this) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_elements
    num_elements = DCB_get_capacity_local(this % c_buffer)
  end function get_capacity_local

  function get_capacity_server(this, buffer_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT),                     intent(in)    :: buffer_id
    integer(C_INT) :: num_elements
    num_elements = DCB_get_capacity_server(this % c_buffer, buffer_id)
  end function get_capacity_server

  function start_listening(this) result(return_value)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: return_value
    return_value = DCB_channel_start_listening(this % c_buffer)
  end function start_listening

  subroutine full_barrier(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    call DCB_full_barrier(this % c_buffer)
  end subroutine full_barrier

  subroutine server_barrier(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    call DCB_server_barrier(this % c_buffer)
  end subroutine server_barrier

end module distributed_circular_buffer_module
