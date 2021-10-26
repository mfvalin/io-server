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
  use circular_buffer_module
  implicit none
  include 'io-server/distributed_circular_buffer.inc'

  private

  public :: CB_KIND_CHAR, CB_KIND_INTEGER_4, CB_KIND_INTEGER_8, CB_KIND_REAL_4, CB_KIND_REAL_8
  public :: DCB_SERVER_BOUND_TYPE, DCB_CLIENT_BOUND_TYPE, DCB_CHANNEL_TYPE

  !> A set of FIFO queues used by multiple pairs of processes, with their data stored on a single one of these processes. Called a DCB
  !> See #distributed_circular_buffer
  type, public :: distributed_circular_buffer
    private
    type(C_PTR) :: c_buffer = C_NULL_PTR !< Pointer to the C struct containing all distributed circular buffer info
  contains
    procedure :: is_valid           !< distributed_circular_buffer_module::is_valid
    procedure :: create_bytes       !< distributed_circular_buffer_module::create_bytes
    procedure :: delete             !< distributed_circular_buffer_module::delete
    procedure :: print              !< distributed_circular_buffer_module::print
    procedure :: put_elems          !< distributed_circular_buffer_module::put_elems
    procedure :: get_elems          !< distributed_circular_buffer_module::get_elems
    procedure :: peek_elems         !< distributed_circular_buffer_module::peek_elems
    procedure :: get_num_elements   !< distributed_circular_buffer_module::get_num_elements
    procedure :: get_num_spaces     !< distributed_circular_buffer_module::get_num_spaces
    procedure :: get_channel_id     !< distributed_circular_buffer_module::get_channel_id
    procedure :: get_server_bound_server_id    !< distributed_circular_buffer_module::get_server_bound_server_id
    procedure :: get_server_bound_client_id    !< distributed_circular_buffer_module::get_server_bound_client_id
    procedure :: get_num_server_bound_clients  !< distributed_circular_buffer_module::get_num_server_bound_clients
    procedure :: get_num_server_consumers      !< distributed_circular_buffer_module::get_num_server_consumers
    GENERIC :: get_capacity => get_capacity_local, get_capacity_server !< Get the capacity of this buffer
    procedure :: get_capacity_local !< distributed_circular_buffer_module::get_capacity_local
    procedure :: get_capacity_server !< distributed_circular_buffer_module::get_capacity_server
    procedure :: start_listening    !< distributed_circular_buffer_module::start_listening
    procedure :: server_barrier     !< distributed_circular_buffer_module::server_barrier
    procedure :: full_barrier       !< distributed_circular_buffer_module::full_barrier
    ! final     :: dcb_finalize
  end type distributed_circular_buffer

contains

  !> Check if the buffer is valid: it has been successfully created, has not yet been destroyed and passes some integrity checks
  !> \sa DCB_check_integrity
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

  !> Create and initialize a distributed circular buffer. See DCB_create. This is a collective call
  !> If there already was an underlying DCB, it will be deleted
  function create_bytes(this, communicator, server_communicator, communication_type, num_bytes_server_bound, num_bytes_client_bound) result(is_valid)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    type(MPI_Comm),    intent(in)                        :: communicator            !< MPI communicator common to all processes that share this buffer
    type(MPI_Comm),    intent(in)                        :: server_communicator     !< MPI communicator between the server processes only
    integer(C_INT),    intent(in)                        :: communication_type      !< Whether this process is for server- or client-bound CBs, or just a communication channel
    integer(C_SIZE_T), intent(in)                        :: num_bytes_server_bound  !< How many bytes of data can be stored in server-bound buffers
    integer(C_SIZE_T), intent(in)                        :: num_bytes_client_bound  !< How many bytes of data can be stored in client-bound buffers
    logical :: is_valid !< .true. if the creation was a success, .false. otherwise

    if (this % is_valid()) then
      call this % delete()
    end if

    this % c_buffer = DCB_create(communicator % mpi_val, server_communicator % mpi_val, communication_type, num_bytes_server_bound, num_bytes_client_bound)
    is_valid = this % is_valid()
  end function create_bytes

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
    logical, intent(in) :: dump_data !< Whether to print buffer content as well

    integer(C_INT) :: c_dump_data

    c_dump_data = 0
    if (dump_data) c_dump_data = 1

    call DCB_print(this % c_buffer, c_dump_data)
  end subroutine print

  !> Insert elements into a distributed circular buffer. See DCB_put_client
#define IgnoreTypeKindRank src_data
#define ExtraAttributes , target
  function put_elems(this, src_data, num_elements, type_id, commit_transaction) result(success)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T), intent(in) :: num_elements !< How many elements we want to insert
    integer,           intent(in) :: type_id      !< Type of elements to insert
    logical,           intent(in) :: commit_transaction !< Whether to make the inserted elements immediately available to the server process
    logical :: success !< Whether the operation was successful

    type(C_PTR)    :: src_ptr
    integer(C_INT) :: status
    integer(C_INT) :: operation
    integer        :: type_size

    success   = .false.
    src_ptr   = C_LOC(src_data)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    status = DCB_put_client(this % c_buffer, src_ptr, num_elements * type_size, operation)

    if (status == 0) success = .true.
  end function put_elems

  !> Extract elements from a distributed circular buffer. See DCB_get_server
#define IgnoreTypeKindRank dest_data
#define ExtraAttributes , target
  function get_elems(this, buffer_id, dest_data, num_elements, type_id, commit_transaction) result(success)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(C_INT), intent(in)                          :: buffer_id !< The individual buffer instance from which we want to read
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T), intent(in) :: num_elements !< How many elements we want to read
    integer,           intent(in) :: type_id      !< Type of elements to read
    logical,           intent(in) :: commit_transaction !< Whether to immediately free the space used by the read elements for the corresponding producer
    logical :: success !< Whether the operation was successful

    type(C_PTR)    :: dest_ptr
    integer(C_INT) :: operation
    integer        :: type_size
    integer(C_INT) :: status

    success   = .false.
    dest_ptr  = C_LOC(dest_data)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    status = DCB_get_server(this % c_buffer, buffer_id, dest_ptr, num_elements * type_size, operation)
    if (status == 0) success = .true.
  end function get_elems

  !> Read the next elements in a buffer instance, without removing them. See DCB_get_server
#define IgnoreTypeKindRank dest_data
#define ExtraAttributes , target
  function peek_elems(this, buffer_id, dest_data, num_elements, type_id) result(success)
    implicit none
    class(distributed_circular_buffer), intent(inout)   :: this
    integer(C_INT), intent(in)                          :: buffer_id !< The individual buffer instance from which we want to peek
#include <IgnoreTypeKindRankPlus.hf>
    integer(C_SIZE_T), intent(in) :: num_elements !< How many elements we want to look at
    integer,           intent(in) :: type_id      !< Type of elements we want to look at
    logical :: success !< Whether the operation was successful

    type(C_PTR)    :: dest_ptr
    integer        :: type_size
    integer(C_INT) :: status

    success   = .false.
    dest_ptr  = C_LOC(dest_data)
    type_size = get_type_size(type_id)

    status = DCB_get_server(this % c_buffer, buffer_id, dest_ptr, num_elements * type_size, CB_PEEK)
    if (status == 0) success = .true.
  end function peek_elems

  !> \brief Get current number of elements of type [type_id] stored in one of the buffer instances.
  !> \return The number of elements if all went well, -1 if there was an error
  !> \sa DCB_get_available_data
  function get_num_elements(this, buffer_id, type_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT), intent(in)                        :: buffer_id  !< The individual buffer instance we want to query
    integer,        intent(in)                        :: type_id    !< Type of elements we want to count
    integer(C_INT64_T) :: num_elements

    integer :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size = get_type_size(type_id)
    num_bytes = DCB_get_available_data(this % c_buffer, buffer_id)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_elements

  !> \brief Get current number of available spaces that can fit element of type [type_id] in one of the buffer instances.
  !> \return The number of spaces if all went well, -1 if there was an error.
  !> \sa DCB_get_available_space
  function get_num_spaces(this, type_id, update_from_remote) result(num_spaces)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer, intent(in)                               :: type_id            !< Type of elements we want to count
    logical, intent(in)                               :: update_from_remote !< Whether to get the very latest count by querying the server
    integer(C_INT64_T) :: num_spaces

    integer            :: type_size
    integer(C_INT64_T) :: num_bytes
    integer(C_INT)     :: c_update

    c_update = 0
    if (update_from_remote) c_update = 1

    num_bytes  = DCB_get_available_space(this % c_buffer, c_update)
    type_size  = get_type_size(type_id)
    num_spaces = num_bytes / type_size
    if (num_bytes < 0) num_spaces = -1
  end function get_num_spaces

  !> Get the server-bound client ID of this buffer. See DCB_get_server_bound_client_id
  !> \return The client ID if we are indeed a server-bound client, -1 otherwise
  function get_server_bound_client_id(this) result(client_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: client_id
    client_id = DCB_get_server_bound_client_id(this % c_buffer)
  end function get_server_bound_client_id

  !> Get the channel ID of this buffer. See DCB_get_channel_id
  !> \return The channel ID if we are indeed a channel process, -1 otherwise
  function get_channel_id(this) result(channel_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: channel_id
    channel_id = DCB_get_channel_id(this % c_buffer)
  end function get_channel_id

  !> Get the consumer ID of this buffer. See DCB_get_server_bound_server_id
  !> \return The consumer ID if we are indeed a consumer, -1 otherwise
  function get_server_bound_server_id(this) result(server_id)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: server_id
    server_id = DCB_get_server_bound_server_id(this % c_buffer)
  end function get_server_bound_server_id

  !> Get the number of server-bound clients that participate in this buffer set
  !> \sa DCB_get_num_server_bound_instances
  function get_num_server_bound_clients(this) result(num_clients)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_clients
    num_clients = DCB_get_num_server_bound_instances(this % c_buffer)
  end function get_num_server_bound_clients

  !> Get the number of server consumer processes that participate in this buffer set
  !> \sa DCB_get_num_server_consumers
  function get_num_server_consumers(this) result(num_consumers)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: num_consumers
    num_consumers = DCB_get_num_server_consumers(this % c_buffer)
  end function get_num_server_consumers

  !> Get the capacity of this circular buffer instance, in the given element type. Can only be called by a producer
  !> \return The number of elements of the given type that the CB instance can hold, -1 if we are not a producer process
  !> \sa DCB_get_capacity_local_bytes
  function get_capacity_local(this, type_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer,                            intent(in)    :: type_id !< Type of elements we want to count
    integer(C_INT64_T) :: num_elements
    num_elements = DCB_get_capacity_local(this % c_buffer) / get_type_size(type_id)
  end function get_capacity_local

  !> Get the capacity of a circular buffer in this DCB. Can only be called by a server process
  !> \return Number of elements of the given type that the CB instance can hold, -1 if we are not a server process
  !> \sa DCB_get_capacity_server_bytes
  function get_capacity_server(this, buffer_id, type_id) result(num_elements)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT),                     intent(in)    :: buffer_id !< ID of the instance we want to query 
    integer       ,                     intent(in)    :: type_id !< Type of elements we want to count
    integer(C_INT64_T) :: num_elements
    num_elements = DCB_get_capacity_server(this % c_buffer, buffer_id) / get_type_size(type_id)
  end function get_capacity_server

  !> Start a channel process working. Will loop until the DCB is deleted.
  !> \return -1 if we are not a channel process, 0 otherwise (once the DCB gets deleted)
  !> \sa DCB_channel_start_listening
  function start_listening(this) result(return_value)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    integer(C_INT) :: return_value
    return_value = DCB_channel_start_listening(this % c_buffer)
  end function start_listening

  !> MPI barrier among all processes that participate in this DCB (including channels)
  !> \sa DCB_full_barrier
  subroutine full_barrier(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    call DCB_full_barrier(this % c_buffer)
  end subroutine full_barrier

  !> MPI barrier among server processes that participate in this DCB (including channels)
  !> \sa DCB_server_barrier
  subroutine server_barrier(this)
    implicit none
    class(distributed_circular_buffer), intent(inout) :: this
    call DCB_server_barrier(this % c_buffer)
  end subroutine server_barrier

  ! subroutine dcb_finalize(this)
  !   implicit none
  !   type(distributed_circular_buffer), intent(inout) :: this
  !   call this % delete()
  ! end subroutine dcb_finalize

end module distributed_circular_buffer_module
