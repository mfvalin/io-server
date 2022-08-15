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

module ioserver_message_module
  use ISO_C_BINDING
  use shmem_heap_module, only: MAX_ARRAY_RANK, HEAP_ELEMENT
  use grid_meta_module
  use ioserver_constants_module
  use jar_module
  use rpn_extra_module
  implicit none
  private

  integer, parameter, public :: MAX_FILE_NAME_SIZE = 2048

  public :: grid_bounds_t, grid_index_t

  type, public :: ioserver_messenger
    private
    integer :: msg_tag_seq   = 0
    integer :: file_open_seq = 0
    logical :: debug_mode = .false.
    type(comm_rank_size) :: model_crs
  contains
    procedure, pass :: set_debug
    procedure, pass :: set_model_crs
    procedure, pass :: bump_tag
    procedure, pass :: get_msg_tag
    procedure, pass :: get_file_tag
  end type ioserver_messenger

  ! Type used as a header when writing data to a stream from a model process
  type, public :: data_record
    integer(HEAP_ELEMENT) :: heap_offset       !< Offset of the data within its heap. Allows to retrieve it from another process
    integer(C_INT64_T)    :: data_size_byte    !< Size of the data packet itself, in bytes
    integer(C_INT64_T)    :: command_size_int8 !< Size of the command part of the message, in 64-bit elements

    integer(C_INT) :: tag       !< Tag associated with this particular message (to be able to group with that of other model PEs)
    integer(C_INT) :: stream    !< Stream to which the data is being sent

    type(grid_bounds_t) :: local_bounds         !< Local grid bounds, in global coordinates
    type(grid_bounds_t) :: global_bounds        !< Subset of the global grid in which we are interested, in global coord ("reduced" global grid)

    integer(C_INT) :: elem_size         !< Size of the grid elements in bytes
    integer(C_INT) :: output_grid_id    !< ID of the grid where the data is being sent
  end type data_record

  type, public, bind(C) :: command_record
    integer(C_INT64_T) :: size_int8     =  0  !< Size of the command content (excluding this header)
    integer(C_INT)     :: command_type  = -1  !< Type of command (in order to know who will have to process it)
    integer(C_INT)     :: stream_id     = -1  !< ID of the stream where the command should be executed
    integer(C_INT)     :: message_tag   = -1  !< Tag of the message that sends this command
  end type command_record

  integer(C_INT), parameter, public :: MSG_HEADER_TAG = 1010101 !< First entry of every message. Helps debugging
  integer(C_INT), parameter, public :: MSG_CAP_TAG    =  101010 !< (Second-to-)Last entry of every message. Helps debugging
  type, public, bind(C) :: message_header
    integer(C_INT)     :: header_tag     = MSG_HEADER_TAG !< Signals the start of a message. Gotta be the first item
    integer(C_INT)     :: command             = -1  !< What this message contains
    integer(C_INT64_T) :: content_size_int8   = -1  !< Message length (excluding this header), in number of 64-bit elements
    integer(C_INT)     :: stream_rank         = -1  !< Stream number in the (fixed) list of stream objects
    integer(C_INT)     :: stream_id           = -1  !< To what stream this message is destined
    integer(C_INT)     :: message_tag         = -1  !< A collective tag associated with messages from model processes (incremented at every message)
    integer(C_INT)     :: sender_global_rank  = -1  !< Who is sending that message
    integer(C_INT)     :: relay_global_rank   = -1  !< Who is transmitting the message
  end type message_header

  type, public, bind(C) :: message_cap
    integer(C_INT)      :: cap_tag    = MSG_CAP_TAG !< Signals the end of a message
    integer(C_INT64_T)  :: msg_length = -1          !< Length of the message that just ended. Gotta match the length indicated in the message header
  end type message_cap

  integer(C_INT), parameter, public :: MSG_COMMAND_DATA          = 0 !< Indicate a message that contains grid data
  integer(C_INT), parameter, public :: MSG_COMMAND_DUMMY         = 1 !< Indicate a message without content or purpose
  integer(C_INT), parameter, public :: MSG_COMMAND_USER          = 2 !< Will call a user-provided function on the server
  integer(C_INT), parameter, public :: MSG_COMMAND_MODEL_STOP    = 4 !< Indicate that the model that sends this message will no longer send anything
  integer(C_INT), parameter, public :: MSG_COMMAND_RELAY_STOP    = 5 !< Indicate that the relay that sends this message will no longer send anything
  integer(C_INT), parameter, public :: MSG_COMMAND_SERVER_CMD    = 6 !< Indicate a message that sends a command to the server to be processes there
  integer(C_INT), parameter, public :: MSG_COMMAND_OPEN_STREAM   = 7 !< Indicate a message that want to create a stream on the server
  integer(C_INT), parameter, public :: MSG_COMMAND_CLOSE_STREAM  = 8 !< Indicate a message that want to create a stream on the server
  integer(C_INT), parameter, public :: MSG_COMMAND_MODEL_STATS   = 9 !< Indicate a message that contains model statistics to transmit to the server

  public :: message_header_size_int8, message_cap_size_int8, data_record_size_int8, command_record_size_int8
  public :: message_header_size_byte, message_cap_size_byte, data_record_size_byte, command_record_size_byte
  public :: print_message_header, print_data_record, print_command_record, get_message_command_string

contains

  function message_header_size_byte()
    implicit none
    integer(C_INT64_T) :: message_header_size_byte !< Size of the message_header type in bytes
    type(message_header) :: dummy_header

    message_header_size_byte = storage_size(dummy_header) / 8
  end function message_header_size_byte

  function message_header_size_int8()
    implicit none
    integer(C_INT64_T) :: message_header_size_int8 !< How many 64-bit integers are needed to contain a message_header
    message_header_size_int8 = num_char_to_num_int8(message_header_size_byte())
  end function message_header_size_int8

  function message_cap_size_byte()
    implicit none
    integer(C_INT64_T) :: message_cap_size_byte !< Size of the message_cap type in bytes
    type(message_cap) :: dummy_cap

    message_cap_size_byte = storage_size(dummy_cap) / 8
  end function message_cap_size_byte

  function message_cap_size_int8()
    implicit none
    integer(C_INT64_T) :: message_cap_size_int8 !< How many 64-bit integers are needed to contain a message_cap
    message_cap_size_int8 = num_char_to_num_int8(message_cap_size_byte())
  end function message_cap_size_int8

  function data_record_size_byte()
    implicit none
    integer(C_INT64_T) :: data_record_size_byte !< Size of the data_record type in bytes
    type(data_record) :: dummy_record
    data_record_size_byte = storage_size(dummy_record) / 8
  end function data_record_size_byte

  function data_record_size_int8()
    implicit none
    integer(C_INT64_T) :: data_record_size_int8 !< How many 64-bit integers are needed to contain a data_record
    data_record_size_int8 = num_char_to_num_int8(data_record_size_byte())
  end function data_record_size_int8

  function command_record_size_byte()
    implicit none
    integer(C_INT64_T) :: command_record_size_byte
    type(command_record) :: dummy_record
    command_record_size_byte = storage_size(dummy_record) / 8
  end function command_record_size_byte

  function command_record_size_int8()
    implicit none
    integer(C_INT64_T) :: command_record_size_int8
    command_record_size_int8 = num_char_to_num_int8(command_record_size_byte())
  end function command_record_size_int8

  subroutine bump_tag(this, new_file)
    implicit none
    class(ioserver_messenger), intent(inout) :: this
    logical, intent(in), optional :: new_file
    integer :: ierr

    if (this % debug_mode) call MPI_Barrier(this % model_crs % comm, ierr)    ! in debug mode, enforce collective mode
    this % msg_tag_seq = this % msg_tag_seq + 1

    if (present(new_file)) then
      if (new_file) this % file_open_seq = this % file_open_seq + 1
    end if
  end subroutine bump_tag

  !> Retrieve the current tag number in the sequence of messages
  function get_msg_tag(this) result(tag)
    implicit none
    class(ioserver_messenger), intent(in) :: this
    integer :: tag
    tag = this % msg_tag_seq
  end function get_msg_tag

  !> Retrieve the current tag number in the sequence of streams
  function get_file_tag(this) result(tag)
    implicit none
    class(ioserver_messenger), intent(in) :: this
    integer :: tag
    tag = this % file_open_seq
  end function get_file_tag

  subroutine set_debug(this, debug_mode)
    implicit none
    class(ioserver_messenger), intent(inout) :: this
    logical, intent(in) :: debug_mode
    integer :: ierr
    this % debug_mode = debug_mode
    if (.not. this % model_crs % is_null()) call MPI_Allreduce(debug_mode, this % debug_mode, 1, MPI_LOGICAL, MPI_LOR, this % model_crs % comm, ierr)
  end subroutine set_debug

  subroutine set_model_crs(this, model_crs)
    implicit none
    class(ioserver_messenger), intent(inout) :: this
    type(comm_rank_size), intent(in) :: model_crs
    this % model_crs = model_crs
  end subroutine set_model_crs

  function get_message_command_string(command) result(command_string)
    implicit none
    integer, intent(in) :: command
    character(len=32) :: command_string
    select case (command)
    case (MSG_COMMAND_DATA)
      command_string = 'MSG_COMMAND_DATA'
    case (MSG_COMMAND_USER)
      command_string = 'MSG_COMMAND_USER'
    case (MSG_COMMAND_DUMMY)
      command_string = 'MSG_COMMAND_DUMMY'
    case (MSG_COMMAND_MODEL_STOP)
      command_string = 'MSG_COMMAND_MODEL_STOP'
    ! case (MSG_COMMAND_OPEN_FILE)
    !   command_string = 'MSG_COMMAND_OPEN_FILE'
    case (MSG_COMMAND_RELAY_STOP)
      command_string = 'MSG_COMMAND_RELAY_STOP'
    case (MSG_COMMAND_SERVER_CMD)
      command_string = 'MSG_COMMAND_SERVER_CMD'
    case (MSG_COMMAND_OPEN_STREAM)
      command_string = 'MSG_COMMAND_OPEN_STREAM'
    case (MSG_COMMAND_CLOSE_STREAM)
      command_string = 'MSG_COMMAND_CLOSE_STREAM'
    case (MSG_COMMAND_MODEL_STATS)
      command_string = 'MSG_COMMAND_MODEL_STATS'
    case default
      command_string = '[ERROR] unknown number'
    end select
  end function get_message_command_string

  subroutine print_message_header(header)
    implicit none
    type(message_header), intent(in) :: header
    print '(A, I8, A, I8, A, I3, 1X, A, A, I4, A, I7, A, I8, A, I5, A, I5)', &
      'Header: header tag ', header % header_tag, &
      ', len ', header % content_size_int8, &
      ', cmd ', header % command, get_message_command_string(header % command), &
      ', stream rank ', header % stream_rank, &
      ', stream ID   ', header % stream_id, &
      ', message tag ', header % message_tag, &
      ', sender rank ', header % sender_global_rank, &
      ', relay rank ', header % relay_global_rank
  end subroutine print_message_header

  subroutine print_command_record(record)
    implicit none
    type(command_record), intent(in) :: record
    print '(A, I5, A, A, A, I7, A, I8)',                   &
      'Command record: Size (int8) ', record % size_int8,   &
      ', command type ', get_message_command_string(record % command_type),             &
      ', stream ID ', record % stream_id,                   &
      ', message tag ', record % message_tag
  end subroutine print_command_record

  subroutine print_data_record(record)
    implicit none
    type(data_record), intent(in) :: record

    print *, 'print_data_record() not implemented', record % tag
    ! print '(A12,   A, I6, I4, I4,   A, I7, I4,   A, I4, I4, I4, I3, I7,   A, I5, I5, I6, I6, I8, I4)', 'Record info: ', &
    !   'sizes ', record % data_size_byte, record % cmeta_size, record % meta_size, &
    !   ', tag+stream ', record % tag, record % stream, &
    !   ', dimensions ', record % ni, record % nj, record % nk, record % nvar, record % type_kind_rank, &
    !   ', global grid ', record % i0, record % j0, record % grid_size_i, record % grid_size_j, record % gnignj, record % output_grid_id

  end subroutine print_data_record

end module ioserver_message_module
