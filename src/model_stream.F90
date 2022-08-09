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

module model_stream_module
  use circular_buffer_module
  use ioserver_message_module
  use jar_module
  use rpn_extra_module
  use shmem_heap_module
  implicit none

  private

#if ! defined(VERSION)
#define VERSION 10000
#endif

  type, public :: model_stream
    private
    integer               :: version = VERSION      !< Version marker, used to check version coherence
    integer               :: stream_rank = -1       !< Stream rank in fixed list of streams, for internal use
    integer               :: stream_id = -1         !< Unique stream ID, for internal use
    logical               :: debug = .false.        !< debug mode at the file level (is this even used?)
    integer               :: global_rank = -1       !< Rank of the model PE that created this object, within the world communicator
    integer               :: model_rank = -1        !< Rank of the model PE that created this object, within the model-only communicator
    type(shmem_heap)      :: local_heap             !< Access to the shared memory heap owned by this model PE
    type(circular_buffer) :: server_bound_cb        !< Access to the server-bound buffer owned by this model PE
    type(ioserver_messenger), pointer :: messenger => NULL() !< Messenger used to manage/synchronized data transmission to the server
    integer               :: put_cmd_timeout_ms     !< How many milliseconds to wait before failing when sending a command
    integer               :: put_data_timeout_ms    !< How many milliseconds to wait before failing when sending data

    contains

    ! initial, pass :: server_file_construct
    ! procedure :: open  => model_stream_open
    procedure :: read  => model_stream_read
    procedure :: open  => model_stream_open
    procedure :: close => model_stream_close
    procedure :: is_open
    procedure, private :: is_version_valid
    procedure :: is_valid
    procedure :: set_debug
    procedure :: send_command
    procedure :: send_data
  end type

  interface model_stream
    procedure :: new_model_stream
  end interface model_stream

contains

  function new_model_stream(global_rank, model_rank, stream_rank, local_heap, server_bound_cb, debug_mode, messenger, put_cmd_timeout_ms, put_data_timeout_ms)
    implicit none
    integer,                intent(in)  :: global_rank
    integer,                intent(in)  :: model_rank
    integer,                intent(in)  :: stream_rank
    type(shmem_heap),       intent(in)  :: local_heap
    type(circular_buffer),  intent(in)  :: server_bound_cb
    logical,                intent(in)  :: debug_mode
    type(ioserver_messenger), pointer, intent(in) :: messenger
    type(model_stream) :: new_model_stream
    integer,                intent(in) :: put_cmd_timeout_ms
    integer,                intent(in) :: put_data_timeout_ms

    if (server_bound_cb % is_valid()) then
      new_model_stream % global_rank = global_rank
      new_model_stream % model_rank  = model_rank
      new_model_stream % stream_rank = stream_rank
      new_model_stream % local_heap  = local_heap
      new_model_stream % server_bound_cb = server_bound_cb
      new_model_stream % debug = debug_mode
      new_model_stream % messenger => messenger
      new_model_stream % put_cmd_timeout_ms = put_cmd_timeout_ms
      new_model_stream % put_data_timeout_ms = put_data_timeout_ms
    else
      print '(A)', 'ERROR: Server-bound CB has not been initialized!'
      return
    end if
  end function new_model_stream

  function set_debug(this, dbg) result(status)
    implicit none
    class(model_stream), intent(INOUT) :: this
    logical, intent(IN) :: dbg
    logical :: status

    status = this % debug
    this % debug = dbg

  end function set_debug

  function is_version_valid(this) result(status)
    implicit none
    class(model_stream), intent(IN) :: this
    logical :: status
    status = this % version == VERSION
  end function is_version_valid

  function is_valid(this)
    implicit none
    class(model_stream), intent(in) :: this
    logical :: is_valid
    is_valid = .false.
    if (this % is_version_valid()) then
      is_valid = this % stream_rank >= 0                    .and. &
                 this % global_rank >= 0                    .and. &
                 this % server_bound_cb % is_valid()        .and. &
                 associated(this % messenger)
    end if
  end function is_valid

  function is_open(this) result(status)
    implicit none
    class(model_stream), intent(IN) :: this
    logical :: status

    status = this % is_valid() .and. this % stream_id > 0
  end function is_open

  function send_command(this, command_content) result(success)
    implicit none
    class(model_stream), intent(inout) :: this
    type(jar),           intent(inout) :: command_content
    logical :: success

    type(message_header) :: header
    type(message_cap)    :: end_cap
    type(command_record) :: command_header

    success = .false.
    
    ! print *, 'Command content: ', command_content % array()

    ! Check if we actually can send a command to this stream
    if (.not. this % is_open()) return
    if (.not. this % server_bound_cb % is_valid()) return

    ! Never forget to get a new message tag! (message only, not file)
    call this % messenger % bump_tag(.false.)

    command_header % size_int8    = command_content % get_top()
    command_header % command_type = MSG_COMMAND_SERVER_CMD
    command_header % stream_id    = this % stream_id
    command_header % message_tag  = this % messenger % get_msg_tag()

    header % content_size_int8  = command_record_size_int8() + command_header % size_int8
    header % command            = MSG_COMMAND_SERVER_CMD
    header % stream_rank        = this % stream_rank
    header % stream_id          = this % stream_id
    header % message_tag        = command_header % message_tag
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_size_int8

    ! call print_message_header(header)
    ! call print_command_record(command_header)
    ! print '(A, I8)', 'Sending command with content size ', command_header % size_int8

    ! Header
    success = this % server_bound_cb % put(                                                                       &
        header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_cmd_timeout_ms)

    ! Command record
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          command_header, command_record_size_byte(), CB_KIND_CHAR, .false.,                                      &
          timeout_ms = this % put_cmd_timeout_ms)
    end if

    ! Command parameters
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          command_content % f_array(), command_header % size_int8, CB_DATA_ELEMENT_KIND, .false.,                 &
          timeout_ms = this % put_cmd_timeout_ms)
    end if

    ! End cap + commit
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = this % put_cmd_timeout_ms)
    end if
  end function send_command

  function model_stream_open(this) result(success)
    implicit none
    class(model_stream),    intent(INOUT) :: this
    logical :: success

    type(message_header) :: header
    type(message_cap)    :: end_cap
    type(command_record) :: command

    success = .false.

    if (.not. this % is_valid()) then
      print *, 'ERROR trying to open model stream: invalid stream object'
      ! print '(A, I3, A, I3, A, L, A, L)', 'stream rank ', this % stream_rank, ' global rank ', this % global_rank,     &
      !       ' valid CB ', this % server_bound_cb % is_valid(.false.),                &
      !       ' associated messenger ', associated(this % messenger)
      return
    end if

    if (this % is_open()) return ! already open

    call this % messenger % bump_tag(.true.)

    this % stream_id = this % messenger % get_file_tag()

    ! print *, 'Opening stream, rank ', this % stream_rank

    header % content_size_int8  = command_record_size_int8()
    header % command            = MSG_COMMAND_SERVER_CMD
    header % stream_rank        = this % stream_rank
    header % stream_id          = this % stream_id
    header % message_tag        = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    command % size_int8    = 0
    command % command_type = MSG_COMMAND_OPEN_STREAM
    command % stream_id    = header % stream_id
    command % message_tag  = header % message_tag

    end_cap % msg_length = header % content_size_int8

    ! Header
    success = this % server_bound_cb % put(                                                                       &
        header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_cmd_timeout_ms)

    ! The command
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          command, command_record_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_cmd_timeout_ms)
    end if

    ! End cap + commit
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = this % put_cmd_timeout_ms)
    end if
  end function model_stream_open

  function model_stream_close(this) result(success)
    implicit none
    class(model_stream), intent(INOUT) :: this
    logical :: success

    type(message_header) :: header
    type(message_cap)    :: end_cap
    type(command_record) :: command

    success = .false.
    if (.not. this % is_open()) return

    call this % messenger % bump_tag()

    header % content_size_int8  = command_record_size_int8()
    header % stream_rank        = this % stream_rank
    header % stream_id          = this % stream_id
    header % message_tag        = this  % messenger % get_msg_tag()
    header % command            = MSG_COMMAND_SERVER_CMD
    header % sender_global_rank = this % global_rank

    command % size_int8    = 0
    command % command_type = MSG_COMMAND_CLOSE_STREAM
    command % stream_id    = header % stream_id
    command % message_tag  = header % message_tag
  
    end_cap % msg_length = header % content_size_int8
    
    ! Header
    success = this % server_bound_cb % put(                                                                     &
        header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_cmd_timeout_ms)

    ! The command
    if (success) then
      success = this % server_bound_cb % put(                                                                   &
          command, command_record_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_cmd_timeout_ms)
    end if

    ! End cap (+ commit)
    if (success) then
      success = this % server_bound_cb % put(                                                                   &
          end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = this % put_cmd_timeout_ms)
    end if

    this % stream_id = -1

    ! call print_message_header(header)

  end function model_stream_close

  !> Send a bunch of data (a "tile") towards the server
  function send_data(this, data_info, local_bounds, global_bounds, command) result(success)
    use iso_c_binding
    use jar_module
    implicit none
    class(model_stream),  intent(inout) :: this           !< model_stream instance
    type(block_meta_f08), intent(in)    :: data_info      !< Array descriptor from shared memory heap allocation
    type(grid_bounds_t),  intent(in)    :: local_bounds   !< Bounds of the local grid, in global coordinates
    type(grid_bounds_t),  intent(in)    :: global_bounds  !< Bounds of the (reduced) global grid, in global coordinates

    type(jar),   intent(IN), optional :: command          !< Command to apply to the data by the model once assembled

    logical :: success  !< Whether this function call succeeded

    type(data_record)     :: rec
    type(message_header)  :: header
    type(message_cap)     :: end_cap
    type(command_record)  :: command_meta

    success = .false.
    if(this % stream_id <= 0) return

    ! Perform a short series of checks on the inputs
    if (.not. (local_bounds % is_valid() .and. global_bounds % is_valid())) then
      print '(A)', 'ERROR: Invalid grid size specified when sending data'
      print '(2L2)', local_bounds % is_valid(), global_bounds % is_valid()
      return
    end if

    call this % messenger % bump_tag()

    !----------------------
    ! Prepare the message

    ! Prepare accompanying command
    command_meta % size_int8    = 0
    command_meta % command_type = MSG_COMMAND_DATA
    command_meta % stream_id    = this % stream_id
    command_meta % message_tag  = this % messenger % get_msg_tag()
    if (present(command)) command_meta % size_int8 = command % get_top()

    ! Prepare record that describes the data
    rec % command_size_int8 = command_meta % size_int8 + command_record_size_int8()

    rec % tag            = this % messenger % get_msg_tag()
    rec % stream         = this % stream_id

    rec % global_bounds = global_bounds
    rec % local_bounds  = local_bounds % intersection(global_bounds)

    rec % elem_size      = data_info % get_kind()
    rec % heap_offset    = data_info % get_offset()
    rec % data_size_byte = rec % local_bounds % compute_num_elements() * rec % elem_size
    
    ! call print_data_record(rec)

    ! Don't send anything if the entire tile will be cropped
    if (.not. rec % local_bounds % is_valid()) then
      success = .true.
      return
    end if

    ! Prepare header for the entire message
    header % content_size_int8  = data_record_size_int8() + command_record_size_int8() + command_meta % size_int8
    header % command            = MSG_COMMAND_DATA
    header % stream_rank        = this % stream_rank
    header % stream_id          = this % stream_id
    header % message_tag        = command_meta % message_tag
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_size_int8

    !-------------------
    ! Send the message

    ! Send header
    success = this % server_bound_cb % put(                                                                       &
        header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_data_timeout_ms)

    ! Send data
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          rec, data_record_size_byte(), CB_KIND_CHAR, .false., timeout_ms = this % put_data_timeout_ms)
    end if

    ! Send the associated command
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          command_meta, command_record_size_byte(), CB_KIND_CHAR, .false.,                                        &
          timeout_ms = this % put_data_timeout_ms)
    end if

    ! Send command content (if needed)
    if (present(command) .and. success) then
      success = this % server_bound_cb % put(                                                                     &
          command % f_array(), command_meta % size_int8, CB_DATA_ELEMENT_KIND, .false.,                           &
          timeout_ms = this % put_data_timeout_ms)
    end if

    ! Add the end cap and commit the message
    if (success) then
      success = this % server_bound_cb % put(                                                                     &
          end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = this % put_data_timeout_ms)
    end if

  end function send_data

  function model_stream_read(this) result(status)
    implicit none
    class(model_stream), intent(INOUT) :: this
    integer :: status

    status = -1
    if (this % stream_id <= 0) return
    call this % messenger % bump_tag()

    status = 0
  end function model_stream_read
end module model_stream_module
