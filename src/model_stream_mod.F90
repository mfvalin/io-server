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
  use heap_module
  use ioserver_message_module
  use jar_module
  use rpn_extra_module
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
    type(heap)            :: local_heap             !< Access to the shared memory heap owned by this model PE
    type(circular_buffer) :: server_bound_cb        !< Access to the server-bound buffer owned by this model PE
    type(ioserver_messenger), pointer :: messenger => NULL() !< Messenger used to manage/synchronized data transmission to the server

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

  function new_model_stream(global_rank, model_rank, stream_rank, local_heap, server_bound_cb, debug_mode, messenger)
    implicit none
    integer,                intent(in)  :: global_rank
    integer,                intent(in)  :: model_rank
    integer,                intent(in)  :: stream_rank
    type(heap),             intent(in)  :: local_heap
    type(circular_buffer),  intent(in)  :: server_bound_cb
    logical,                intent(in)  :: debug_mode
    type(ioserver_messenger), pointer, intent(in) :: messenger
    type(model_stream) :: new_model_stream

    if (.not. server_bound_cb % is_valid(.false.)) then
      print *, 'Server-bound CB has not been initialized!'
      return
    end if

    new_model_stream % global_rank = global_rank
    new_model_stream % model_rank  = model_rank
    new_model_stream % stream_rank = stream_rank
    new_model_stream % local_heap  = local_heap
    new_model_stream % server_bound_cb = server_bound_cb
    new_model_stream % debug = debug_mode
    new_model_stream % messenger => messenger
    
    ! print *, 'Creating model stream, rank ', stream_rank

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
                 this % server_bound_cb % is_valid(.false.) .and. &
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
    if (.not. this % server_bound_cb % is_valid(.true.)) return

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

    success = this % server_bound_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false.)
    success = this % server_bound_cb % put(command_header, command_record_size_byte(), CB_KIND_CHAR, .false.)                    .and. success
    success = this % server_bound_cb % put(command_content % f_array(), command_header % size_int8, CB_DATA_ELEMENT_KIND, .false.) .and. success
    success = this % server_bound_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)                               .and. success
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

    success = this % server_bound_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false.)
    success = this % server_bound_cb % put(command, command_record_size_byte(), CB_KIND_CHAR, .false.)  .and. success
    success = this % server_bound_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)      .and. success   ! Append cap and commit message

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
    
    success = this % server_bound_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false.)
    success = this % server_bound_cb % put(command, command_record_size_byte(), CB_KIND_CHAR, .false.)  .and. success
    success = this % server_bound_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)      .and. success ! Append cap + commit

    this % stream_id = -1

    ! call print_message_header(header)

  end function model_stream_close

  ! cprs and meta only need to be supplied by one of the writing PEs
  function send_data(this, my_data, subgrid_area, global_grid, grid_out, command, cprs, meta) result(success)
    use iso_c_binding
    use jar_module
    implicit none
    class(model_stream),  intent(INOUT) :: this
    type(block_meta_f08), intent(IN)    :: my_data          !< array descriptor from h % allocate
    type(subgrid_t),      intent(IN)    :: subgrid_area     !< area of this subgrid in global grid
    type(grid_t),         intent(IN)    :: global_grid      !< global grid info
    type(grid_t),         intent(IN)    :: grid_out         !< output grid

    type(jar),   intent(IN), optional :: command          !< Command to apply to the data by the model once assembled
    type(cmeta), intent(IN), optional :: cprs             !< compression related metadata (carried serialized)
    type(jar),   intent(IN), optional :: meta             !< metadata associated with data (carried serialized and blindly)

    logical :: success  !< Whether this function call succeeded

    type(data_record)     :: rec
    type(message_header)  :: header
    type(message_cap)     :: end_cap
    type(command_record)  :: command_meta
    integer(JAR_ELEMENT)  :: low, high
    integer(JAR_ELEMENT), dimension(:), pointer :: metadata

    success = .false.
    if(this % stream_id <= 0) return

    ! Perform a short series of checks on the inputs
    block
      ! Check that dimensions in area are consistent with metadata
      if (.not. all(my_data % get_dimensions() == subgrid_area % size)) then
        print '(A, 5I4, 1X, 5I4)', 'ERROR: Trying to send data array that does not match subgrid dimensions', my_data % get_dimensions(), subgrid_area % size
        return
      end if

      ! Check that the given offset of the subgrid can fit within the global grid
      if (any(subgrid_area % offset < 1) .or. any(subgrid_area % offset > global_grid % size)) then
        print '(A, 5I4)', 'ERROR: Subgrid offset is invalid!', subgrid_area % offset
        return
      end if
    end block

    ! Check that given element size is the same as in the global grid
    if (my_data % get_kind() .ne. global_grid % elem_size) then
      print '(A, 1X, I2, 1X, I2)', 'WARNING: Element size mentioned in global grid does not match data type', global_grid % elem_size, my_data % get_kind()
    end if

    call this % messenger % bump_tag()

    !----------------------
    ! Prepare the message

    ! Size of the various metadata objects included
    rec % cmeta_size = 0
    rec % meta_size  = 0
    if(present(cprs)) then
      rec % cmeta_size = int(cmeta_size_int8(), kind=4)
    endif
    if(present(meta)) then
      low  = meta % get_bot()
      high = meta % get_top()
      rec % meta_size = high - low       ! useful number of elements
      metadata => meta % f_array()
    endif

    command_meta % size_int8    = 0
    command_meta % command_type = MSG_COMMAND_DATA
    command_meta % stream_id    = this % stream_id
    command_meta % message_tag  = this % messenger % get_msg_tag()
    if (present(command)) command_meta % size_int8 = command % get_top()
    rec % command_size_int8 = command_meta % size_int8 + command_record_size_int8()

    rec % tag            = this % messenger % get_msg_tag()
    rec % stream         = this % stream_id

    rec % subgrid_area   = subgrid_area
    rec % global_grid    = global_grid
    rec % global_grid % elem_size = my_data % get_kind()

    rec % output_grid_id = grid_out % id

    rec % elem_size      = my_data % get_kind()
    rec % heap_offset    = my_data % get_offset()
    rec % data_size_byte = product(rec % subgrid_area % size) * rec % elem_size

    ! print *, rec % ni, rec % nj, rec % nk, rec % nvar, f_block % k()

    ! print *, 'From model'
    ! print *, 'Area % nv = ', area % nv
    ! call print_data_record(rec)

    header % content_size_int8  = data_record_size_int8() + rec % cmeta_size + rec % meta_size + command_record_size_int8() + command_meta % size_int8
    header % command            = MSG_COMMAND_DATA
    header % stream_rank        = this % stream_rank
    header % stream_id          = this % stream_id
    header % message_tag        = command_meta % message_tag
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_size_int8

    !-------------------
    ! Send the message

    ! Put header + data
    success = this % server_bound_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false.)
    success = this % server_bound_cb % put(rec, data_record_size_byte(), CB_KIND_CHAR, .false.) .and. success

    success = this % server_bound_cb % put(command_meta, command_record_size_byte(), CB_KIND_CHAR, .false.) .and. success
    if (present(command)) success = this % server_bound_cb % put(command % f_array(), command_meta % size_int8, CB_DATA_ELEMENT_KIND, .false.) .and. success

    ! Optional parts of the message
    if(present(cprs)) success = this % server_bound_cb % put(cprs, int(rec % cmeta_size, kind=8), CB_KIND_INTEGER_8, .false.) .and. success
    if(present(meta)) success = this % server_bound_cb % put(metadata(low + 1 : high), int(rec % meta_size, kind=8), CB_KIND_INTEGER_8, .false.) .and. success

    ! Add the end cap and commit the message
    success = this % server_bound_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.) .and. success

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
