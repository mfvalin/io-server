
module ioserver_message_module
  use ISO_C_BINDING
  use heap_module, only: MAX_ARRAY_RANK
  use ioserver_constants
  use jar_module
  implicit none
  private

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
    procedure, pass :: send_server_bound_message
  end type ioserver_messenger

  integer, parameter :: MAXPACK = 16
  type, public :: cmeta                        ! information passed to compression software
    real(kind=8)    :: errabs_r = 0.0
    integer(kind=8) :: errabs_i = 0
    real(kind=4)    :: errrel_r = 0.0
    integer         :: errrel_i = 0
    integer         :: nbits    = 0
    integer, dimension(MAXPACK) :: pack_info
  end type

  type, public, bind(C) :: grid_t
    integer(C_INT) :: id = -1
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: size = 1 !< Number of elements in the grid in each possible dimension
    integer(C_INT) :: elem_size = -1  !< Size of each grid element in bytes
  end type grid_t

  type, public, bind(C) :: subgrid_t
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: size   = 1  !< Number of elements of the subgrid in each possible dimension
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: offset = 1  !< Offset of the subgrid within the larger one in each possible dimension
    ! integer(C_INT) :: elem_size = -1  !< Size of each grid element in bytes
  end type subgrid_t

  ! Type used as a header when writing data to a stream from a model process
  type, public, bind(C) :: model_record
    type(C_PTR)    :: data              !< Will be translated to its own memory space by relay. We want it to be 64-bit aligned!!
    integer(C_INT) :: data_size_byte    !< Size of the data packet itself, in bytes
    integer(C_INT) :: cmeta_size        !< Size of the compression metadata included
    integer(JAR_ELEMENT) :: meta_size   !< Size of other metadata included

    integer(C_INT) :: tag               !< Tag associated with this particular message (to be able to group with that of other model PEs)
    integer(C_INT) :: stream            !< Stream to which the data is being sent

    type(subgrid_t) :: subgrid_area
    type(grid_t)    :: global_grid

    integer(C_INT) :: elem_size         !< Size of the grid elements in bytes
    integer(C_INT) :: output_grid_id    !< ID of the grid where the data is being sent
  end type model_record

  integer(C_INT), parameter, public :: MSG_HEADER_TAG = 1010101 !< First entry of every message. Helps debugging
  integer(C_INT), parameter, public :: MSG_CAP_TAG    =  101010 !< (Second-to-)Last entry of every message. Helps debugging
  type, public, bind(C) :: message_header
    integer(C_INT) :: header_tag      = MSG_HEADER_TAG !< Signals the start of a message. Gotta be the first item
    integer(C_INT) :: content_length  = -1    !< Message length (excluding this header). Units depend on content of message
    integer(C_INT) :: command         = -1    !< What this message contains
    integer(C_INT) :: stream_id       = -1    !< To what stream this message is destined
    integer(C_INT) :: tag             = -1    !< A collective tag associated with messages from model processes (incremented at every message)
    integer(C_INT) :: sender_global_rank = -1 !< Who is sending that message
    integer(C_INT) :: relay_global_rank = -1  !< Who is transmitting the message
  end type message_header

  type, public, bind(C) :: message_cap
    integer(C_INT) :: cap_tag    = MSG_CAP_TAG !< Signals the end of a message
    integer(C_INT) :: msg_length = -1          !< Length of the message that just ended. Gotta match the length indicated in the message header
  end type message_cap

  integer, parameter, public :: MSG_COMMAND_DATA        = 0 !< Indicate a message that contains grid data
  integer, parameter, public :: MSG_COMMAND_DUMMY       = 1 !< Indicate a message without content or purpose
  integer, parameter, public :: MSG_COMMAND_OPEN_FILE   = 2 !< Indicate a message that wants to open a file
  integer, parameter, public :: MSG_COMMAND_CLOSE_FILE  = 3 !< Indicate a message that wants to close a file
  integer, parameter, public :: MSG_COMMAND_MODEL_STOP  = 4 !< Indicate that the model that sends this message will no longer send anything
  integer, parameter, public :: MSG_COMMAND_RELAY_STOP  = 5 !< Indicate that the relay that sends this message will no longer send anything
  integer, parameter, public :: MSG_COMMAND_ACKNOWLEDGE = 6 !< Indicate a message without content, but with the purpose to acknowledge something?

  public :: message_header_size_int, message_cap_size_int, model_record_size_int, cmeta_size_int, send_server_bound_message, print_message_header, print_model_record
contains

  function message_header_size_int()
    implicit none
    integer(C_INT64_T) :: message_header_size_int

    type(message_header) :: dummy_header
    integer(kind=4) :: dummy_int

    message_header_size_int = storage_size(dummy_header) / storage_size(dummy_int)
  end function message_header_size_int

  function message_cap_size_int()
    implicit none
    integer(C_INT64_T) :: message_cap_size_int

    type(message_cap) :: dummy_cap
    integer(kind=4)   :: dummy_int

    message_cap_size_int = storage_size(dummy_cap) / storage_size(dummy_int)
  end function message_cap_size_int

  function model_record_size_int()
    implicit none
    integer(C_INT64_T) :: model_record_size_int

    type(model_record) :: dummy_record
    integer(kind=4) :: dummy_int

    model_record_size_int = storage_size(dummy_record) / storage_size(dummy_int)
  end function model_record_size_int

  function cmeta_size_int()
    implicit none
    integer(C_INT64_T) :: cmeta_size_int

    type(cmeta) :: dummy_cmeta
    integer(kind=4) :: dummy_int

    cmeta_size_int = storage_size(dummy_cmeta) / storage_size(dummy_int)
  end function cmeta_size_int

  subroutine bump_tag(this, new_file)
    implicit none
    class(ioserver_messenger), intent(inout) :: this
    logical, intent(in), optional :: new_file

    if (this % debug_mode) call MPI_Barrier(this % model_crs % comm)    ! in debug mode, enforce collective mode
    this % msg_tag_seq = this % msg_tag_seq + 1

    if (present(new_file)) then
      if (new_file) this % file_open_seq = this % file_open_seq + 1
    end if
  end subroutine bump_tag

  function get_msg_tag(this) result(tag)
    implicit none
    class(ioserver_messenger), intent(in) :: this
    integer :: tag
    tag = this % msg_tag_seq
  end function get_msg_tag

  function get_file_tag(this) result(tag)
    implicit none
    class(ioserver_messenger), intent(in) :: this
    integer :: tag
    tag = this % file_open_seq
  end function get_file_tag

  function send_server_bound_message(this, message, msg_size, buffer) result(success)
    use circular_buffer_module
    implicit none
    class(ioserver_messenger),    intent(inout) :: this
    integer,                      intent(in)    :: msg_size
    integer, dimension(msg_size), intent(in)    :: message
    type(circular_buffer),        intent(inout) :: buffer

    logical :: success

    type(message_header)  :: header
    type(message_cap)     :: end_cap

    success = .false.
    call this % bump_tag()

    header % content_length = msg_size
    header % command        = MSG_COMMAND_DUMMY
    header % tag            = this % msg_tag_seq

    end_cap % msg_length = header % content_length

    success = buffer % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = buffer % put(message, int(msg_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    success = buffer % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success

  end function send_server_bound_message

  subroutine set_debug(this, debug_mode)
    implicit none
    class(ioserver_messenger), intent(inout) :: this
    logical, intent(in) :: debug_mode
    this % debug_mode = debug_mode
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
    character(len=24) :: command_string
    select case (command)
    case (MSG_COMMAND_DATA)
      command_string = 'MSG_COMMAND_DATA'
    case (MSG_COMMAND_ACKNOWLEDGE)
      command_string = 'MSG_COMMAND_ACKNOWLEDGE'
    case (MSG_COMMAND_CLOSE_FILE)
      command_string = 'MSG_COMMAND_CLOSE_FILE'
    case (MSG_COMMAND_DUMMY)
      command_string = 'MSG_COMMAND_DUMMY'
    case (MSG_COMMAND_MODEL_STOP)
      command_string = 'MSG_COMMAND_MODEL_STOP'
    case (MSG_COMMAND_OPEN_FILE)
      command_string = 'MSG_COMMAND_OPEN_FILE'
    case (MSG_COMMAND_RELAY_STOP)
      command_string = 'MSG_COMMAND_RELAY_STOP'
    case default
      command_string = '[ERROR] unknown number'
    end select
  end function get_message_command_string

  subroutine print_message_header(header)
    implicit none
    type(message_header), intent(in) :: header
    print '(A, I8, A, I8, A, I3, 1X, A, A, I3, A, I8, A, I5, A, I5)', &
      'Header: header tag ', header % header_tag, &
      ', len ', header % content_length, &
      ', cmd ', header % command, get_message_command_string(header % command), &
      ', stream ', header % stream_id, &
      ', tag ', header % tag, &
      ', rank ', header % sender_global_rank, &
      ', relay rank ', header % relay_global_rank
  end subroutine print_message_header

  subroutine print_model_record(record)
    implicit none
    type(model_record), intent(in) :: record

    print *, 'print_model_record() not implemented', record % tag
    ! print '(A12,   A, I6, I4, I4,   A, I7, I4,   A, I4, I4, I4, I3, I7,   A, I5, I5, I6, I6, I8, I4)', 'Record info: ', &
    !   'sizes ', record % data_size_byte, record % cmeta_size, record % meta_size, &
    !   ', tag+stream ', record % tag, record % stream, &
    !   ', dimensions ', record % ni, record % nj, record % nk, record % nvar, record % type_kind_rank, &
    !   ', global grid ', record % i0, record % j0, record % grid_size_i, record % grid_size_j, record % gnignj, record % output_grid_id

  end subroutine print_model_record

end module ioserver_message_module
