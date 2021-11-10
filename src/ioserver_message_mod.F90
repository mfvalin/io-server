
module ioserver_message_module
  use ISO_C_BINDING
  use ioserver_constants
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

  type, public :: grid
    integer :: id             ! grid id
    integer :: size_i, size_j ! horizontal dimensions of full grid
  end type

  type, public :: subgrid
    integer :: i0            ! starting point of this subgrid in the x direction
    integer :: ni            ! number of points of this subgrid in the x direction
    integer :: j0            ! starting point of this subgrid in the y direction
    integer :: nj            ! number of points of this subgrid in the y direction
    integer :: nk            ! number of vertical levels
    integer :: nv            ! number of variables
  end type

  type, private :: data_header        ! record : data_header , metadata(nm integers) , subgrid(ni * nj * nk * nv elements)
    integer :: nw            ! number of elements in record
    integer :: nbits         ! number of bits per subgrid element in record
    integer :: tag           ! unique sequence tag
    integer :: weight        ! number of pieces in parcel (normally 1)
    integer :: np            ! number of pieces to reassemble
    integer :: grid          ! grid id
    type(subgrid) :: s       ! subgrid description
    integer :: nm            ! length of metadata "jar" (32 bit units)
  end type

  ! Type used as a header when writing data (from a model process)
  type, public :: model_record
    integer(C_INT) :: record_length    ! global record length = size of model_record + csize + msize
    integer(C_INT) :: tag, stream
    integer(C_INT) :: ni, nj
    integer(C_INT) :: grid_size_i, grid_size_j, gnignj
    integer(C_INT) :: output_grid
    integer(C_INT) :: i0, j0
    integer(C_INT) :: nk, nvar
    integer(C_INT) :: type_kind_rank
    type(C_PTR)    :: data
    integer(C_INT) :: csize, msize
  end type model_record

  type, public :: message_header
    integer(C_INT) :: length      = -1
    integer(C_INT) :: command     = -1
    integer(C_INT) :: stream_id   = -1
    integer(C_INT) :: tag         = -1
    integer(C_INT) :: sender_global_rank = -1
  end type message_header

  integer, parameter, public :: MSG_COMMAND_DATA        = 0
  integer, parameter, public :: MSG_COMMAND_DUMMY       = 1
  integer, parameter, public :: MSG_COMMAND_OPEN_FILE   = 2
  integer, parameter, public :: MSG_COMMAND_CLOSE_FILE  = 3
  integer, parameter, public :: MSG_COMMAND_MODEL_STOP  = 4
  integer, parameter, public :: MSG_COMMAND_RELAY_STOP  = 5
  integer, parameter, public :: MSG_COMMAND_ACKNOWLEDGE = 6

  public :: message_header_size_int, model_record_size_int, cmeta_size_int, send_server_bound_message, message_header_print
contains

  function message_header_size_int()
    implicit none
    integer(C_INT64_T) :: message_header_size_int

    type(message_header) :: dummy_header
    integer(kind=4) :: dummy_int

    message_header_size_int = storage_size(dummy_header) / storage_size(dummy_int)
  end function message_header_size_int

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

    success = .false.
    call this % bump_tag()

    header % length  = msg_size
    header % command = MSG_COMMAND_DUMMY
    header % tag     = this % msg_tag_seq

    success = buffer % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = buffer % put(message, int(msg_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    success = buffer % put(msg_size, 1_8, CB_KIND_INTEGER_4, .true.) .and. success

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

  subroutine message_header_print(header)
    implicit none
    type(message_header), intent(in) :: header
    print '(A, I8, A, I3, A, I3, A, I8, A, I5)', &
      'Header: len ', header % length, ', cmd ', header % command, ', stream ', header % stream_id, ', tag ', header % tag, ', rank ', header % sender_global_rank
  end subroutine message_header_print

end module ioserver_message_module
