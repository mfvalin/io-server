module model_stream_module
  use circular_buffer_module
  use heap_module
  use ioserver_message_module
  implicit none

  private

#if ! defined(VERSION)
#define VERSION 10000
#endif

  type, public :: model_stream
    private
    integer               :: version = VERSION    ! version marker, used to check version coherence
    integer               :: stream_id = -1       ! Stream ID, for internal use
    character(len=1), dimension(:), pointer :: name => NULL()
    logical               :: debug = .false.      ! debug mode at the file level
    integer               :: global_rank = -1
    type(heap)            :: local_heap
    type(circular_buffer) :: server_bound_cb
    type(ioserver_messenger), pointer :: messenger => NULL()

    contains

    ! initial, pass :: server_file_construct
    procedure   :: open
    procedure   :: read
    procedure   :: write
    procedure   :: close
    procedure   :: is_open
    procedure   :: is_version_valid
    procedure   :: set_debug

  end type

  interface model_stream
    procedure :: new_model_stream
  end interface model_stream

contains

  function new_model_stream(global_rank, local_heap, server_bound_cb, debug_mode, messenger)
    implicit none
    integer,                intent(in)  :: global_rank
    type(heap),             intent(in)  :: local_heap
    type(circular_buffer),  intent(in)  :: server_bound_cb
    logical,                intent(in)  :: debug_mode
    type(ioserver_messenger), pointer, intent(in) :: messenger
    type(model_stream) :: new_model_stream

    new_model_stream % global_rank = global_rank
    new_model_stream % local_heap  = local_heap
    new_model_stream % server_bound_cb = server_bound_cb
    new_model_stream % debug = debug_mode
    new_model_stream % messenger => messenger
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

  function is_open(this) result(status)
    implicit none
    class(model_stream), intent(IN) :: this
    logical :: status

    status = this % is_version_valid()
    status = status .and. (this % stream_id > 0) 
    status = status .and. associated(this % name)

  end function is_open

  function open(this, name) result(success)
    implicit none
    class(model_stream),    intent(INOUT) :: this
    character(len=*),      intent(IN)    :: name
    logical :: success

    integer :: lname
    type(message_header) :: header
    type(message_cap)    :: end_cap

    success = .false.

    if (.not. this % is_version_valid()) return ! wrong version
    if (this % is_open())                return ! already open

    if (.not. this % server_bound_cb % is_valid()) then
      print *, 'Server file has not been initialized!'
      return
    end if

    call this % messenger % bump_tag(.true.)

    this % stream_id = this % messenger % get_file_tag()

    lname = len(trim(name)) + 1
    allocate(this % name(lname))
    this % name(1:lname) = transfer(trim(name) // char(0), this % name) ! Append a NULL character because this might be read in C code

    header % content_length     = lname
    header % command            = MSG_COMMAND_OPEN_FILE
    header % stream_id          = this % stream_id
    header % tag                = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(this % name, int(lname, kind=8), CB_KIND_CHAR, .false.) .and. success
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success       ! Append length and commit message

  end function open

  function close(this) result(success)
    implicit none
    class(model_stream), intent(INOUT) :: this
    logical :: success

    type(message_header) :: header
    type(message_cap)    :: end_cap

    success = .false.
    if (.not. this % is_open()) return

    call this % messenger % bump_tag()

    header % content_length     = 0
    header % stream_id          = this % stream_id
    header % tag                = this  % messenger % get_msg_tag()
    header % command            = MSG_COMMAND_CLOSE_FILE
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    this % stream_id = -1
    deallocate(this % name)
    nullify(this % messenger)
    
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.)  .and. success ! Append length + commit

    ! call print_message_header(header)

  end function close

  ! cprs and meta only need to be supplied by one of the writing PEs
  function write(this, mydata, area, grid_in, grid_out, cprs, meta) result(success)
    use jar_module
    implicit none
    class(model_stream), intent(INOUT) :: this
    type(block_meta),   intent(IN)    :: mydata           ! array descriptor from h % allocate
    type(subgrid),      intent(IN)    :: area             ! area in global space
    type(grid),         intent(IN)    :: grid_in          ! input grid
    type(grid),         intent(IN)    :: grid_out         ! output grid

    type(cmeta), intent(IN), optional :: cprs             ! compression related metadata (carried serialized)
    type(jar),   intent(IN), optional :: meta             ! metadata associated with data (carried serialized and blindly)

    logical :: success

    type(model_record)   :: rec
    type(message_header) :: header
    type(message_cap)    :: end_cap
    integer(JAR_ELEMENT), dimension(:), pointer :: metadata
    integer(JAR_ELEMENT) :: low, high
    type(C_PTR) :: p
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: d
    integer(C_INT) :: tkr
    integer(C_SIZE_T) :: o
    logical :: dim_ok

    type(block_meta_f08) :: f_block

    success = .false.
    if(this % stream_id <= 0) return

    call block_meta_internals(mydata, p, d, tkr, o)        ! grep block_meta private contents
    ! check that dimensions in area are consistent with metadata
    dim_ok = d(1) == area % ni .and. d(2) == area % nj .and. d(3) == area % nk .and. d(4) == area % nv

    if(.not. dim_ok) return

    call this % messenger % bump_tag()

    !----------------------
    ! Prepare the message

    ! Size of the various metadata objects included
    rec % cmeta_size = 0
    rec % meta_size  = 0
    if(present(cprs)) then
      rec % cmeta_size = int(cmeta_size_int(), kind=4)
    endif
    if(present(meta)) then
      low  = meta % low()
      high = meta % high()
      rec % meta_size = high - low       ! useful number of elements
      metadata => meta % array()
    endif

    f_block = mydata

    ! call f_block % print()

    rec % tag            = this % messenger % get_msg_tag()
    rec % stream         = this % stream_id

    rec % ni             = area % ni
    rec % nj             = area % nj
    rec % grid_size_i    = grid_in % size_i
    rec % grid_size_j    = grid_in % size_j
    rec % gnignj         = grid_in % size_i * grid_in % size_j
    rec % output_grid_id = grid_out % id
    rec % i0             = area % i0
    rec % j0             = area % j0
    rec % nk             = area % nk
    rec % nvar           = area % nv
    rec % type_kind_rank = tkr

    rec % data           = p
    rec % data_size_byte = rec % ni * rec % nj * rec % nk * rec % nvar * f_block % k()

    ! print *, rec % ni, rec % nj, rec % nk, rec % nvar, f_block % k()

    ! print *, 'From model'
    ! print *, 'Area % nv = ', area % nv
    ! call print_model_record(rec)

    header % content_length     = int(model_record_size_int() + rec % cmeta_size + rec % meta_size, kind=4)
    header % command            = MSG_COMMAND_DATA
    header % stream_id          = this % stream_id
    header % tag                = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    !-------------------
    ! Send the message

    ! Put header + data
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(rec, model_record_size_int(), CB_KIND_INTEGER_4, .false.) .and. success

    ! Optional parts of the message
    if(present(cprs)) success = this % server_bound_cb % put(cprs, int(rec % cmeta_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    if(present(meta)) success = this % server_bound_cb % put(metadata(low + 1 : high), int(rec % meta_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success

    ! Add the end cap and commit the message
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success
  end function write

  function read(this) result(status)
    implicit none
    class(model_stream), intent(INOUT) :: this
    integer :: status

    status = -1
    if (this % stream_id <= 0) return
    call this % messenger % bump_tag()

    status = 0
  end function read
end module model_stream_module
