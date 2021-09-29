
module ioserver_file_module
  use circular_buffer_module
  use ioserver_constants
  use ioserver_message_module
  use shmem_heap
  use simple_mutex_module
  implicit none
#if ! defined(VERSION)
#define VERSION 10000
#endif

  save
  private

  public :: block_meta, subgrid, grid, cmeta

  type, public :: server_file
    private
    integer               :: version = VERSION    ! version marker, used to check version coherence
    integer               :: stream_id = -1       ! File ID, for internal use
    character(len=1), dimension(:), pointer :: name => NULL()
    logical               :: debug = .false.      ! debug mode at the file level
    integer               :: global_rank = -1
    type(heap)            :: local_heap
    type(circular_buffer) :: server_bound_cb
    type(ioserver_messenger), pointer :: messenger => NULL()

    contains

    ! initial, pass :: server_file_construct
    procedure   :: open      => server_file_open
    procedure   :: read      => server_file_read
    procedure   :: write     => server_file_write
    procedure   :: close     => server_file_close
    procedure   :: is_open   => server_file_is_open
    procedure   :: is_valid  => server_file_is_version_valid
    procedure   :: set_debug => server_file_set_debug

  end type

  interface server_file
    procedure :: new_server_file
  end interface server_file


  type, public :: stream_file
    private
    integer :: stream_id = -1       ! File ID, for internal use
    character(len=:), allocatable :: name
    integer :: unit = -1

    type(simple_mutex) :: mutex

    contains
    procedure :: open    => stream_file_open
    procedure :: is_open => stream_file_is_open
    final     :: stream_file_finalize
  end type stream_file

contains

  function new_server_file(global_rank, local_heap, server_bound_cb, debug_mode, messenger)
    implicit none
    ! class(server_file),     intent(out) :: this
    integer,                intent(in)  :: global_rank
    type(heap),             intent(in)  :: local_heap
    type(circular_buffer),  intent(in)  :: server_bound_cb
    logical,                intent(in)  :: debug_mode
    type(ioserver_messenger), pointer, intent(in) :: messenger
    type(server_file) :: new_server_file

    new_server_file % global_rank = global_rank
    new_server_file % local_heap  = local_heap
    new_server_file % server_bound_cb = server_bound_cb
    new_server_file % debug = debug_mode
    new_server_file % messenger => messenger
  end function new_server_file

  function server_file_set_debug(this, dbg) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    logical, intent(IN) :: dbg
    logical :: status

    status = this % debug
    this % debug = dbg

  end function server_file_set_debug

  function server_file_is_version_valid(this) result(status)
    implicit none
    class(server_file), intent(IN) :: this
    logical :: status

    status = this % version == VERSION

  end function server_file_is_version_valid

  function server_file_is_open(this) result(status)
    implicit none
    class(server_file), intent(IN) :: this
    logical :: status

    status = this % is_valid()
    status = status .and. (this % stream_id > 0) 
    status = status .and. associated(this % name)

  end function server_file_is_open

  function server_file_open(this, name) result(status)
    implicit none
    class(server_file),    intent(INOUT) :: this
    character(len=*),      intent(IN)    :: name
    integer :: status

    integer :: lname
    type(message_header) :: header
    logical :: success

    status = -1
    success = .false.

    if (.not. this % is_valid()) return ! wrong version
    if (this % is_open())        return ! already open

    if (.not. this % server_bound_cb % is_valid()) then
      print *, 'Server file has not been initialized!'
      return
    end if

    call this % messenger % bump_tag(.true.)

    this % stream_id = this % messenger % get_file_tag()

    lname = len(trim(name)) + 1
    allocate(this % name(lname))
    this % name(1:lname) = transfer(trim(name) // char(0), this % name) ! Append a NULL character because this might be read in C code

    header % length  = lname
    header % command = MSG_COMMAND_OPEN_FILE
    header % stream  = this % stream_id
    header % tag     = this % messenger % get_msg_tag()

    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(this % name, int(lname, kind=8), CB_KIND_CHAR, .false.) .and. success
    success = this % server_bound_cb % put(lname, 1_8, CB_KIND_INTEGER_4, .true.) .and. success       ! Append length and commit message

    if (success) status = 0
  end function server_file_open

  function server_file_close(this) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    integer :: status

    status = -1
    if(this % stream_id <= 0)                return    ! not open
    if(.not. associated(this % name)) return    ! not open

    call this % messenger % bump_tag()
    this % stream_id = -1
    deallocate(this % name)
    status = 0
  end function server_file_close

  ! cprs and meta only need to be supplied by one of the writing PEs
  function server_file_write(this, mydata, area, grid_in, grid_out, cprs, meta) result(status)
    use data_serialize
    implicit none
    class(server_file), intent(INOUT) :: this
    type(block_meta),   intent(IN)    :: mydata           ! array descriptor from h % allocate
    type(subgrid),      intent(IN)    :: area             ! area in global space
    type(grid),         intent(IN)    :: grid_in          ! input grid
    type(grid),         intent(IN)    :: grid_out         ! output grid

    type(cmeta), intent(IN), optional :: cprs             ! compression related metadata (carried serialized)
    type(jar),   intent(IN), optional :: meta             ! metadata associated with data (carried serialized and blindly)

    integer :: status
    type(model_record)   :: rec
    type(message_header) :: header
    integer(C_INT), dimension(:), pointer :: metadata
    integer(C_INT) :: low, high
    type(C_PTR) :: p
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: d
    integer(C_INT) :: tkr
    integer(C_SIZE_T) :: o
    logical :: dim_ok, success

    status = -1
    if(this % stream_id <= 0) return

    ! transmission layout for circular buffer (compute PE -> relay PE) :
    ! length                 (32 bits)
    ! tag number             (32 bits)
    ! file (stream) number   (32 bits)
    ! grid segment dimensions(2 x32 bits)
    ! input grid size (i,j)  (2 x 32 bits)
    ! global grid size       (32 bits)
    ! grid_out (code)        (32 bits)
    ! area lower left corner (2 x 32 bits)
    ! nk, nvar               (2 x 32 bits)
    ! array type/kind/rank   (32 bits)
    ! data pointer           (64 bits)     (will be translated to its own memory space by relay PE)
    ! cprs size , may be 0   (32 bits)
    ! meta size, may be 0    (32 bits)
    ! cprs                   (cprs size x 32 bits)
    ! meta                   (meta size x 32 bits)
    ! length                 (32 bits)
!
    call block_meta_internals(mydata, p, d, tkr, o)        ! grep block_meta private contents
    ! check that dimensions in area are consistent with metadata
    dim_ok = d(1) == area % ni .and. d(2) == area % nj .and. d(3) == area % nk .and. d(4) == area % nv

    if(.not. dim_ok) return

    call this % messenger % bump_tag()
    rec % csize = 0
    rec % msize = 0
    if(present(cprs)) then
      rec % csize = int(cmeta_size_int(), kind=4)
    endif
    if(present(meta)) then
      low = meta % low()
      high = meta % high()
      rec % msize =  high - low       ! useful number of elements
      metadata    => meta % array()
    endif
    rec % record_length = int(model_record_size_int(), kind=4)
    rec % record_length = rec % record_length + rec % csize
    rec % record_length = rec % record_length + rec % msize
    rec % tag           = this % messenger % get_msg_tag()
    rec % stream        = this % stream_id
    rec % ni            = area % ni
    rec % nj            = area % nj
    rec % grid_size_i   = grid_in % size_i
    rec % grid_size_j   = grid_in % size_j
    rec % gnignj        = grid_in % size_i * grid_in % size_j
    rec % output_grid   = grid_out % id
    rec % i0            = area % i0
    rec % j0            = area % j0
    rec % nk            = area % nk
    rec % nvar          = area % nv
    rec % type_kind_rank = tkr
    rec % data          = p

    header % length  = rec % record_length
    header % command = MSG_COMMAND_DATA
    header % stream  = this % stream_id
    header % tag     = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank
!
    success = .true.
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.) .and. success
    success = this % server_bound_cb % put(rec, model_record_size_int(), CB_KIND_INTEGER_4, .false.)      .and. success
    if(present(cprs)) success = this % server_bound_cb % put(cprs, int(rec % csize, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    ! only send useful part of metadata jar (if supplied)
    if(present(meta)) success = this % server_bound_cb % put(metadata(low + 1 : high), int(rec % msize, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    success = this % server_bound_cb % put(rec % record_length, 1_8, CB_KIND_INTEGER_4, .true.) .and. success

    if (success) status = 0
  end function server_file_write

  function server_file_read(this) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    integer :: status

    status = -1
    if (this % stream_id <= 0) return
    call this % messenger % bump_tag()

    status = 0
  end function server_file_read

  function stream_file_open(this, stream_id, file_name) result(success)
    implicit none
    class(stream_file), intent(inout) :: this
    integer,            intent(in)    :: stream_id
    character(len=*),   intent(in)    :: file_name
    logical :: success

    success = .false.
    if (.not. this % is_open()) then
      this % name = trim(file_name) // '.out'
      print *, 'Opening file, name = ', this % name

      open(newunit = this % unit, file = this % name, status = 'replace', form = 'unformatted')
      this % stream_id = stream_id
      success = .true.
    end if

  end function stream_file_open

  function stream_file_is_open(this)
    implicit none
    class(stream_file), intent(in) :: this
    logical :: stream_file_is_open
    stream_file_is_open = (this % stream_id >= 0)
  end function stream_file_is_open

  subroutine stream_file_finalize(this)
    implicit none
    type(stream_file), intent(inout) :: this

    if (this % is_open()) then
      close(this % unit)
      this % stream_id = -1
    end if
  end subroutine stream_file_finalize

end module ioserver_file_module
