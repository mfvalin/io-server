
module ioserver_file_module
  use circular_buffer_module
  use grid_assembly_module
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
    integer :: stream_id = -1     ! File ID, for internal use
    integer :: unit = -1          ! Fortran file unit, when open
    integer :: owner_id = -1      ! Who owns (will read/write into) this file
    integer :: mutex_value = 0    ! For later, if we need to lock this file with a mutex
    character(len=:), allocatable :: name

    type(heap) :: data_heap
    type(grid_assembly) :: partial_grid_data

    contains
    procedure :: open       => stream_file_open
    procedure :: close      => stream_file_close
    procedure :: put_data   => stream_file_put_data
    procedure :: flush_data => stream_file_flush_data

    procedure :: is_open  => stream_file_is_open
    procedure :: is_valid => stream_file_is_valid
    procedure :: print    => stream_file_print
    procedure :: is_same_name => stream_file_is_same_name
    procedure :: get_owner_id => stream_file_get_owner_id
    final     :: stream_file_finalize

    procedure, private :: make_full_filename => stream_file_make_full_filename
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

  function server_file_open(this, name) result(success)
    implicit none
    class(server_file),    intent(INOUT) :: this
    character(len=*),      intent(IN)    :: name
    logical :: success

    integer :: lname
    type(message_header) :: header

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

    header % length     = lname
    header % command    = MSG_COMMAND_OPEN_FILE
    header % stream_id  = this % stream_id
    header % tag        = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(this % name, int(lname, kind=8), CB_KIND_CHAR, .false.) .and. success
    success = this % server_bound_cb % put(lname, 1_8, CB_KIND_INTEGER_4, .true.) .and. success       ! Append length and commit message

  end function server_file_open

  function server_file_close(this) result(success)
    implicit none
    class(server_file), intent(INOUT) :: this
    logical :: success

    type(message_header) :: header

    success = .false.
    if (.not. this % is_open()) return

    call this % messenger % bump_tag()

    header % length = message_header_size_int()
    header % stream_id = this % stream_id
    header % tag = this  % messenger % get_msg_tag()
    header % command = MSG_COMMAND_CLOSE_FILE
    header % sender_global_rank = this % global_rank

    this % stream_id = -1
    deallocate(this % name)
    nullify(this % messenger)
    
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(header % length, 1_8, CB_KIND_INTEGER_4, .true.)  .and. success ! Append length + commit

    call message_header_print(header)

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

    header % length     = rec % record_length
    header % command    = MSG_COMMAND_DATA
    header % stream_id  = this % stream_id
    header % tag        = this % messenger % get_msg_tag()
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

  function stream_file_open(this, stream_id, file_name, owner_id) result(success)
    implicit none
    class(stream_file), intent(inout) :: this
    integer,            intent(in)    :: stream_id
    character(len=*),   intent(in)    :: file_name
    integer,            intent(in)    :: owner_id
    logical :: success

    success = .false.
    if (.not. this % is_open()) then
      this % name = this % make_full_filename(file_name)

      open(newunit = this % unit, file = this % name, status = 'replace', form = 'unformatted')
      this % stream_id = stream_id
      this % owner_id = owner_id
      print '(A, A, A, I4, A, I6, A, I4)', 'Opened file, name = ', this % name, ', stream = ', this % stream_id, ', unit = ', this % unit, ', owner ID = ', this % owner_id
      success = .true.
    end if
  end function stream_file_open

  function stream_file_close(this) result(success)
    implicit none
    class(stream_file), intent(inout) :: this
    logical :: success

    integer :: num_flushed, num_incomplete

    success = .false.

    if (this % is_open()) then
      num_flushed = this % partial_grid_data % flush_completed_grids(this % unit)
      if (num_flushed > 0) then
        print *, 'Flushed ', num_flushed, ' completed grids upon closing ', this % name
      end if
      num_incomplete = this % partial_grid_data % get_num_partial_grids()
      if (num_incomplete > 0) then
        print *, 'ERROR: AAAhhhh  there are still ', num_incomplete, ' incomplete grids in file'
      end if
      close(this % unit)
      this % stream_id = -1
      this % owner_id = -1
      success = .true.
    end if
  end function stream_file_close

  function stream_file_put_data(this, record, subgrid_data) result(success)
    implicit none
    class(stream_file),   intent(inout) :: this
    class(model_record),  intent(in)    :: record
    integer, intent(in), dimension(record % ni, record % nj) :: subgrid_data
    logical :: success

    success = this % partial_grid_data % put_data(record, subgrid_data)
  end function stream_file_put_data

  function stream_file_flush_data(this) result(num_lines_flushed)
    implicit none
    class(stream_file), intent(inout) :: this
    integer :: num_lines_flushed

    num_lines_flushed = this % partial_grid_data % flush_completed_grids(this % unit)
  end function stream_file_flush_data

  function stream_file_is_valid(this)
    implicit none
    class(stream_file), intent(in) :: this
    logical :: stream_file_is_valid
    stream_file_is_valid = (this % stream_id > 0)
  end function stream_file_is_valid

  function stream_file_is_open(this)
    implicit none
    class(stream_file), intent(in) :: this
    logical :: stream_file_is_open
    stream_file_is_open = .false.
    if (this % is_valid()) stream_file_is_open = (this % unit .ne. -1)
  end function stream_file_is_open

  function stream_file_make_full_filename(this, filename) result(full_filename)
    implicit none
    class(stream_file), intent(in) :: this
    character(len=*),   intent(in) :: filename
    character(len=:), allocatable :: full_filename
    full_filename = trim(filename) // '.out'
  end function stream_file_make_full_filename

  function stream_file_is_same_name(this, filename) result(is_same_name)
    implicit none
    class(stream_file), intent(in) :: this
    character(len=*),   intent(in) :: filename
    logical :: is_same_name

    is_same_name = (this % name == this % make_full_filename(filename))
  end function stream_file_is_same_name

  function stream_file_get_owner_id(this) result(owner_id)
    implicit none
    class(stream_file), intent(in) :: this
    integer :: owner_id
    owner_id = this % owner_id
  end function stream_file_get_owner_id

  subroutine stream_file_finalize(this)
    implicit none
    type(stream_file), intent(inout) :: this

    logical :: success
    if (this % is_open()) then
      success = this % close()
    else
      success = .true.
    end if
  end subroutine stream_file_finalize

  subroutine stream_file_print(this)
    implicit none
    class(stream_file), intent(in) :: this
    if (allocated(this % name)) then
      print '(A, I4, I4, I4, I4)', this % name, this % stream_id, this % unit, this % owner_id, this % mutex_value
    else
      print '(I4, I4, I4, I4)', this % stream_id, this % unit, this % owner_id, this % mutex_value
    end if
  end subroutine stream_file_print

end module ioserver_file_module
