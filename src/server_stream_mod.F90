
module server_stream_module
  ! use circular_buffer_module
  use grid_assembly_module
  ! use ioserver_constants
  use ioserver_message_module
  use heap_module
  use rpn_extra_module, only: sleep_us
  use simple_mutex_module
  implicit none
#if ! defined(VERSION)
#define VERSION 10000
#endif

  private

  public :: block_meta, subgrid, grid, cmeta

  integer, parameter :: STREAM_STATUS_UNINITIALIZED = -1
  integer, parameter :: STREAM_STATUS_INITIALIZED   =  0
  integer, parameter :: STREAM_STATUS_OPEN          =  1
  integer, parameter :: STREAM_STATUS_CLOSED        =  2

  type, public :: shared_server_stream
    private
    integer :: stream_id    = -1     !< File ID, for internal use
    integer :: unit         = -1     !< Fortran file unit, when open
    integer :: owner_id     = -1     !< Who owns (will read/write into) this file
    integer :: mutex_value  = 0      !< When we need to lock this file with a mutex. Don't ever touch this value directly (i.e. other than through a mutex object)
    integer :: status       = STREAM_STATUS_UNINITIALIZED
    character(len=:), allocatable :: name

    type(grid_assembly) :: partial_grid_data = grid_assembly(grid_assembly_line())

    contains
    private
    procedure, pass :: open       => shared_server_stream_open
    procedure, pass :: close      => shared_server_stream_close
    procedure, pass :: flush_data => shared_server_stream_flush_data

    procedure, pass :: is_open      => shared_server_stream_is_open
    procedure, pass :: is_closed    => shared_server_stream_is_closed
    procedure, pass :: is_valid     => shared_server_stream_is_valid
    procedure, pass :: print        => shared_server_stream_print
    procedure, pass :: is_same_name => shared_server_stream_is_same_name
    procedure, pass :: get_owner_id => shared_server_stream_get_owner_id

    final :: shared_server_stream_finalize

    procedure, nopass, private :: make_full_filename
  end type shared_server_stream

  interface shared_server_stream
    procedure new_shared_server_stream
  end interface shared_server_stream

  type, public :: local_server_stream
    private
    integer             :: server_id = -2 !< On which process this local instance is located (default different from the default owner ID of the shared stream)
    logical             :: got_open_request = .false.
    type(simple_mutex)  :: mutex
    type(heap)          :: data_heap
    type(shared_server_stream), pointer :: shared_instance => NULL()

    contains

    procedure, pass :: init     => local_server_stream_init
    procedure, pass :: is_init  => local_server_stream_is_init
    procedure, pass :: is_owner => local_server_stream_is_owner

    procedure, pass :: open       => local_server_stream_open
    procedure, pass :: close      => local_server_stream_close
    procedure, pass :: is_open    => local_server_stream_is_open
    procedure, pass :: put_data   => local_server_stream_put_data
    procedure, pass :: flush_data => local_server_stream_flush_data
  end type local_server_stream

contains

  function new_shared_server_stream(stream_id, owner_id)
    implicit none
    integer, intent(in) :: stream_id
    integer, intent(in) :: owner_id
    type(shared_server_stream) :: new_shared_server_stream
    new_shared_server_stream % stream_id = stream_id
    new_shared_server_stream % owner_id  = owner_id
    new_shared_server_stream % status    = STREAM_STATUS_INITIALIZED
  end function new_shared_server_stream

  function shared_server_stream_open(this, file_name) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    character(len=*),            intent(in)    :: file_name
    logical :: success

    success = .false.
    if (.not. this % is_open()) then
      this % name = this % make_full_filename(file_name)

      open(newunit = this % unit, file = this % name, status = 'replace', form = 'unformatted')
      print '(A, A, A, I4, A, I6, A, I4)', 'Opened file, name = ', this % name, ', stream = ', this % stream_id, ', unit = ', this % unit, ', owner ID = ', this % owner_id
      this % status = STREAM_STATUS_OPEN
      success = .true.
    else if (this % name .ne. this % make_full_filename(file_name)) then
      print *, 'ERROR: Trying to open twice the same file, but with a different name'
    else
      ! File was already open, with the same name
      success = .true.
    end if
  end function shared_server_stream_open

  function shared_server_stream_close(this, data_heap) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    type(heap),                  intent(inout) :: data_heap
    logical :: success

    integer :: num_flushed, num_incomplete

    integer, parameter :: MAX_NUM_ATTEMPTS = 100
    integer, parameter :: WAIT_TIME_US     = 100000
    integer :: i

    success = .false.

    if (this % is_open()) then
      do i = 1, MAX_NUM_ATTEMPTS
        num_flushed = this % partial_grid_data % flush_completed_grids(this % unit, data_heap)
        if (num_flushed > 0) then
          print *, 'Flushed ', num_flushed, ' completed grids upon closing ', this % name
        end if
        num_incomplete = this % partial_grid_data % get_num_partial_grids()
        if (num_incomplete > 0) then
          print '(A, I4, A, F6.2, A)', ' DEBUG: There are still ', num_incomplete, ' incomplete grids in file, will wait another ', (MAX_NUM_ATTEMPTS - i) * WAIT_TIME_US / 1000000.0, ' second(s)'
        else
          exit
        end if
        call sleep_us(WAIT_TIME_US)
      end do

      this % status = STREAM_STATUS_CLOSED
      close(this % unit)
      this % stream_id = -1
      this % owner_id = -1
      success = .true.
    end if
  end function shared_server_stream_close

  function shared_server_stream_flush_data(this, data_heap) result(num_lines_flushed)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    type(heap),                  intent(inout) :: data_heap
    integer :: num_lines_flushed

    num_lines_flushed = this % partial_grid_data % flush_completed_grids(this % unit, data_heap)
  end function shared_server_stream_flush_data

  function shared_server_stream_is_valid(this) result(is_valid)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_valid
    is_valid = (this % status >= 0)
  end function shared_server_stream_is_valid

  function shared_server_stream_is_open(this) result(is_open)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_open
    is_open = (this % status == STREAM_STATUS_OPEN)
  end function shared_server_stream_is_open

  function shared_server_stream_is_closed(this) result(is_closed)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_closed
    is_closed = (this % status == STREAM_STATUS_CLOSED)
  end function shared_server_stream_is_closed

  function make_full_filename(filename) result(full_filename)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=:), allocatable :: trimmed_filename
    character(len=:), allocatable :: full_filename
    integer :: last_char

    trimmed_filename = trim(filename)
    last_char = len(trimmed_filename)
    if (trimmed_filename(last_char:last_char) == achar(0)) last_char = last_char - 1
    full_filename =  trimmed_filename(1:last_char)// '.out'
  end function make_full_filename

  function shared_server_stream_is_same_name(this, filename) result(is_same_name)
    implicit none
    class(shared_server_stream), intent(in) :: this
    character(len=*),     intent(in) :: filename
    logical :: is_same_name

    is_same_name = (this % name == this % make_full_filename(filename))
  end function shared_server_stream_is_same_name

  function shared_server_stream_get_owner_id(this) result(owner_id)
    implicit none
    class(shared_server_stream), intent(in) :: this
    integer :: owner_id
    owner_id = this % owner_id
  end function shared_server_stream_get_owner_id

  subroutine shared_server_stream_finalize(this)
    implicit none
    type(shared_server_stream), intent(inout) :: this

    logical :: success
    if (this % is_open()) then
      success = .false.
      print *, 'ERROR: Shared server stream is still open on finalize()'
    else
      success = .true.
    end if
  end subroutine shared_server_stream_finalize

  subroutine shared_server_stream_print(this)
    implicit none
    class(shared_server_stream), intent(in) :: this
    if (allocated(this % name)) then
      print '(A, I4, I4, I4, I4)', this % name, this % stream_id, this % unit, this % owner_id, this % mutex_value
    else
      print '(I4, I4, I4, I4)', this % stream_id, this % unit, this % owner_id, this % mutex_value
    end if
  end subroutine shared_server_stream_print

  subroutine local_server_stream_init(this, server_id, shared_instance, data_heap)
    implicit none
    class(local_server_stream),          intent(inout) :: this
    integer,                             intent(in)    :: server_id
    type(shared_server_stream), pointer, intent(inout) :: shared_instance
    type(heap),                          intent(inout) :: data_heap

    this % server_id        =  server_id
    this % got_open_request =  .false.
    this % shared_instance  => shared_instance
    this % data_heap        =  data_heap
    call this % mutex % init_from_int(this % shared_instance % mutex_value, this % server_id)
  end subroutine local_server_stream_init

  function local_server_stream_is_init(this) result (is_init)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_init
    is_init = .false.
    if (this % server_id >= 0) then
      is_init = associated(this % shared_instance)
    end if
  end function local_server_stream_is_init
  
  function local_server_stream_is_owner(this) result(is_owner)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_owner
    is_owner = (this % server_id == this % shared_instance % get_owner_id())
  end function local_server_stream_is_owner

  function local_server_stream_open(this, file_name) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    character(len=*),           intent(in)    :: file_name
    logical :: success

    this % got_open_request = .true.
    if (this % is_owner()) then
      success = this % shared_instance % open(file_name)
    else
      success = .true.
    end if
  end function local_server_stream_open

  function local_server_stream_close(this) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: success

    if (this % is_open() .and. this % is_owner()) then
      success = this % shared_instance % close(this % data_heap)
    else
      print *, 'DEBUG: Not the owner, so wont even try to close (might be closed already anyway)'
      success = .true.
    end if
  end function local_server_stream_close

  function local_server_stream_is_open(this) result(is_open)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_open
    is_open = .false.
    if (this % is_init()) then
      is_open = this % shared_instance % is_open()
    end if
  end function local_server_stream_is_open

  function local_server_stream_put_data(this, record, subgrid_data) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    type(model_record),         intent(in)    :: record
    integer, intent(in), dimension(record % ni, record % nj) :: subgrid_data
    logical :: success

    integer, parameter :: MAX_NUM_ATTEMPTS = 100
    integer, parameter :: WAIT_TIME_US     = 100000
    integer :: i

    success = .false.
    if (this % is_open()) then
      success = .true.
    else if ((.not. this % is_owner()) .and. (this % got_open_request)) then
      print *, "DEBUG: The file is not open yet, let's wait a bit ", MAX_NUM_ATTEMPTS * WAIT_TIME_US / 1000000.0
      do i = 1, MAX_NUM_ATTEMPTS
        if (this % is_open()) then
          success = .true.
          print *, 'DEBUG: Done waiting!'
          exit
        else if (this % shared_instance % is_closed()) then
          print *, 'ERROR: Stream has been closed, we will no longer be able to put anything in it'
          exit
        end if
        print *, 'DEBUG: Still waiting...'
        call sleep_us(WAIT_TIME_US)
      end do
    end if

    if (.not. success) then
      print *, 'ERROR: Trying to put data into a stream that is not open'
      return
    end if

    success = this % shared_instance % partial_grid_data % put_data(record, subgrid_data, this % data_heap, this % mutex)
  end function local_server_stream_put_data

  function local_server_stream_flush_data(this) result(num_flushed)
    implicit none
    class(local_server_stream), intent(inout) :: this
    integer :: num_flushed
    
    num_flushed = 0
    if (this % is_owner()) num_flushed = this % shared_instance % flush_data(this % data_heap)
  end function local_server_stream_flush_data
end module server_stream_module
