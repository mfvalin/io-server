
module server_stream_module
  ! use circular_buffer_module
  use grid_assembly_module
  ! use ioserver_constants
  use ioserver_message_module
  use heap_module
  ! use simple_mutex_module
  implicit none
#if ! defined(VERSION)
#define VERSION 10000
#endif

  save
  private

  public :: block_meta, subgrid, grid, cmeta

  type, public :: server_stream
    private
    integer :: stream_id = -1     ! File ID, for internal use
    integer :: unit = -1          ! Fortran file unit, when open
    integer :: owner_id = -1      ! Who owns (will read/write into) this file
    integer :: mutex_value = 0    ! For later, if we need to lock this file with a mutex
    character(len=:), allocatable :: name

    type(heap) :: data_heap
    type(grid_assembly) :: partial_grid_data

    contains
    procedure :: open
    procedure :: close
    procedure :: put_data
    procedure :: flush_data

    procedure :: is_open
    procedure :: is_valid
    procedure :: print => server_stream_print
    procedure :: is_same_name
    procedure :: get_owner_id
    final     :: server_stream_finalize

    procedure, nopass, private :: make_full_filename
  end type server_stream

contains

  function open(this, stream_id, file_name, owner_id) result(success)
    implicit none
    class(server_stream), intent(inout) :: this
    integer,              intent(in)    :: stream_id
    character(len=*),     intent(in)    :: file_name
    integer,              intent(in)    :: owner_id
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
  end function open

  function close(this) result(success)
    implicit none
    class(server_stream), intent(inout) :: this
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
  end function close

  function put_data(this, record, subgrid_data) result(success)
    implicit none
    class(server_stream), intent(inout) :: this
    class(model_record),  intent(in)    :: record
    integer, intent(in), dimension(record % ni, record % nj) :: subgrid_data
    logical :: success

    success = this % partial_grid_data % put_data(record, subgrid_data)
  end function put_data

  function flush_data(this) result(num_lines_flushed)
    implicit none
    class(server_stream), intent(inout) :: this
    integer :: num_lines_flushed

    num_lines_flushed = this % partial_grid_data % flush_completed_grids(this % unit)
  end function flush_data

  function is_valid(this)
    implicit none
    class(server_stream), intent(in) :: this
    logical :: is_valid
    is_valid = (this % stream_id > 0)
  end function is_valid

  function is_open(this)
    implicit none
    class(server_stream), intent(in) :: this
    logical :: is_open
    is_open = .false.
    if (this % is_valid()) is_open = (this % unit .ne. -1)
  end function is_open

  function make_full_filename(filename) result(full_filename)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=:), allocatable :: full_filename
    full_filename = trim(filename) // '.out'
  end function make_full_filename

  function is_same_name(this, filename)
    implicit none
    class(server_stream), intent(in) :: this
    character(len=*),     intent(in) :: filename
    logical :: is_same_name

    is_same_name = (this % name == this % make_full_filename(filename))
  end function is_same_name

  function get_owner_id(this) result(owner_id)
    implicit none
    class(server_stream), intent(in) :: this
    integer :: owner_id
    owner_id = this % owner_id
  end function get_owner_id

  subroutine server_stream_finalize(this)
    implicit none
    type(server_stream), intent(inout) :: this

    logical :: success
    if (this % is_open()) then
      success = this % close()
    else
      success = .true.
    end if
  end subroutine server_stream_finalize

  subroutine server_stream_print(this)
    implicit none
    class(server_stream), intent(in) :: this
    if (allocated(this % name)) then
      print '(A, I4, I4, I4, I4)', this % name, this % stream_id, this % unit, this % owner_id, this % mutex_value
    else
      print '(I4, I4, I4, I4)', this % stream_id, this % unit, this % owner_id, this % mutex_value
    end if
  end subroutine server_stream_print

end module server_stream_module
