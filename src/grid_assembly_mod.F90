module grid_assembly_module
  use ioserver_functions
  implicit none

  private

  integer, parameter :: MAX_NUM_STREAMS = 64
  integer, parameter :: MAX_ASSEMBLY_LINES = 20


  type, private :: grid_assembly_line
    integer :: tag = -1
    integer :: file_unit
    integer, dimension(:, :), allocatable :: data
    integer(kind=8) :: missing_data = -1
  end type

  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines

    contains
    procedure :: put_data => grid_assembly_put_data
  end type

  type, public :: stream_file
    private
    integer :: stream_id = -1       ! File ID, for internal use
    character(len=:), allocatable :: name
    integer :: unit = -1

    contains
    procedure :: open    => stream_file_open
    procedure :: is_open => stream_file_is_open
    final     :: stream_file_finalize
  end type stream_file

contains

  function grid_assembly_put_data(this, record, entry_size, subgrid_data) result(status)
    implicit none
    class(grid_assembly), intent(inout) :: this
    class(model_record),  intent(in)    :: record
    ! integer, intent(in) :: tag
    ! integer, intent(in) :: start_i, start_j
    ! integer, intent(in) :: num_i, num_j
    ! integer, intent(in) :: total_size_i, total_size_j
    integer, intent(in) :: entry_size
    integer, intent(in), dimension(record % ni, record % nj * entry_size) :: subgrid_data

    integer :: status

    integer :: i_line, line_id, free_line_id
    integer :: i0, i1, j0, j1

    status       = -1
    line_id      = -1
    free_line_id = -1
    do i_line = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i_line) % tag == record % tag) then
        line_id = i_line
        exit
      else if (this % lines(i_line) % tag == -1 .and. free_line_id == -1) then
        free_line_id = i_line
      end if
    end do
    
    if (line_id == -1) then
      if (free_line_id .ne. -1) then
        line_id = free_line_id
        print *, 'Starting assembly for a new grid! Tag = ', record % tag, line_id, free_line_id
        this % lines(line_id) % tag = record % tag
        this % lines(line_id) % file_unit = record % stream
        allocate(this % lines(line_id) % data(record % grid_size_i, record % grid_size_j * entry_size))
        this % lines(line_id) % missing_data = record % grid_size_i * record % grid_size_j * entry_size
      else
        print *, 'We have reached the maximum number of grids being assembled! Quitting.'
        error stop 1
        return
      end if
    end if

    i0 = record % i0
    i1 = i0 + record % ni - 1
    j0 = record % j0 * entry_size
    j1 = j0 + (record % nj - 1) * entry_size
    ! print *, 'si, sj, ni, nj', start_i, start_j, num_i, num_j
    ! print *, 'Writing to ', i0, i1, j0, j1, entry_size
    this % lines(line_id) % data(i0:i1, j0:j1) = subgrid_data(:,:)
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - record % ni * record % nj * entry_size

    if (this % lines(line_id) % missing_data == 0) then
      print *, 'Completed a grid! Gotta write it down now'
      status = line_id
    else
      status = 0
    end if
  end function grid_assembly_put_data

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

end module grid_assembly_module
