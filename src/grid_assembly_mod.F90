module grid_assembly_module
  use ioserver_message_module
  use heap_module
  implicit none
  private

  integer, parameter :: MAX_ASSEMBLY_LINES = 20

  type, public :: grid_assembly_line
    integer :: tag = -1
    integer, dimension(:, :), pointer :: data => NULL()
    integer(kind=8) :: missing_data = -1

  contains
    procedure, pass :: free_data => grid_assembly_line_free_data
  end type

  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines = grid_assembly_line()

  contains
    private
    procedure, pass, public :: put_data => grid_assembly_put_data
    procedure, pass, public :: flush_completed_grids => grid_assembly_flush_completed_grids
    procedure, pass, public :: get_num_partial_grids

    procedure, pass :: is_line_full => grid_assembly_is_line_full
    procedure, pass :: get_line_id => grid_assembly_get_line_id
    procedure, pass :: get_lowest_completed_line
    procedure, pass :: flush_line => grid_assembly_flush_line
  end type

contains

  subroutine grid_assembly_line_free_data(this)
    implicit none
    class(grid_assembly_line), intent(inout) :: this
    if (associated(this % data)) then
      deallocate(this % data)
      nullify(this % data)
      ! print *, 'Just freed data for tag ', this % tag
    else
      ! print *, 'AAAhhhhh data is not allocated for tag ', this % tag
    end if
  end subroutine grid_assembly_line_free_data

  function grid_assembly_get_line_id(this, record, data_heap) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    type(heap),           intent(inout) :: data_heap
    integer :: line_id

    integer :: i_line, free_line_id

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
        this % lines(line_id) % tag = record % tag
        allocate(this % lines(line_id) % data(record % grid_size_i, record % grid_size_j))
        this % lines(line_id) % missing_data = record % grid_size_i * record % grid_size_j
        ! print '(A, I6, I4, I4)', 'Starting assembly for a new grid! Tag = ', record % tag, line_id, free_line_id
        ! print '(A, I4, A, I4, A, I8)', 'Grid size: ', record % grid_size_i, ' x ', record % grid_size_j, ', missing data: ', this % lines(line_id) % missing_data
      else
        print *, 'ERROR. We have reached the maximum number of grids being assembled! Quitting.'
        return
      end if
    end if
  end function grid_assembly_get_line_id

  function grid_assembly_is_line_full(this, line_id) result(is_line_full)
    implicit none
    class(grid_assembly), intent(in) :: this
    integer,              intent(in) :: line_id
    logical :: is_line_full
    is_line_full = .false.
    if (this % lines(line_id) % tag > 0) then
      is_line_full = (this % lines(line_id) % missing_data == 0)
      ! print '(A, I3, A, I6, A, L)', 'Line ', line_id, ' with tag ', this % lines(line_id) % tag, ' is full? ', is_line_full
    end if
  end function grid_assembly_is_line_full

  function grid_assembly_put_data(this, record, subgrid_data, data_heap) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    integer,              intent(in), dimension(record % ni, record % nj) :: subgrid_data
    type(heap),           intent(inout) :: data_heap

    logical :: success

    integer :: i_line, line_id
    integer :: i0, i1, j0, j1, size_x, size_y

    success = .false.
    line_id = this % get_line_id(record, data_heap)

    if (line_id < 0) return

    ! print '(A, I7, A, I4)', 'Putting ', record % ni * record % nj, ' data in partial grid ', line_id

    i0 = record % i0
    i1 = i0 + record % ni - 1
    j0 = record % j0
    j1 = j0 + (record % nj - 1)
    this % lines(line_id) % data(i0:i1, j0:j1) = subgrid_data(:, :)
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - record % ni * record % nj

    success = .true.
  end function grid_assembly_put_data

  function grid_assembly_flush_completed_grids(this, file_unit) result(num_flushed)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer,              intent(in)    :: file_unit
    integer :: num_flushed

    logical :: finished
    integer :: i, line_id

    num_flushed = 0
    finished = .false.
    do i = 1, MAX_ASSEMBLY_LINES
      line_id = this % get_lowest_completed_line()
      if (line_id < 0) return
      ! print '(A, I3, A)', 'Line ', line_id, ' is completed'
      call this % flush_line(line_id, file_unit)
      num_flushed = num_flushed + 1
    end do
  end function grid_assembly_flush_completed_grids

  function get_lowest_completed_line(this) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer :: line_id

    integer :: i, lowest_tag

    line_id = -1
    lowest_tag = -1

    do i = 1, MAX_ASSEMBLY_LINES
      if (this % is_line_full(i)) then
        if (lowest_tag < 0 .or. lowest_tag > this % lines(i) % tag) then
          lowest_tag = this % lines(i) % tag
          line_id = i
        end if
      end if
    end do
  end function get_lowest_completed_line

  subroutine grid_assembly_flush_line(this, line_id, file_unit)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer,              intent(in)    :: line_id
    integer,              intent(in)    :: file_unit

    print '(A, I4, I8, A)', 'Flushing line ', line_id, size(this % lines(line_id) % data) * 4, ' bytes'
    ! print *, this % lines(line_id) % data

    write(unit=file_unit) this % lines(line_id) % data(:, :)

    call this % lines(line_id) % free_data()
    this % lines(line_id) % tag = -1
    this % lines(line_id) % missing_data = -1
  end subroutine grid_assembly_flush_line

  function get_num_partial_grids(this) result(num_partial_grids)
    implicit none
    class(grid_assembly), intent(in) :: this
    integer :: num_partial_grids

    integer :: i

    num_partial_grids = 0
    do i = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i) % tag > 0) then
        num_partial_grids = num_partial_grids + 1
      end if
    end do
  end function get_num_partial_grids

end module grid_assembly_module
