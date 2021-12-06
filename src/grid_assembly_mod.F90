module grid_assembly_module
  use iso_c_binding
  
  use ioserver_message_module
  use heap_module
  use simple_mutex_module
  implicit none
  private

  integer, parameter :: MAX_ASSEMBLY_LINES = 20

  type, public :: grid_assembly_line
    integer               :: tag = -1
    integer               :: dim_i = -1
    integer               :: dim_j = -1
    integer(HEAP_ELEMENT) :: data_offset = -1
    integer(kind=8)       :: missing_data = -1

  contains
    ! procedure, pass :: free_data => grid_assembly_line_free_data
  end type

  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines = grid_assembly_line()

  contains
    private
    procedure, pass, public :: put_data              => grid_assembly_put_data
    procedure, pass, public :: flush_completed_grids => grid_assembly_flush_completed_grids
    procedure, pass, public :: get_num_partial_grids

    procedure, pass :: is_line_full => grid_assembly_is_line_full
    procedure, pass :: get_line_id  => grid_assembly_get_line_id
    procedure, pass :: create_line  => grid_assembly_create_line
    procedure, pass :: get_lowest_completed_line
    procedure, pass :: flush_line   => grid_assembly_flush_line
  end type

contains


  function grid_assembly_get_line_id(this, record, free_line_id_out) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    integer, optional,    intent(out)   :: free_line_id_out
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

    if (present(free_line_id_out)) free_line_id_out = free_line_id
  end function grid_assembly_get_line_id

  function grid_assembly_create_line(this, record, data_heap, mutex) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    type(heap),           intent(inout) :: data_heap
    type(simple_mutex),   intent(inout) :: mutex
    integer :: line_id

    integer :: free_line_id
    integer, dimension(:,:), pointer :: data_array
    type(block_meta) :: data_array_info

    call mutex % lock()
    ! print *, 'Creating line for ', mutex % get_id()

    line_id = this % get_line_id(record, free_line_id)

    if (line_id == -1) then
      if (free_line_id .ne. -1) then
        line_id = free_line_id

        data_array_info = data_heap % allocate(data_array, [record % grid_size_i, record % grid_size_j])

        if (.not. associated(data_array)) then
          print *, 'ERROR: Could not allocate from the heap!'
          error stop 1
        end if


        this % lines(line_id) % data_offset = get_block_meta_offset(data_array_info)
        this % lines(line_id) % dim_i = record % grid_size_i
        this % lines(line_id) % dim_j = record % grid_size_j

        ! allocate(this % lines(line_id) % data(record % grid_size_i, record % grid_size_j))
        this % lines(line_id) % missing_data = record % grid_size_i * record % grid_size_j

        print *, 'Created line ', line_id, mutex % get_id(), this % lines(line_id) % missing_data, record % tag
        ! data_array(:,:) = -1

        ! print '(A, I6, I4, I4)', 'Starting assembly for a new grid! Tag = ', record % tag, line_id, free_line_id
        ! print '(A, I4, A, I4, A, I8)', 'Grid size: ', record % grid_size_i, ' x ', record % grid_size_j, ', missing data: ', this % lines(line_id) % missing_data

        this % lines(line_id) % tag = record % tag ! Signal that the assembly line is ready to be used
      else
        print *, 'ERROR. We have reached the maximum number of grids being assembled! Will not be able to insert data.'
      end if
    else
      print *, 'Looks like the line was created by someone else ', mutex % get_id()
    end if

    call mutex % unlock()
  end function grid_assembly_create_line

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

  function grid_assembly_put_data(this, record, subgrid_data, data_heap, mutex) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    integer,              intent(in), dimension(record % ni, record % nj) :: subgrid_data
    type(heap),           intent(inout) :: data_heap
    type(simple_mutex),   intent(inout) :: mutex

    integer, dimension(:,:), pointer :: full_grid
    type(C_PTR) :: full_grid_ptr

    logical :: success

    integer :: i_line, line_id
    integer :: i0, i1, j0, j1, size_x, size_y

    success = .false.
    line_id = this % get_line_id(record)
    if (line_id < 0) line_id = this % create_line(record, data_heap, mutex)
    if (line_id < 0) return

    ! print '(A, I7, A, I4)', 'Putting ', record % ni * record % nj, ' data in partial grid ', line_id

    ! print *, 'Trying to put data in line # (from #)', line_id, mutex % get_id()
    full_grid_ptr = data_heap % address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(full_grid_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset, mutex % get_id()
      error stop 1
    end if
    call c_f_pointer(full_grid_ptr, full_grid, [this % lines(line_id) % dim_i, this % lines(line_id) % dim_j])

    i0 = record % i0
    i1 = i0 + record % ni - 1
    j0 = record % j0
    j1 = j0 + (record % nj - 1)
    full_grid(i0:i1, j0:j1) = subgrid_data(:, :)

    ! TODO Gotta do it better than with a mutex!!!!!!!!!
    call mutex % lock()
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - record % ni * record % nj
    call mutex % unlock()

    success = .true.
  end function grid_assembly_put_data

  function grid_assembly_flush_completed_grids(this, file_unit, data_heap) result(num_flushed)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer,              intent(in)    :: file_unit
    type(heap),           intent(inout) :: data_heap
    integer :: num_flushed

    logical :: finished
    integer :: i, line_id

    num_flushed = 0
    finished = .false.
    do i = 1, MAX_ASSEMBLY_LINES
      line_id = this % get_lowest_completed_line()
      if (line_id < 0) return
      ! print '(A, I3, A, I5, A)', 'Line ', line_id, ' with tag ', this % lines(line_id) % tag, ' is completed'
      call this % flush_line(line_id, file_unit, data_heap)
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
      if (this % lines(i) % tag > 0 .and. (lowest_tag < 0 .or. lowest_tag > this % lines(i) % tag)) then
        lowest_tag = this % lines(i) % tag
        if (this % is_line_full(i)) line_id = i
      end if

    end do
  end function get_lowest_completed_line

  subroutine grid_assembly_flush_line(this, line_id, file_unit, data_heap)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer,              intent(in)    :: line_id
    integer,              intent(in)    :: file_unit
    type(heap),           intent(inout) :: data_heap

    integer, dimension(:,:), pointer :: data_array
    type(C_PTR) :: data_ptr
    integer(C_INT) :: free_result

    ! print *, 'Trying to flush line #', line_id
    data_ptr = data_heap % address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(data_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset
      error stop 1
    end if
    call c_f_pointer(data_ptr, data_array, [this % lines(line_id) % dim_i, this % lines(line_id) % dim_j])

    print '(A, I4, I8, A, I5,A,I5)', 'Flushing line ', line_id, size(data_array) * 4, ' bytes, ', this % lines(line_id) % dim_i, 'x', this % lines(line_id) % dim_j
    ! print *,  data_array

    write(unit=file_unit) data_array(:, :)

    ! Reset the assembly line
    free_result = data_heap % free(this % lines(line_id) % data_offset)
    if (free_result .ne. 0) then
      print *, 'ERROR: Could not free the assembly line!'
      error stop 1
    end if
    this % lines(line_id) = grid_assembly_line()
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
