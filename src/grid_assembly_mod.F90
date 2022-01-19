! Copyright (C) 2022  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
!
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

module grid_assembly_module
  use iso_c_binding
  
  use ioserver_message_module
  use heap_module
  use simple_mutex_module
  implicit none
  private

  integer, parameter :: MAX_ASSEMBLY_LINES = 20 !< How many grids in a stream can be partially assembled at once

  !> Object used to perform the assembly of a single grid within a stream
  !> Located in shared memory
  type, public :: grid_assembly_line
    integer               :: tag = -1               !< Message tag associated with this specific grid to assemble
    type(grid_t)          :: global_grid = grid_t() !< Grid info
    integer(HEAP_ELEMENT) :: data_offset = -1       !< Offset of the data region where the grid is being assembled in the shared memory heap
    integer(kind=8)       :: missing_data = -1      !< Number of elements left to insert into the grid being assembled

  contains
    ! procedure, pass :: free_data => grid_assembly_line_free_data
  end type

  !> Object used to coordinate the assembly of multiple grids received through a single stream
  !> Located in shared memory
  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines = grid_assembly_line() !< Array of individual grids being assembled

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

  !> Create an assembly line for a grid in the owning stream
  function grid_assembly_create_line(this, record, data_heap, mutex) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this      !< The grid assembly object where we want to assemble a grid
    type(model_record),   intent(in)    :: record    !< The data record that triggered the creation of an assembly line
    type(heap),           intent(inout) :: data_heap !< Heap from which we will allocate the necessary (shared) memory for assembling the grid
    type(simple_mutex),   intent(inout) :: mutex !< Mutex for the owning stream. We don't want other people creating assembly lines simultaneously
    integer :: line_id !< Id of the created line

    integer :: free_line_id
    type(block_meta_f08) :: data_array_info

    call mutex % lock()

    line_id = this % get_line_id(record, free_line_id) ! First check if someone else has already created an assembly line for this record

    if (line_id == -1) then           ! Good, there's no assembly line yet
      if (free_line_id .ne. -1) then  ! Good, there's a free line in the grid assembly object
        line_id = free_line_id

        ! Get some shared memory
        block
          integer(kind = 4), dimension(:,:,:,:,:), pointer :: data_array_4
          integer(kind = 8), dimension(:,:,:,:,:), pointer :: data_array_8

          print *, 'heap size = ', data_heap % get_size()

          if (record % elem_size == 4) then
            data_array_info = data_heap % allocate(data_array_4, record % global_grid % size)
            if (.not. associated(data_array_4)) then
              print *, 'ERROR: Could not allocate from the heap! (4)'
              error stop 1
            end if
          else if (record % elem_size == 8) then
            data_array_info = data_heap % allocate(data_array_8, record % global_grid % size)
            if (.not. associated(data_array_8)) then
              print *, 'ERROR: Could not allocate from the heap! (8)'
              error stop 1
            end if
          else
            print *, 'ERROR: Cannot deal with elements of size ', record % elem_size
          end if
        end block

        ! Initialize the assembly line object
        associate(line => this % lines(line_id))
          line % data_offset  = data_array_info % get_offset()
          line % global_grid  = record % global_grid
          line % missing_data = product(line % global_grid % size)
          line % tag          = record % tag ! Signal that the assembly line is ready to be used
          print *, 'Created line ', line_id, mutex % get_id(), line % missing_data, record % tag
        end associate
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

  subroutine set_data_4(full_grid, subgrid, start, end)
    implicit none
    integer(kind=4),    dimension(:,:,:,:,:), contiguous, intent(inout) :: full_grid
    integer(kind=4),    dimension(:,:,:,:,:), contiguous, intent(inout) :: subgrid
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK), intent(in) :: start, end
    full_grid(start(1):end(1), start(2):end(2), start(3):end(3), start(4):end(4), start(5):end(5)) = subgrid(:, :, :, :, :)
  end subroutine set_data_4
  
  subroutine set_data_8(full_grid, subgrid, start, end)
    implicit none
    integer(kind=8),    dimension(:,:,:,:,:), contiguous, intent(inout) :: full_grid
    integer(kind=8),    dimension(:,:,:,:,:), contiguous, intent(inout) :: subgrid
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK), intent(in) :: start, end
    full_grid(start(1):end(1), start(2):end(2), start(3):end(3), start(4):end(4), start(5):end(5)) = subgrid(:, :, :, :, :)
  end subroutine set_data_8

  function grid_assembly_put_data(this, record, subgrid_data, data_heap, mutex) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this
    type(model_record),   intent(in)    :: record
    integer(kind = 8),    intent(in), dimension(:), pointer, contiguous :: subgrid_data
    type(heap),           intent(inout) :: data_heap
    type(simple_mutex),   intent(inout) :: mutex

    type(C_PTR) :: full_grid_ptr, subgrid_ptr

    logical :: success

    integer :: line_id
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: index_start, index_end

    success = .false.
    line_id = this % get_line_id(record)
    if (line_id < 0) line_id = this % create_line(record, data_heap, mutex)
    if (line_id < 0) return

    ! Retrieve full grid from heap
    full_grid_ptr = data_heap % get_address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(full_grid_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset, mutex % get_id()
      error stop 1
    end if

    subgrid_ptr = c_loc(subgrid_data)
    index_start = record % subgrid_area % offset
    index_end   = index_start + record % subgrid_area % size - 1

    ! Insert data into full grid
    block
      integer(kind = 4), dimension(:,:,:,:,:), pointer :: full_grid_4, subgrid_4
      integer(kind = 8), dimension(:,:,:,:,:), pointer :: full_grid_8, subgrid_8
      print *, 'GOTTA REDUCE THIS if-else INTO A ONE-LINER'
      if (record % elem_size == 4) then
        call c_f_pointer(full_grid_ptr, full_grid_4, this % lines(line_id) % global_grid % size)
        call c_f_pointer(subgrid_ptr, subgrid_4, record % subgrid_area % size)
        call set_data_4(full_grid_4, subgrid_4, index_start, index_end)
      else if (record % elem_size == 8) then
        call c_f_pointer(full_grid_ptr, full_grid_8, this % lines(line_id) % global_grid % size)
        call c_f_pointer(subgrid_ptr, subgrid_8, record % subgrid_area % size)
        call set_data_8(full_grid_8, subgrid_8, index_start, index_end)
      end if
    end block

    ! Update missing data indicator
    call mutex % lock()
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - product(record % subgrid_area % size)
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

    integer(kind = 4), dimension(:,:,:,:,:), pointer :: data_array_4
    integer(kind = 8), dimension(:,:,:,:,:), pointer :: data_array_8
    type(C_PTR) :: data_ptr
    logical :: success
    integer :: num_bytes

    ! print *, 'Trying to flush line #', line_id
    data_ptr = data_heap % get_address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(data_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset
      error stop 1
    end if

    if (this % lines(line_id) % global_grid % elem_size == 4) then
      call c_f_pointer(data_ptr, data_array_4, this % lines(line_id) % global_grid % size)
      num_bytes = size(data_array_4) * 4
      write(unit=file_unit) data_array_4
    else if (this % lines(line_id) % global_grid % elem_size == 8) then
      call c_f_pointer(data_ptr, data_array_8, this % lines(line_id) % global_grid % size)
      num_bytes = size(data_array_8) * 8
      write(unit=file_unit) data_array_8
    else
      print *, 'ERROR: Impossible element size when flushing grid'
      error stop 1
    end if

    print '(A, I4, I8, A, 5(I5))', 'Flushed line ', line_id, num_bytes, ' bytes, dim ', this % lines(line_id) % global_grid % size

    ! Reset the assembly line
    success = data_heap % free(this % lines(line_id) % data_offset)
    if (.not. success) then
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
