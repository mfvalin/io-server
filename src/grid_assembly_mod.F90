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

  !> Retrieve the line id where the grid described by the given record is being assembled.
  !> If the grid's assembly has not started yet, this function can optionnally indicate the
  !> ID of a line that is not being currently used.
  function grid_assembly_get_line_id(this, record, free_line_id_out) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this     !< Grid assembler we are querying
    type(model_record),   intent(in)    :: record   !< Record that determines the grid we are assembling
    integer, optional,    intent(out)   :: free_line_id_out !< [out] [Optional] ID of an assembly line that is free
    integer :: line_id !< Assembly line ID that corresponds to the grid indicated by the record. -1 if that grid is not currently being assembled

    integer :: i_line, free_line_id

    line_id      = -1
    free_line_id = -1
    do i_line = 1, MAX_ASSEMBLY_LINES                           ! Loop over all possible lines
      if (this % lines(i_line) % tag == record % tag) then      ! Found it!
        line_id = i_line
        exit
      else if (this % lines(i_line) % tag == -1 .and. free_line_id == -1) then   ! Found a (first) free line
        free_line_id = i_line
      end if
    end do

    if (present(free_line_id_out)) free_line_id_out = free_line_id ! Update optional output
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
    integer(C_INT8_T), dimension(:,:,:,:,:), pointer :: data_array_byte
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: grid_size

    call mutex % lock()

    line_id = this % get_line_id(record, free_line_id) ! First check if someone else has already created an assembly line for this record

    if (line_id == -1) then           ! Good, there's no assembly line yet
      if (free_line_id .ne. -1) then  ! Good, there's a free line in the grid assembly object
        line_id = free_line_id

        ! Get some shared memory
        grid_size       = record % global_grid % size
        grid_size(1)    = grid_size(1) * record % elem_size ! Use bytes in the first dimension (so multiply it by size of element)
        data_array_info = data_heap % allocate(data_array_byte, grid_size)
        if (.not. associated(data_array_byte)) then
          print *, 'ERROR: Could not allocate from the heap! (bytes)'
          error stop 1
        end if

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

  !> Check whether the grid at the given line is fully assembled
  function grid_assembly_is_line_full(this, line_id) result(is_line_full)
    implicit none
    class(grid_assembly), intent(in) :: this
    integer,              intent(in) :: line_id !< [in] Line we are querying
    logical :: is_line_full !< Whether the grid at the given line is fully assembled
    is_line_full = .false.
    if (this % lines(line_id) % tag > 0) then ! There is something at that line
      is_line_full = (this % lines(line_id) % missing_data == 0)
    end if
  end function grid_assembly_is_line_full

  !> Put data from one array into another using Fortran array indexing. It's useful to
  !> do it in its own function because it bypasses precautions for potentially aliased pointers.
  !> This means you should *not* call this function on array pointers if the full_grid might
  !> overlap with the subgrid.
  subroutine set_full_grid_data(full_grid, subgrid, start)
    implicit none
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, intent(inout) :: full_grid !< [in,out] Destination of the copy
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, intent(inout) :: subgrid   !< [in,out] Source of the copy
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK), intent(in) :: start !< [in] Starting index in each dimension of the arrays

    full_grid(start(1):, start(2):, start(3):, start(4):, start(5):) = subgrid(:,:,:,:,:)
  end subroutine set_full_grid_data

  !> Insert data from a subgrid in its appropriate "full grid" being assembled by this object.
  function grid_assembly_put_data(this, record, subgrid_data, data_heap, mutex) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this       !< [in,out] The grid assembler object where we want to put the data
    type(model_record),   intent(in)    :: record     !< [in]     Metadata about the subgrid we want to insert
    integer(C_INT64_T),   intent(in), dimension(:), pointer, contiguous :: subgrid_data !< [in] The data we want to insert, as a 1-D array of 64-bit integers
    type(heap),           intent(inout) :: data_heap  !< [in,out] The heap that contains the memory where we are assembling the grid
    type(simple_mutex),   intent(inout) :: mutex      !< [in,out] To avoid synchronization issues (allocating memory, updating completion percentage)

    logical :: success !< Whether we were able to insert the subgrid into the full grid without issues

    integer :: line_id ! Assembly line where the full grid is being assembled
    type(C_PTR) :: full_grid_ptr, subgrid_ptr                                                    ! C pointers to the full grid and subgrid data
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, pointer :: full_grid_byte, subgrid_byte ! Fortran pointers to the full/subgrid data
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: index_start, full_size_byte, sub_size_byte  ! Sets of array indices

    success = .false.

    line_id = this % get_line_id(record)                                      ! Retrieve line where the grid is being assembled
    if (line_id < 0) line_id = this % create_line(record, data_heap, mutex)   ! Create the line if it didn't already exist
    if (line_id < 0) return                                                   ! We have a problem

    ! Retrieve full grid from heap
    full_grid_ptr = data_heap % get_address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(full_grid_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset, mutex % get_id()
      error stop 1
    end if

    ! Compute addressing sizes/indices. The first dimension is actually in bytes rather than elements, so we need to include
    ! a factor of 'elem_size' into that index
    full_size_byte    = this % lines(line_id) % global_grid % size
    full_size_byte(1) = full_size_byte(1) * this % lines(line_id) % global_grid % elem_size
    sub_size_byte     = record % subgrid_area % size
    sub_size_byte(1)  = sub_size_byte(1) * record % elem_size
    index_start       = record % subgrid_area % offset
    index_start(1)    = (index_start(1) - 1) * record % elem_size + 1
    
    ! Retrieve data as 5-D Fortran pointers
    subgrid_ptr = c_loc(subgrid_data)
    call c_f_pointer(full_grid_ptr, full_grid_byte, full_size_byte)
    call c_f_pointer(subgrid_ptr, subgrid_byte, sub_size_byte)

    ! Insert data into full grid
    ! Using a function allows to avoid aliasing restrictions (i.e. having to copy on the stack first)
    call set_full_grid_data(full_grid_byte, subgrid_byte, index_start)

    ! Update missing data indicator
    call mutex % lock()
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - product(record % subgrid_area % size)
    call mutex % unlock()

    success = .true.
  end function grid_assembly_put_data

  !> Clear the assembly lines that contain completed grids. For now this means write them to the
  !> given file
  function grid_assembly_flush_completed_grids(this, file_unit, data_heap) result(num_flushed)
    implicit none
    class(grid_assembly), intent(inout) :: this       !< [in,out] The assembler we are cleaning up
    integer,              intent(in)    :: file_unit  !< [in]     File (unit) where to write the completed grids
    type(heap),           intent(inout) :: data_heap  !< [in,out] Heap where the grid data is located
    integer :: num_flushed !< How many assembly lines were flushed

    integer :: i, line_id

    num_flushed = 0
    ! The maximum number of completed lines is the total number of lines that the assembler can contain,
    ! so we loop over that amount
    do i = 1, MAX_ASSEMBLY_LINES
      line_id = this % get_lowest_completed_line()          ! Find the completed assembly line with the lowest tag
      if (line_id < 0) return                               ! There isn't any
      call this % flush_line(line_id, file_unit, data_heap) ! Flush it
      num_flushed = num_flushed + 1
    end do
  end function grid_assembly_flush_completed_grids

  !> Find the assembly line with the lowest tag, whose grid is also completely assembled.
  !> If a line is fully assembled, but has a higher tag that an incomplete one, it will *not*
  !> be selected.
  function get_lowest_completed_line(this) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer :: line_id !< ID of the line with the lowest tag, if it is completed. -1 otherwise

    integer :: i, lowest_tag

    line_id = -1
    lowest_tag = -1

    do i = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i) % tag > 0 .and. (lowest_tag < 0 .or. lowest_tag > this % lines(i) % tag)) then
        lowest_tag = this % lines(i) % tag      ! Keep track of the lowest tag
        if (this % is_line_full(i)) line_id = i ! Select this line for now
      end if
    end do
  end function get_lowest_completed_line

  !> Flush the given assembly line into the given file unit.
  !> *BIG WARNING*: Since we write as a set of bytes, we need to read it the same way,
  !> because of little- and big-endian stuff.
  subroutine grid_assembly_flush_line(this, line_id, file_unit, data_heap)
    implicit none
    class(grid_assembly), intent(inout) :: this       !< 
    integer,              intent(in)    :: line_id    !< [in] ID of the line we want to flush
    integer,              intent(in)    :: file_unit  !< [in] File unit where we want to write the content of the assembly line
    type(heap),           intent(inout) :: data_heap  !< [in,out] Heap where the assembled grid is stored

    type(C_PTR) :: data_ptr
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, pointer :: data_array_byte
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: grid_size

    logical :: success
    integer :: num_bytes

    ! Retrieve pointer to grid data
    data_ptr = data_heap % get_address_from_offset(this % lines(line_id) % data_offset)
    if (.not. c_associated(data_ptr)) then
      print *, 'ERROR: Pointer retrieved from heap is not associated!', this % lines(line_id) % data_offset
      error stop 1
    end if

    ! Get a Fortran pointer to that data
    grid_size = this % lines(line_id) % global_grid % size
    grid_size(1) = grid_size(1) * this % lines(line_id) % global_grid % elem_size
    call c_f_pointer(data_ptr, data_array_byte, grid_size)

    ! Write it to the file
    num_bytes = size(data_array_byte)
    write(unit=file_unit) data_array_byte

    ! print '(A, I4, I8, A, 5(I5))', 'Flushed line ', line_id, num_bytes, ' bytes, dim ', this % lines(line_id) % global_grid % size

    ! Reset the assembly line
    success = data_heap % free(this % lines(line_id) % data_offset)
    if (.not. success) then
      print *, 'ERROR: Could not free the assembly line!'
      error stop 1
    end if
    this % lines(line_id) = grid_assembly_line()
  end subroutine grid_assembly_flush_line

  !> Find how many grid are currently being assembled and are still incomplete
  function get_num_partial_grids(this) result(num_partial_grids)
    implicit none
    class(grid_assembly), intent(in) :: this
    integer :: num_partial_grids !< How many grids are being assembled and still incomplete

    integer :: i

    num_partial_grids = 0
    do i = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i) % tag > 0) then
        num_partial_grids = num_partial_grids + 1
      end if
    end do
  end function get_num_partial_grids

end module grid_assembly_module
