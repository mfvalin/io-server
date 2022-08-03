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
  
  use grid_meta_module
  use ioserver_message_module
  use shmem_heap_module
  use rpn_extra_module
  use simple_mutex_module
  implicit none
  private

  integer, parameter :: MAX_ASSEMBLY_LINES = 20 !< How many grids in a stream can be partially assembled at once

  !> Object used to perform the assembly of a single grid within a stream
  !> Located in shared memory
  type, public :: grid_assembly_line
    integer               :: tag = -1               !< Message tag associated with this specific grid to assemble
    type(grid_bounds_t)   :: reduced_global_grid    !< Grid dimensions info
    integer               :: element_size = 0       !< Grid element size in number of bytes
    integer(HEAP_ELEMENT) :: data_offset = -1       !< Offset of the data region where the grid is being assembled in the shared memory heap
    integer(kind=8)       :: missing_data = -1      !< Number of elements left to insert into the grid being assembled

  contains
    ! procedure, pass :: free_data => grid_assembly_line_free_data
  end type

  !> Object used to coordinate the assembly of multiple grids received through a single stream
  !> Located in shared memory
  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines !< Array of individual grids being assembled

  contains
    private
    procedure, pass, public :: put_data => grid_assembly_put_data
    procedure, pass, public :: get_num_grids
    procedure, pass, public :: get_completed_line_data
    procedure, pass, public :: free_line

    procedure, pass :: get_line_id_from_tag

    procedure, pass :: print_overview

    procedure, pass :: is_line_full => grid_assembly_is_line_full
    procedure, pass :: get_line_id  => grid_assembly_get_line_id
    procedure, pass :: create_line  => grid_assembly_create_line
  end type
  
  interface grid_assembly_line
    procedure :: new_grid_assembly_line
  end interface

contains

  function new_grid_assembly_line() result(line)
    implicit none
    type(grid_assembly_line) :: line
    ! Just use the default values
    if (line % tag >= 0) print *, 'Woah! Tag should be negative at this point'
  end function new_grid_assembly_line

  subroutine print_overview(this)
    implicit none
    class(grid_assembly), intent(in) :: this

    integer :: i_line, num_lines

    integer, dimension(MAX_ASSEMBLY_LINES) :: tags
    integer(kind=8), dimension(MAX_ASSEMBLY_LINES) :: missing_data

    missing_data(:) = -1
    num_lines = 0
    do i_line = 1, MAX_ASSEMBLY_LINES
      tags(i_line) = this % lines(i_line) % tag
      if (tags(i_line) > 0) then
        num_lines = num_lines + 1
        missing_data(i_line) = this % lines(i_line) % missing_data
      end if
    end do

    print '(A, I3, A)', 'There are ', num_lines, ' grids currently being assembled, with tags '
    print '(10I10)', tags
    print '(A)', ' and missing data '
    print '(10I10)', missing_data
  end subroutine

  !> Retrieve the line id where the grid described by the given record is being assembled.
  !> If the grid's assembly has not started yet, this function can optionnally indicate the
  !> ID of a line that is not being currently used.
  function grid_assembly_get_line_id(this, record, free_line_id_out) result(line_id)
    implicit none
    class(grid_assembly), intent(inout) :: this     !< Grid assembler we are querying
    type(data_record),    intent(in)    :: record   !< Record that determines the grid we are assembling
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
    type(data_record),    intent(in)    :: record    !< The data record that triggered the creation of an assembly line
    type(shmem_heap),     intent(inout) :: data_heap !< Heap from which we will allocate the necessary (shared) memory for assembling the grid
    type(simple_mutex),   intent(inout) :: mutex !< Mutex for the owning stream. We don't want other people creating assembly lines simultaneously
    integer :: line_id !< Id of the created line

    integer :: free_line_id
    type(block_meta_f08) :: data_array_info
    integer(C_INT8_T), dimension(:,:,:,:,:), pointer :: data_array_byte
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: grid_size

    ! print '(I2,A,I6,A,I3)', mutex % get_id(), ' Trying to create an assembly line for tag ', record % tag, ' for file ', record % stream

    call mutex % lock()

    line_id = this % get_line_id(record, free_line_id) ! First check if someone else has already created an assembly line for this record

    if (line_id == -1) then           ! Good, there's no assembly line yet
      if (free_line_id .ne. -1) then  ! Good, there's a free line in the grid assembly object
        line_id = free_line_id

        ! print '(I2, A, I3)', mutex % get_id(), ' Creating line ', line_id

        ! Get some shared memory
        grid_size       = record % reduced_global_grid % get_size_val()
        grid_size(1)    = grid_size(1) * record % elem_size ! Use bytes in the first dimension (so multiply it by size of element)
        data_array_info = data_heap % allocate(data_array_byte, grid_size)
        if (.not. associated(data_array_byte)) then
          print *, 'ERROR: Could not allocate from the heap! (bytes)'
          error stop 1
        end if

        ! Initialize the assembly line object
        associate(line => this % lines(line_id))
          line % data_offset          = data_array_info % get_offset()
          line % reduced_global_grid  = record % reduced_global_grid
          line % element_size         = record % elem_size
          line % missing_data         = line % reduced_global_grid % compute_num_elements()
          line % tag                  = record % tag ! Signal that the assembly line is ready to be used
          ! print '(A, I3, I2, I10, I6)', 'Created line ', line_id, mutex % get_id(), line % missing_data, record % tag
        end associate
      else
        ! print '(I2, A)', mutex % get_id(), ' ERROR (warning?). We have reached the maximum number of grids being assembled! Will not be able to insert data.'
      end if
    else
      print '(I2, A, I3, A)', mutex % get_id(), ' Looks like line ', line_id, ' was created by someone else '
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
  subroutine set_full_grid_data(full_grid, subgrid, start, end)
    implicit none
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, intent(inout) :: full_grid !< [in,out] Destination of the copy
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, intent(inout) :: subgrid   !< [in,out] Source of the copy
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK), intent(in) :: start, end !< [in] Starting and ending index in each dimension of the arrays

    full_grid(start(1):end(1), start(2):end(2), start(3):end(3), start(4):end(4), start(5):end(5)) = subgrid(:,:,:,:,:)
  end subroutine set_full_grid_data

  !> Insert data from a subgrid in its appropriate "full grid" being assembled by this object.
  function grid_assembly_put_data(this, record, subgrid_data, data_heap, mutex) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this       !< [in,out] The grid assembler object where we want to put the data
    type(data_record),    intent(in)    :: record     !< [in]     Metadata about the subgrid we want to insert
    integer(C_INT64_T),   intent(in), dimension(:), pointer, contiguous :: subgrid_data !< [in] The data we want to insert, as a 1-D array of 64-bit integers
    type(shmem_heap),     intent(inout) :: data_heap  !< [in,out] The heap that contains the memory where we are assembling the grid
    type(simple_mutex),   intent(inout) :: mutex      !< [in,out] To avoid synchronization issues (allocating memory, updating completion percentage)

    logical :: success !< Whether we were able to insert the subgrid into the full grid without issues

    integer :: line_id ! Assembly line where the full grid is being assembled
    type(C_PTR) :: full_grid_ptr, subgrid_ptr                                                    ! C pointers to the full grid and subgrid data
    integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, pointer :: full_grid_byte, subgrid_byte ! Fortran pointers to the full/subgrid data
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: index_start, index_end, full_size_byte, sub_size_byte  ! Sets of array indices

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
    full_size_byte    = this % lines(line_id) % reduced_global_grid % get_size_val()
    full_size_byte(1) = full_size_byte(1) * this % lines(line_id) % element_size
    sub_size_byte     = record % local_grid_global_id % get_size_val()
    sub_size_byte(1)  = sub_size_byte(1) * record % elem_size
    index_start       = record % local_grid_global_id % get_min_val()
    index_start(1)    = (index_start(1) - 1) * record % elem_size + 1
    index_end         = index_start + sub_size_byte - 1
    
    ! Retrieve data as 5-D Fortran pointers
    subgrid_ptr = c_loc(subgrid_data)
    call c_f_pointer(full_grid_ptr, full_grid_byte, full_size_byte)
    call c_f_pointer(subgrid_ptr, subgrid_byte, sub_size_byte)

    ! Insert data into full grid
    ! Using a function allows to avoid aliasing restrictions (i.e. having to copy on the stack first)
    call set_full_grid_data(full_grid_byte, subgrid_byte, index_start, index_end)

    ! Update missing data indicator
    call mutex % lock()
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - record % local_grid_global_id % compute_num_elements()
    call mutex % unlock()

    success = .true.
  end function grid_assembly_put_data

  !> Find how many grid are currently being assembled (some may be complete)
  function get_num_grids(this) result(num_partial_grids)
    implicit none
    class(grid_assembly), intent(in) :: this
    integer :: num_partial_grids !< How many grids are being assembled (some might be complete)

    integer :: i

    num_partial_grids = 0
    do i = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i) % tag > 0) then
        num_partial_grids = num_partial_grids + 1
      end if
    end do
  end function get_num_grids

  !> Find the line ID that has the given tag. Will wait for a certain (optionally specified) timeout
  !> before deciding the tag just doesn't exist
  function get_line_id_from_tag(this, tag, timeout_ms) result(line_id)
    implicit none
    class(grid_assembly), intent(in) :: this        !< Grid assembly instance
    integer,              intent(in) :: tag         !< Tag we are looking for
    integer, optional,    intent(in) :: timeout_ms  !< [optional] How long (in ms) we want to wait before giving up
    integer :: line_id !< ID of the assembly line with the given tag in the lines array. -1 if not found

    integer, parameter :: DEFAULT_TOTAL_WAIT_TIME_MS = 1000
    integer, parameter :: SINGLE_WAIT_TIME_MS = 1
    integer :: wait_time_ms

    integer :: i_wait, i_line

    wait_time_ms = DEFAULT_TOTAL_WAIT_TIME_MS
    if (present(timeout_ms)) wait_time_ms = timeout_ms

    line_id = -1

    do i_wait = 1, wait_time_ms / SINGLE_WAIT_TIME_MS
      do i_line = 1, MAX_ASSEMBLY_LINES
        if (this % lines(i_line) % tag == tag) then
          line_id = i_line
          return
        end if
      end do
      call sleep_us(SINGLE_WAIT_TIME_MS * 1000)
    end do
  end function get_line_id_from_tag

  function get_completed_line_data(this, tag, data_heap) result(data_ptr)
    implicit none
    class(grid_assembly), intent(inout) :: this
    integer,              intent(in)    :: tag
    type(shmem_heap),     intent(inout) :: data_heap  !< [in,out] Heap where the assembled grid is stored
    type(C_PTR) :: data_ptr

    integer, parameter :: TOTAL_ASSEMBLY_WAIT_TIME_MS = 5000
    integer, parameter :: SINGLE_ASSEMBLY_WAIT_TIME_MS = 1

    integer :: i_wait_assembly

    integer :: line_id

    data_ptr = C_NULL_PTR

    ! First get the corresponding assembly line. It might not have been created yet, so we might have to wait a bit.
    do i_wait_assembly = 1, TOTAL_ASSEMBLY_WAIT_TIME_MS / SINGLE_ASSEMBLY_WAIT_TIME_MS
      line_id = this % get_line_id_from_tag(tag)
      if (line_id >= 1) then
        exit
      end if
      call sleep_us(SINGLE_ASSEMBLY_WAIT_TIME_MS * 1000)
    end do

    ! Even after waiting, still no line...
    if (line_id < 1) then
      print '(A, I8)', 'ERROR: There is no assembly line corresponding to tag ', tag
      return
    end if

    ! Now that we have found the correct assembly line, we wait until it is complete (timer is reset)
    do i_wait_assembly = 1, TOTAL_ASSEMBLY_WAIT_TIME_MS / SINGLE_ASSEMBLY_WAIT_TIME_MS
      if (this % is_line_full(line_id)) then
        data_ptr = data_heap % get_address_from_offset(this % lines(line_id) % data_offset)
        ! print *, 'address = ', transfer(data_ptr, 1_8)
        ! if (.not. data_heap % is_valid_block(data_ptr)) print *, 'ERROR: INVALID BLOCK!!!'
        ! if (.not. data_heap % check()) print *, 'ERROR: HEAP NOT OK'
        return
      end if
      call sleep_us(SINGLE_ASSEMBLY_WAIT_TIME_MS * 1000)
    end do

    print '(A, F5.1, A, I6, A)', 'ERROR: Timeout after ', TOTAL_ASSEMBLY_WAIT_TIME_MS / 1000.0, 's while waiting for a grid with tag ', tag, ' to be assembled'
    ! call this % print_overview()
    ! print '(A, 5I6)', '       Grid ', this % lines(line_id) % global_grid % size
  end function get_completed_line_data

  !> Free the space used by the given grid assembly line on the shared memory heap
  function free_line(this, tag, data_heap, mutex) result(success)
    implicit none
    class(grid_assembly), intent(inout) :: this       !< Grid assembly instance
    integer,              intent(in)    :: tag        !< Tag of the data we want to delete
    type(shmem_heap),     intent(inout) :: data_heap  !< [in,out] Heap where the assembled grid is stored
    type(simple_mutex),   intent(inout) :: mutex      !< [in,out] To avoid synchronization issues (allocating memory, updating completion percentage)
    logical :: success
    
    integer :: line_id

    success = .false.

    line_id = this % get_line_id_from_tag(tag)
    if (line_id < 1) then
      print *, 'ERROR: Did not find requested line!'
      return
    end if

    call mutex % lock()
    success = data_heap % free(this % lines(line_id) % data_offset)
    this % lines(line_id) = grid_assembly_line()
    call mutex % unlock()

    if (.not. success) print *, 'ERROR: Unable to free from heap'

  end function free_line

end module grid_assembly_module
