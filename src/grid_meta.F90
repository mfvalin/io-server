!  Copyright (C) 2022  Recherche en Prevision Numerique
!
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
!
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!> \author V. Magnoux, Recherche en Prevision Numerique
!> \date 2020-2022

!> \file grid_meta.F90
!>
!> Definition of grid description types for easier manipulation of 
!> multidimensional grids

module grid_meta_module
  use iso_c_binding
  implicit none
  private

  public :: print_grid_index

  !> How many grid dimensions we can manage.
  !> Must *not* be higher than #MAX_ARRAY_RANK in shmem_heap
  integer, parameter :: IOSERVER_MAX_NUM_GRID_DIM = 5

  !> \brief Derived type that contains an index into a grid with up to 5
  !> (#IOSERVER_MAX_NUM_GRID_DIM) dimensions.
  !> Can be used to describe a point within a grid, or the upper or lower
  !> bound of a grid
  type, public :: grid_index_t
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: val = (/ 1, 1, 1, 1, 1 /)
    contains
    procedure, pass :: assign_array4
    procedure, pass :: assign_array8
    generic :: assignment(=) => assign_array4, assign_array8
  end type grid_index_t

  !> \brief Derived type that describes a grid with its lower and upper
  !> bounds
  type, public :: grid_bounds_t
    type(grid_index_t) :: min_bound !< Minimum coordinate of the grid, in all its dimensions
    type(grid_index_t) :: max_bound !< Maximum coordinate of the grid, in all its dimensions
    type(grid_index_t) :: size      !< Size of the grid in all its dimensions
  contains
    procedure, pass :: compute_num_elements

    procedure, pass :: get_min  => grid_bounds_get_min
    procedure, pass :: get_max  => grid_bounds_get_max
    procedure, pass :: get_size => grid_bounds_get_size
    procedure, pass :: set      => grid_bounds_set

    procedure, pass :: print    => grid_bounds_print
  end type grid_bounds_t

  interface grid_index_t
    procedure :: new_grid_index_array4
    procedure :: new_grid_index_array8
    procedure :: new_grid_index_i4
    procedure :: new_grid_index_i8
  end interface

contains

  !> Assign an array of 32-bit integers to the set of indices
  !> If the array is too long, only take the first #IOSERVER_MAX_NUM_GRID_DIM
  !> entries
  subroutine assign_array4(this, array)
    implicit none
    class(grid_index_t), intent(inout)            :: this  !< The grid_index_t instance
    integer(C_INT32_T),  intent(in), dimension(:) :: array !< The array we want to assign
    integer :: upper
    ! Get the correct number of entries from input array
    upper = ubound(array, dim=1) - lbound(array, dim=1) + 1
    upper = min(upper, IOSERVER_MAX_NUM_GRID_DIM)
    this % val(:) = 1 ! Make sure the other entries are set to 1
    this % val(1:upper) = array
  end subroutine assign_array4

  !> Assign an array of 64-bit integers to the set of indices
  !> If the array is too long, only take the first #IOSERVER_MAX_NUM_GRID_DIM
  !> entries
  subroutine assign_array8(this, array)
    implicit none
    class(grid_index_t), intent(inout)            :: this  !< The grid_index_t instance
    integer(C_INT64_T),  intent(in), dimension(:) :: array !< The array we want to assign
    integer :: upper
    ! Get the correct number of entries from input array
    upper = ubound(array, dim=1) - lbound(array, dim=1) + 1
    upper = min(upper, IOSERVER_MAX_NUM_GRID_DIM)
    this % val(:) = 1 ! Make sure the other entries are set to 1
    this % val(1:upper) = array
  end subroutine assign_array8

  !> Create a grid_index_t from an array of 32-bit integers
  !> \sa assign_array4
  function new_grid_index_array4(id)
    implicit none
    integer(C_INT32_T), dimension(:), intent(in) :: id
    type(grid_index_t) :: new_grid_index_array4
    new_grid_index_array4 = id ! Just call the assignment operator
  end function

  !> Create a grid_index_t from an array of 64-bit integers
  !> \sa assign_array8
  function new_grid_index_array8(id)
    implicit none
    integer(C_INT64_T), dimension(:), intent(in) :: id
    type(grid_index_t) :: new_grid_index_array8
    new_grid_index_array8 = id ! Just call the assignment operator
  end function

  !> Create a grid_index_t from a set of up to 5 32-bit integers
  function new_grid_index_i4(i1, i2, i3, i4, i5)
    implicit none
    integer(C_INT32_T), intent(in)           :: i1
    integer(C_INT32_T), intent(in), optional :: i2, i3, i4, i5
    type(grid_index_t) :: new_grid_index_i4
    new_grid_index_i4 % val(1) = i1
    if (present(i2)) new_grid_index_i4 % val(2) = i2
    if (present(i3)) new_grid_index_i4 % val(3) = i3
    if (present(i4)) new_grid_index_i4 % val(4) = i4
    if (present(i5)) new_grid_index_i4 % val(5) = i5
  end function

  !> Create a grid_index_t from a set of up to 5 64-bit integers
  function new_grid_index_i8(i1, i2, i3, i4, i5)
    implicit none
    integer(C_INT64_T), intent(in)           :: i1
    integer(C_INT64_T), intent(in), optional :: i2, i3, i4, i5
    type(grid_index_t) :: new_grid_index_i8
    new_grid_index_i8 % val(1) = i1
    if (present(i2)) new_grid_index_i8 % val(2) = i2
    if (present(i3)) new_grid_index_i8 % val(3) = i3
    if (present(i4)) new_grid_index_i8 % val(4) = i4
    if (present(i5)) new_grid_index_i8 % val(5) = i5
  end function

  subroutine print_grid_index(id)
    implicit none
    type(grid_index_t), intent(in) :: id
    print '(A, 5I7)', 'grid index', id % val
  end subroutine

  function grid_bounds_get_min(grid_bounds) result(min_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: min_bound
    min_bound = grid_bounds % min_bound
  end function grid_bounds_get_min

  function grid_bounds_get_max(grid_bounds) result(max_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: max_bound
    max_bound = grid_bounds % max_bound
  end function grid_bounds_get_max

  function grid_bounds_get_size(grid_bounds) result(grid_size)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: grid_size
    grid_size = grid_bounds % size
  end function grid_bounds_get_size

  function grid_bounds_set(this, min_bound, max_bound, size) result(success)
    implicit none
    class(grid_bounds_t), intent(inout) :: this             !< The grid_bounds_t instance we want to specify
    type(grid_index_t), intent(in), optional :: min_bound
    type(grid_index_t), intent(in), optional :: max_bound
    type(grid_index_t), intent(in), optional :: size
    logical :: success

    integer :: num_inputs

    success = .false.

    num_inputs = 0
    if (present(min_bound)) num_inputs = num_inputs + 1
    if (present(max_bound)) num_inputs = num_inputs + 1
    if (present(size))      num_inputs = num_inputs + 1
    if (num_inputs .ne. 2) then
      print *, 'ERROR: Setting grid params in grid_bounds_t. Need to give exactly 2 params (out of min_bound, max_bound and size)'
      return
    end if

    if (present(min_bound)) this % min_bound = min_bound
    if (present(max_bound)) this % max_bound = max_bound
    if (present(size))      this % size      = size

    if (.not. present(min_bound)) this % min_bound = this % max_bound % val(:) - this % size % val(:) + 1
    if (.not. present(max_bound)) this % max_bound = this % min_bound % val(:) + this % size % val(:) - 1
    if (.not. present(size))      this % size      = this % max_bound % val(:) - this % min_bound % val(:) + 1

    if (any(this % size % val < 1)) then
      print '(A, 10I8)', 'WARNING: Grid does not make sense, size = ', this % size % val
      return 
    end if

    success = .true.
  end function grid_bounds_set

  function compute_num_elements(grid_bounds) result(num_elements)
    implicit none
    class(grid_bounds_t) :: grid_bounds
    integer(C_INT64_T) :: num_elements

    num_elements = product(grid_bounds % size % val)
  end function compute_num_elements

  subroutine grid_bounds_print(grid_bounds)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    print '(3(A, 5I6, /), A, I9)',                          &
        'min_bound ', grid_bounds % min_bound % val,        &
        'max_bound ', grid_bounds % max_bound % val,        &
        'size      ', grid_bounds % size % val,             &
        'num elem  ', grid_bounds % compute_num_elements()

  end subroutine grid_bounds_print

end module grid_meta_module
