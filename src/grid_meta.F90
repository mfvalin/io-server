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
  integer, parameter, public :: IOSERVER_MAX_NUM_GRID_DIM = 5

  !> \brief Derived type that contains an index into a grid with up to 5
  !> (IOSERVER_MAX_NUM_GRID_DIM) dimensions.
  !> Can be used to describe a point within a grid, or the upper or lower
  !> bound of a grid
  type, public :: grid_index_t
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: val = (/ 1, 1, 1, 1, 1 /)
    contains
    procedure, pass :: assign_array4
    procedure, pass :: assign_array8
    generic :: assignment(=)  => assign_array4, assign_array8
    procedure, pass :: min    => grid_index_min
    procedure, pass :: max    => grid_index_max
  end type grid_index_t

  !> \brief Derived type that describes a grid with its lower and upper
  !> bounds
  type, public :: grid_bounds_t
    private
    type(grid_index_t) :: min_bound !< Minimum coordinate of the grid, in all its dimensions
    type(grid_index_t) :: max_bound !< Maximum coordinate of the grid, in all its dimensions
    type(grid_index_t) :: size      !< Size of the grid in all its dimensions
  contains
    procedure, pass :: compute_num_elements

    generic :: get_min  => grid_bounds_get_min, grid_bounds_get_min_1d
    generic :: get_max  => grid_bounds_get_max, grid_bounds_get_max_1d
    generic :: get_size => grid_bounds_get_size, grid_bounds_get_size_1d
    procedure, pass :: grid_bounds_get_min, grid_bounds_get_min_1d
    procedure, pass :: grid_bounds_get_max, grid_bounds_get_max_1d
    procedure, pass :: grid_bounds_get_size, grid_bounds_get_size_1d
    procedure, pass :: get_min_val  => grid_bounds_get_min_val
    procedure, pass :: get_max_val  => grid_bounds_get_max_val
    procedure, pass :: get_size_val => grid_bounds_get_size_val

    procedure, pass :: get_min_as_bytes
    procedure, pass :: get_max_as_bytes
    procedure, pass :: get_size_as_bytes

    generic :: set_min  => grid_bounds_set_min_index ,  grid_bounds_set_min_vals_i4,  grid_bounds_set_min_vals_i8
    generic :: set_max  => grid_bounds_set_max_index ,  grid_bounds_set_max_vals_i4,  grid_bounds_set_max_vals_i8
    generic :: set_size => grid_bounds_set_size_index, grid_bounds_set_size_vals_i4, grid_bounds_set_size_vals_i8
    procedure, pass :: grid_bounds_set_min_vals_i4, grid_bounds_set_min_vals_i8, grid_bounds_set_min_index
    procedure, pass :: grid_bounds_set_max_vals_i4, grid_bounds_set_max_vals_i8, grid_bounds_set_max_index
    procedure, pass :: grid_bounds_set_size_vals_i4, grid_bounds_set_size_vals_i8, grid_bounds_set_size_index
    procedure, pass :: set => grid_bounds_set

    procedure, pass :: print        => grid_bounds_print
    procedure, pass :: is_valid     => grid_bounds_is_valid
    procedure, pass :: intersection => grid_bounds_intersection
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

  function grid_index_min(lhs, rhs) result(min_index)
    implicit none
    class(grid_index_t), intent(in) :: lhs
    type(grid_index_t),  intent(in) :: rhs
    type(grid_index_t) :: min_index
    min_index = grid_index_t(merge(lhs % val, rhs % val, lhs % val <= rhs % val))
  end function grid_index_min

  function grid_index_max(lhs, rhs) result(min_index)
    implicit none
    class(grid_index_t), intent(in) :: lhs
    type(grid_index_t),  intent(in) :: rhs
    type(grid_index_t) :: min_index
    min_index = grid_index_t(merge(lhs % val, rhs % val, lhs % val >= rhs % val))
  end function grid_index_max

  function grid_bounds_get_min(grid_bounds) result(min_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: min_bound
    min_bound = grid_bounds % min_bound
  end function grid_bounds_get_min

  function grid_bounds_get_min_1d(grid_bounds, dim) result(min_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer, intent(in) :: dim
    integer(C_INT64_T) :: min_bound
    min_bound = grid_bounds % min_bound % val(dim)
  end function grid_bounds_get_min_1d

  function grid_bounds_get_max(grid_bounds) result(max_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: max_bound
    max_bound = grid_bounds % max_bound
  end function grid_bounds_get_max

  function grid_bounds_get_max_1d(grid_bounds, dim) result(max_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer, intent(in) :: dim
    integer(C_INT64_T) :: max_bound
    max_bound = grid_bounds % max_bound % val(dim)
  end function grid_bounds_get_max_1d

  function grid_bounds_get_size(grid_bounds) result(grid_size)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    type(grid_index_t) :: grid_size
    grid_size = grid_bounds % size
  end function grid_bounds_get_size

  function grid_bounds_get_size_1d(grid_bounds, dim) result(grid_size)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer, intent(in) :: dim
    integer(C_INT64_T) :: grid_size
    grid_size = grid_bounds % size % val(dim)
  end function grid_bounds_get_size_1d

  function grid_bounds_get_min_val(grid_bounds) result(min_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: min_bound
    min_bound = grid_bounds % min_bound % val
  end function grid_bounds_get_min_val

  function grid_bounds_get_max_val(grid_bounds) result(max_bound)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: max_bound
    max_bound = grid_bounds % max_bound % val
  end function grid_bounds_get_max_val

  function grid_bounds_get_size_val(grid_bounds) result(grid_size)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: grid_size
    grid_size = grid_bounds % size % val
  end function grid_bounds_get_size_val

  subroutine grid_bounds_set_min_index(grid_bounds, min_bound)
    implicit none
    class(grid_bounds_t), intent(inout) :: grid_bounds
    type(grid_index_t),   intent(in)    :: min_bound
    grid_bounds % min_bound = min_bound
    grid_bounds % max_bound = min_bound % val(:) + grid_bounds % size % val(:) - 1
    if (.not. grid_bounds % is_valid()) grid_bounds % size % val(:) = 0
  end subroutine grid_bounds_set_min_index

  subroutine grid_bounds_set_max_index(grid_bounds, max_bound)
    implicit none
    class(grid_bounds_t), intent(inout) :: grid_bounds
    type(grid_index_t),   intent(in)    :: max_bound
    grid_bounds % max_bound = max_bound
    grid_bounds % size      = grid_bounds % max_bound % val(:) - grid_bounds % min_bound % val(:) + 1
    if (.not. grid_bounds % is_valid()) grid_bounds % size % val(:) = 0
  end subroutine grid_bounds_set_max_index

  subroutine grid_bounds_set_size_index(grid_bounds, grid_size)
    implicit none
    class(grid_bounds_t), intent(inout) :: grid_bounds
    type(grid_index_t),   intent(in)    :: grid_size
    grid_bounds % size      = grid_size
    grid_bounds % max_bound = grid_bounds % min_bound % val(:) + grid_bounds % size % val(:) - 1
    if (.not. grid_bounds % is_valid()) grid_bounds % size % val(:) = 0
  end subroutine grid_bounds_set_size_index

  subroutine grid_bounds_set_min_vals_i8(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT64_T),           intent(in)    :: v1
    integer(C_INT64_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: min_bound
    min_bound % val(1) = v1
    if (present(v2)) min_bound % val(2) = v2
    if (present(v3)) min_bound % val(3) = v3
    if (present(v4)) min_bound % val(4) = v4
    if (present(v5)) min_bound % val(5) = v5
    call grid_bounds % grid_bounds_set_min_index(min_bound)
  end subroutine grid_bounds_set_min_vals_i8

  subroutine grid_bounds_set_min_vals_i4(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT32_T),           intent(in)    :: v1
    integer(C_INT32_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: min_bound
    min_bound % val(1) = v1
    if (present(v2)) min_bound % val(2) = v2
    if (present(v3)) min_bound % val(3) = v3
    if (present(v4)) min_bound % val(4) = v4
    if (present(v5)) min_bound % val(5) = v5
    call grid_bounds % grid_bounds_set_min_index(min_bound)
  end subroutine grid_bounds_set_min_vals_i4

  subroutine grid_bounds_set_max_vals_i8(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT64_T),           intent(in)    :: v1
    integer(C_INT64_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: max_bound
    max_bound % val(1) = v1
    if (present(v2)) max_bound % val(2) = v2
    if (present(v3)) max_bound % val(3) = v3
    if (present(v4)) max_bound % val(4) = v4
    if (present(v5)) max_bound % val(5) = v5
    call grid_bounds % grid_bounds_set_max_index(max_bound)
  end subroutine grid_bounds_set_max_vals_i8

  subroutine grid_bounds_set_max_vals_i4(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT32_T),           intent(in)    :: v1
    integer(C_INT32_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: max_bound
    max_bound % val(1) = v1
    if (present(v2)) max_bound % val(2) = v2
    if (present(v3)) max_bound % val(3) = v3
    if (present(v4)) max_bound % val(4) = v4
    if (present(v5)) max_bound % val(5) = v5
    call grid_bounds % grid_bounds_set_max_index(max_bound)
  end subroutine grid_bounds_set_max_vals_i4

  subroutine grid_bounds_set_size_vals_i8(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT64_T),           intent(in)    :: v1
    integer(C_INT64_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: size
    size % val(1) = v1
    if (present(v2)) size % val(2) = v2
    if (present(v3)) size % val(3) = v3
    if (present(v4)) size % val(4) = v4
    if (present(v5)) size % val(5) = v5
    call grid_bounds % grid_bounds_set_size_index(size)
  end subroutine grid_bounds_set_size_vals_i8

  subroutine grid_bounds_set_size_vals_i4(grid_bounds, v1, v2, v3, v4, v5)
    implicit none
    class(grid_bounds_t),         intent(inout) :: grid_bounds
    integer(C_INT32_T),           intent(in)    :: v1
    integer(C_INT32_T), optional, intent(in)    :: v2, v3, v4, v5

    type(grid_index_t) :: size
    size % val(1) = v1
    if (present(v2)) size % val(2) = v2
    if (present(v3)) size % val(3) = v3
    if (present(v4)) size % val(4) = v4
    if (present(v5)) size % val(5) = v5
    call grid_bounds % grid_bounds_set_size_index(size)
  end subroutine grid_bounds_set_size_vals_i4

  subroutine grid_bounds_set(this, min_bound, max_bound, size)
    implicit none
    class(grid_bounds_t), intent(inout) :: this             !< The grid_bounds_t instance we want to specify
    type(grid_index_t), intent(in), optional :: min_bound
    type(grid_index_t), intent(in), optional :: max_bound
    type(grid_index_t), intent(in), optional :: size

    if (present(min_bound) .and. present(max_bound) .and. .not. present(size)) then
      this % min_bound = min_bound
      call this % set_max(max_bound)
    else if (present(min_bound) .and. present(size) .and. .not. present(max_bound)) then
      this % min_bound = min_bound
      call this % set_size(size)
    else if (present(max_bound) .and. present(size) .and. .not. present(min_bound)) then
      this % min_bound = max_bound % val(:) - size % val(:) + 1
      call this % set_size(size)
    else
      this % size % val(:) = 0
    end if
  end subroutine grid_bounds_set

  function compute_num_elements(grid_bounds) result(num_elements)
    implicit none
    class(grid_bounds_t) :: grid_bounds
    integer(C_INT64_T) :: num_elements

    num_elements = product(grid_bounds % size % val)
  end function compute_num_elements

  function get_min_as_bytes(grid_bounds, elem_size) result(min_bytes)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer,              intent(in) :: elem_size
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: min_bytes

    min_bytes = grid_bounds % min_bound % val
    min_bytes(1) = (min_bytes(1) - 1) * elem_size + 1
  end function get_min_as_bytes

  function get_max_as_bytes(grid_bounds, elem_size) result(max_bytes)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer,              intent(in) :: elem_size
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: max_bytes

    max_bytes    = grid_bounds % max_bound % val
    max_bytes(1) = max_bytes(1) * elem_size
  end function get_max_as_bytes

  function get_size_as_bytes(grid_bounds, elem_size) result(grid_size_bytes)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    integer, intent(in) :: elem_size
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: grid_size_bytes

    grid_size_bytes    = grid_bounds % size % val
    grid_size_bytes(1) = grid_size_bytes(1) * elem_size
  end function get_size_as_bytes

  subroutine grid_bounds_print(grid_bounds)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    print '(3(A, 5I6, /), A, I12)',                         &
        'min_bound ', grid_bounds % min_bound % val,        &
        'max_bound ', grid_bounds % max_bound % val,        &
        'size      ', grid_bounds % size % val,             &
        'num elem  ', grid_bounds % compute_num_elements()
  end subroutine grid_bounds_print

  function grid_bounds_is_valid(grid_bounds) result(is_valid)
    implicit none
    class(grid_bounds_t), intent(in) :: grid_bounds
    logical :: is_valid
    is_valid = all(grid_bounds % size % val >= 1)
  end function grid_bounds_is_valid

  function grid_bounds_intersection(lhs, rhs) result(intersection)
    implicit none
    class(grid_bounds_t), intent(in) :: lhs
    type(grid_bounds_t),  intent(in) :: rhs
    type(grid_bounds_t) :: intersection

    call intersection % set(min_bound = lhs % min_bound % max(rhs % min_bound),   &
                            max_bound = lhs % max_bound % min(rhs % max_bound))
  end function grid_bounds_intersection

end module grid_meta_module
