!  serializer for FORTRAN programming
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

program grid_meta_test
  use iso_c_binding
  use grid_meta_module
  implicit none

  type(grid_index_t)  :: a, b, c
  type(grid_bounds_t) :: grid
  type(grid_index_t)  :: g_min, g_max, g_size

  integer(C_INT64_T), dimension(4) :: test_array
  integer(C_INT32_T), dimension(4) :: test_array_4

  test_array = (/5, 6, 7, 8/)
  test_array_4 = test_array
  test_array = test_array_4

  call print_grid_index(grid_index_t( (/ 3 /) ))
  call print_grid_index(grid_index_t( 3 ))
  call print_grid_index(grid_index_t( (/ 2, 4 /) ))
  call print_grid_index(grid_index_t( 2, 4 ))
  call print_grid_index(grid_index_t( (/ 1, 4, 5 /) ))
  call print_grid_index(grid_index_t( 1, 4, 5 ))
  call print_grid_index(grid_index_t( 1, 4, i5 = 5 ))
  call print_grid_index(grid_index_t( (/ 2, 3, 4, 5 /) ))
  call print_grid_index(grid_index_t( 2, 3, 4, 5 ))
  call print_grid_index(grid_index_t(test_array))
  call print_grid_index(grid_index_t(test_array_4))
  call print_grid_index(grid_index_t( (/ 1, 2, 3, 4, 5 /) ))
  call print_grid_index(grid_index_t( 1, 2, 3, 4, 5 ))
  call print_grid_index(grid_index_t( (/ 1_8, 2_8, 3_8, 4_8, 5_8 /) ))
  call print_grid_index(grid_index_t( 8_8, 9_8, 10_8, 11_8, 12_8 ))
  call print_grid_index(grid_index_t( (/ 9, 8, 7, 6, 5, 4, 3, 2 /) ))


  a % val(1:2) =(/ 1_8, 5_8 /)
  call print_grid_index(a)

  a = (/ 1 /)
  call print_grid_index(a)
  a = (/ 1, 2 /)
  call print_grid_index(a)
  a = (/ 1, 2, 3 /)
  call print_grid_index(a)
  a = (/ 1, 2, 3, 4 /)
  call print_grid_index(a)
  a = (/ 1, 2, 3, 4, 5 /)
  call print_grid_index(a)
  a = (/ 1_8, 2_8, 3_8, 4_8, 5_8 /)
  call print_grid_index(a)
  a = (/ 5, 4, 3, 6, 5, 4, 1, 2, 3, 4, 5 /)
  call print_grid_index(a)

  g_min  = (/ -1, -2, 1, 0 /)
  g_max  = (/ 1, 4, 3, 0 /)
  g_size = (/ 3, 7, 3, 1 /)

  call grid % set(min_bound = g_min, max_bound = g_max)
  a = grid % get_size()
  if (.not. grid % is_valid() .or. .not. all( a % val == g_size % val) ) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong grid size, got ', a % val, ', should be ', g_size % val
    error stop 1
  end if

  if (grid % compute_num_elements() .ne. product(g_size % val)) then
    print *, 'ERROR: Wrong grid size, got ', grid % compute_num_elements(), ', should be ', product(g_size % val)
    error stop 1
  end if

  call grid % set(min_bound = g_min, size = g_size)
  a = grid % get_max()
  if (.not. grid % is_valid() .or. .not. all(a%val == g_max%val)) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong max bound, got ', a % val, ', should be ', g_max % val
    error stop 1
  end if

  call grid % set(max_bound = g_max, size = g_size)
  a = grid % get_min()
  if (.not. grid % is_valid() .or. .not. all(a%val == g_min%val)) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong min bound, got ', a % val, ', should be ', g_min % val
    error stop 1
  end if

  call grid % set(min_bound = g_min)
  if (grid % is_valid()) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  call grid % set()
  if (grid % is_valid()) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  call grid % set(min_bound = g_min, max_bound = g_max, size = g_size)
  if (grid % is_valid()) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  call grid % set(min_bound = g_max, max_bound = g_min)
  if (grid % is_valid()) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  call grid % set(max_bound = g_max, size = g_min)
  if (grid % is_valid()) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  ! Min/max test
  block
    type(grid_index_t) :: expected_min, expected_max
    type(grid_index_t) :: input1, input2    
    type(grid_index_t) :: result_min, result_max

    input1 = (/ 1, 2, 4, -5 /)
    input2 = (/ 3, -2, 4, -3, 2 /)
    expected_min = (/ 1, -2, 4, -5, 1 /)
    expected_max = (/ 3, 2, 4, -3, 2 /)

    result_min = input1 % min(input2)
    result_max = input1 % max(input2)

    if (.not. all(result_min % val == expected_min % val)) then
      print *, 'ERROR: min'
      error stop 1
    end if

    if (.not. all(result_max % val == expected_max % val)) then
      print *, 'ERROR: max'
      error stop 1
    end if

    result_min = input2 % min(input1)
    result_max = input2 % max(input1)

    if (.not. all(result_min % val == expected_min % val)) then
      print *, 'ERROR: min'
      error stop 1
    end if

    if (.not. all(result_max % val == expected_max % val)) then
      print *, 'ERROR: max'
      error stop 1
    end if
  end block

  ! Intersection test
  block
    type(grid_bounds_t) :: grid1, grid2, grid3
    type(grid_bounds_t) :: int1, int2, int3, int4, int5, int6
    call grid1 % set(min_bound = grid_index_t(), max_bound = grid_index_t(5, 5, 5, 5))
    call grid2 % set(min_bound = grid_index_t(3, 3, 3, 3), max_bound = grid_index_t(6, 6, 6, 6))
    call grid3 % set(min_bound = grid_index_t(-1, -1, -1), max_bound = grid_index_t(2))

    int1 = grid1 % intersection(grid2)
    int2 = grid1 % intersection(grid3)
    int3 = grid2 % intersection(grid3)

    int4 = grid2 % intersection(grid1)
    int5 = grid3 % intersection(grid1)
    int6 = grid3 % intersection(grid2)

    if (int1 % compute_num_elements() .ne. 81 .or. int4 % compute_num_elements() .ne. 81) then
      print *, 'ERROR: intersection 1/4 is not valid'
      error stop 1
    end if

    if (int2 % compute_num_elements() .ne. 2 .or. int5 % compute_num_elements() .ne. 2) then
      print *, 'ERROR: intersection 2/5 is not valid'
      error stop 1
    end if

    if (int3 % is_valid() .or. int6 % is_valid()) then
      print *, 'ERROR: intersections 3/6 should not be valid'
    end if
  end block

  ! "As bytes" test
  block
    type(grid_bounds_t) :: grid, grid_bytes
    integer :: elem_size
    integer :: i_case
    integer(C_INT64_T), dimension(IOSERVER_MAX_NUM_GRID_DIM) :: min_bytes, max_bytes, size_bytes

    integer, parameter :: num_cases = 5
    integer, dimension(2, 2, num_cases) :: cases

    cases(:, 1, 1) = [-7, 1]
    cases(:, 2, 1) = [-3, 5]

    cases(:, 1, 2) = [1, 1]
    cases(:, 2, 2) = [5, 5]

    cases(:, 1, 3) = [0, 1]
    cases(:, 2, 3) = [1, 5]

    cases(:, 1, 4) = [-1, 1]
    cases(:, 2, 4) = [0, 5]

    cases(:, 1, 5) = [12, 1]
    cases(:, 2, 5) = [12, 5]

    do i_case = 1, num_cases
      call grid % set(min_bound = grid_index_t(cases(:, 1, i_case)), max_bound = grid_index_t(cases(:, 2, i_case)))
      ! print '(A, 5i4)', 'size: ', grid % get_size_val()

      do elem_size = 1, 12
        min_bytes  = grid % get_min_as_bytes(elem_size)
        max_bytes  = grid % get_max_as_bytes(elem_size)
        size_bytes = grid % get_size_as_bytes(elem_size)

        ! print '(A, 5I4)', 'min bytes:  ', min_bytes
        ! print '(A, 5I4)', 'max bytes:  ', max_bytes
        ! print '(A, 5I4)', 'size bytes: ', size_bytes

        call grid_bytes % set(min_bound = grid_index_t(min_bytes), max_bound = grid_index_t(max_bytes))
        if (.not. all(size_bytes == grid_bytes % get_size_val())) then
          print '(A)', 'ERROR: Got wrong byte size!!!'
          print '(A, 5i4)', '  expected ', size_bytes
          print '(A, 5i4)', '  got      ', grid_bytes % get_size_val()
          error stop 1
        end if

        call grid_bytes % set(min_bound = grid_index_t(min_bytes), size = grid_index_t(size_bytes))
        if (.not. all(max_bytes == grid_bytes % get_max_val())) then
          print *, 'ERROR: Got wrong byte max'
          error stop 1
        end if

        call grid_bytes % set(max_bound = grid_index_t(max_bytes), size = grid_index_t(size_bytes))
        if (.not. all(min_bytes == grid_bytes % get_min_val())) then
          print *, 'ERROR: Got wrong byte min'
          error stop 1
        end if

      end do
    end do

  end block

  print *, 'Test was successful'

end program grid_meta_test