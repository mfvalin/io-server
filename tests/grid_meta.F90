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

  logical :: success

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

  success = grid % set(min_bound = g_min, max_bound = g_max)
  a = grid % get_size()
  if (.not. success .or. .not. all( a % val == g_size % val) ) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong grid size, got ', a % val, ', should be ', g_size % val
    error stop 1
  end if

  if (grid % compute_num_elements() .ne. product(g_size % val)) then
    print *, 'ERROR: Wrong grid size, got ', grid % compute_num_elements(), ', should be ', product(g_size % val)
    error stop 1
  end if

  success = grid % set(min_bound = g_min, size = g_size)
  a = grid % get_max()
  if (.not. success .or. .not. all(a%val == g_max%val)) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong max bound, got ', a % val, ', should be ', g_max % val
    error stop 1
  end if

  success = grid % set(max_bound = g_max, size = g_size)
  a = grid % get_min()
  if (.not. success .or. .not. all(a%val == g_min%val)) then
    print '(A, 5I4, A, 5I4)', 'ERROR: Wrong min bound, got ', a % val, ', should be ', g_min % val
    error stop 1
  end if

  success = grid % set(min_bound = g_min)
  if (success) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  success = grid % set()
  if (success) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  success = grid % set(min_bound = g_min, max_bound = g_max, size = g_size)
  if (success) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  success = grid % set(min_bound = g_max, max_bound = g_min)
  if (success) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  success = grid % set(max_bound = g_max, size = g_min)
  if (success) then
    print *, 'ERROR: set grid vals should have failed'
    error stop 1
  end if

  print *, 'Test was successful'

end program grid_meta_test