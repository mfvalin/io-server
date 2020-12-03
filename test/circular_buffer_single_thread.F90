! Copyright (C) 2020 Recherche en Prevision Numerique
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation,
! version 2 of the License.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this program; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.

program test_circular_buffer_single_thread
  implicit none

  call basic_test()

end program test_circular_buffer_single_thread


subroutine basic_test()

  use iso_c_binding
  use circular_buffer_module, only: DATA_ELEMENT, circular_buffer
  implicit none

  integer, parameter :: BUFFER_SIZE = 5001;

  integer, parameter :: NUM_ELEM = BUFFER_SIZE * 3;
  integer, parameter :: STEP_SIZE = 4;

  type(circular_buffer) :: circ_buffer
  integer(DATA_ELEMENT), dimension(NUM_ELEM) :: local, local_out
  logical :: success
  integer :: i, num_elem_in, num_elem_free
  integer :: num_error
  integer :: buffer_limit

  num_error = 0
  do i = 1, size(local)
    local(i) = i
  enddo

  local_out(:) = -1
  success = circ_buffer % create(BUFFER_SIZE)

  if (.not. success) then
    num_error = num_error + 1
    print *, 'AAAAHHHHHH Could not create buffer'
    return
  end if

  buffer_limit = circ_buffer % get_available_space()

!  call circ_buffer % print_header()

  do i = 1, NUM_ELEM, STEP_SIZE
    num_elem_free = circ_buffer % atomic_put(local(i), STEP_SIZE)
    num_elem_in = circ_buffer % get_available_data()

    if (num_elem_in .ne. STEP_SIZE) then
      num_error = num_error + 1
!      print *, 'Wrong number of elements in buffer after adding data'
    end if

    num_elem_free = circ_buffer % get_available_space()
    if (num_elem_free .ne. buffer_limit - STEP_SIZE) then
      num_error = num_error + 1
!      print *, 'Wrong space available in buffer after adding data', num_elem_free, buffer_limit - STEP_SIZE
    end if

    num_elem_in = circ_buffer % atomic_get(local_out(i), STEP_SIZE)
    num_elem_free = circ_buffer % get_available_space()

    if (num_elem_in .ne. 0) then
      num_error = num_error + 1
!      print *, 'We read everything, but there still seems to be some data left!'
    end if

    if (num_elem_free .ne. buffer_limit) then
      num_error = num_error + 1
!      print *, 'We read everything, but available space != buffer limit'
    end if

    if (.not. all(local(i:i+STEP_SIZE -1) == local_out(i:i+STEP_SIZE -1))) then
      num_error = num_error + 1
!      print *, 'Did not read the same data back!'
    end if
  end do

  if (num_error > 0) then
    print *, 'AAAAAhhhhh we found errors when running the test!!', num_error
  else
    print *, 'All good for basic test (create, put, get, data, space)'
  end if

end subroutine basic_test
