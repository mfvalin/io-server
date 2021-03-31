! Copyright (C) 2021  Environnement Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

module parameters
  implicit none
  public

  integer, parameter :: NUM_BUFFER_ELEMENTS = 200
  integer, parameter :: NUM_DATA_ELEMENTS_SMALL = 10
  integer, parameter :: NUM_DATA_ELEMENTS_LARGE = 5000
  integer, parameter :: STEP_SIZE = 5
  integer, parameter :: NUM_CONSUMERS = 2

  integer, parameter :: SECRET_TEST_VALUE = 123

end module parameters


function test_dcb_channel(buffer) result(num_errors)
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none
  class(distributed_circular_buffer), intent(inout) :: buffer
  integer :: num_errors
  integer :: return_value

  return_value = buffer % start_listening()

  num_errors = 1
  if (return_value == 0) num_errors = 0
end function test_dcb_channel


function test_dcb_consumer(buffer, rank) result(num_errors)
  use iso_c_binding
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use circular_buffer_module
  use parameters
  implicit none

  interface
    subroutine init_array(array, rank)
      import :: DATA_ELEMENT
      implicit none
      integer(DATA_ELEMENT), dimension(:), intent(inout) :: array
      integer, intent(in) :: rank
    end subroutine init_array
  end interface

  class(distributed_circular_buffer), intent(inout) :: buffer
  integer, intent(in) :: rank
  integer :: num_errors

  integer        :: i, i_prod
  integer(C_INT) :: num_elements, num_producers, consumer_id, capacity
  integer :: first_prod, num_prod_local, last_prod
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_SMALL) :: data_small, expected_data_small
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_LARGE) :: data_large, expected_data_large

  num_errors = 0
  num_producers = buffer % get_num_producers()
  consumer_id   = buffer % get_consumer_id()
  capacity      = buffer % get_capacity()

  num_prod_local = ceiling(real(num_producers) / NUM_CONSUMERS)
  first_prod     = consumer_id * num_prod_local
  last_prod      = min(first_prod + num_prod_local, num_producers) - 1

  do i = 0, num_producers - 1
    num_elements = buffer % get_num_elements(i)
    if (num_elements .ne. 0) num_errors = num_errors + 1
  end do

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Producers partially put stuff
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  if (consumer_id == 0) then
    print *, 'DOING CHECKS'
    do i = 0, num_producers - 1
      num_elements = buffer % get_num_elements(i)
      if (num_elements .ne. 0) num_errors = num_errors + 1
    end do
  end if
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Producers commit their store from previous operation
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  if (consumer_id == 0) then
    do i = 0, num_producers - 1
      num_elements = buffer % get_num_elements(i)
      if (num_elements .ne. 1) then
        print *, 'There should be exactly 1 element!'
        num_errors = num_errors + 1
      end if

      num_elements = buffer % peek(i, data_small, 1)
      if (data_small(1) .ne. SECRET_TEST_VALUE) then
        print *, 'Value seems wrong (after peeking)', data_small(1), SECRET_TEST_VALUE
        num_errors = num_errors + 1
      end if
      if (num_elements .ne. 1) then
        print *, 'There should be exactly 1 element!'
        num_errors = num_errors + 1
      end if

      data_small(1) = -1
      num_elements = buffer % get(i, data_small, 1, .false.)
      if (data_small(1) .ne. SECRET_TEST_VALUE) then
        print *, 'Value seems wrong (after reading)', data_small(1), SECRET_TEST_VALUE
        num_errors = num_errors + 1
      end if
      if (num_elements .ne. 0) then
        print *, 'There should be exactly 1 element!'
        num_errors = num_errors + 1
      end if

    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Producers do checks
  !---------------------------
  call buffer % full_barrier()
  !---------------------------

   if (consumer_id == 0) then
     do i = 0, num_producers - 1
       num_elements = buffer % get(i, data_small, 0, .true.)
     end do
   end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  do i = 0, num_producers - 1
    num_elements = buffer % get_num_elements(i)
    if (num_elements .ne. NUM_DATA_ELEMENTS_SMALL) then
      num_errors = num_errors + 1
      print *, 'Wrong number of elements in buffer !!!!!!!!', rank, i, num_elements
    end if
  end do

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  do i_prod = first_prod, last_prod
    call init_array(expected_data_small, i_prod)

    num_elements = buffer % get(i_prod, data_small, NUM_DATA_ELEMENTS_SMALL, .true.)

    if (num_elements .ne. 0) then
      num_errors = num_errors + 1
      print *, 'Error, buffer should be empty but has ', num_elements
    end if
    if (.not. all(data_small == expected_data_small)) then
      num_errors = num_errors + 1
!      print *, 'Error, got ', data_small
!      print *, 'Expected   ', expected_data_small
    end if
  end do

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  do i_prod = first_prod, last_prod
    call init_array(expected_data_large, i_prod)
    !do i = 1, NUM_DATA_ELEMENTS_LARGE
    !print *, 'expected ', expected_data_large(:)
    !end do
    do i = 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE

      num_elements = buffer % get(i_prod, data_large(i : i + STEP_SIZE - 1), STEP_SIZE, .true.)

      if (.not. all(data_large(i:i + STEP_SIZE - 1) == expected_data_large(i : i + STEP_SIZE - 1))) then
        num_errors = num_errors + 1
!        print *, 'Wrong data!!!', data_large(i:i+STEP_SIZE - 1), '--', expected_data_large(i : i + STEP_SIZE - 1)
!        print *, 'Earlier data:', data_large(i - STEP_SIZE: i - 1), '--', expected_data_large(i - STEP_SIZE:i - 1)
      end if

    end do

    num_elements = buffer % get_num_elements(i_prod)
    if (num_elements .ne. 0) then
      print *, 'Buffer is not empty after doing all the reads!!!', num_elements
      num_errors = num_errors + 1
    end if
  end do

  !call buffer % server_barrier()

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Producers fill buffers completely
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  do i_prod = first_prod, last_prod
    num_elements = buffer % get_num_elements(i_prod)
    if (num_elements .ne. capacity) then
      print *, 'Buffer should be completely full!', num_elements, capacity
      num_errors = num_errors + 1
    end if

    ! Should return -1
    num_elements = buffer % get(i_prod, data_large, capacity + 1, .true.)
    if (num_elements .ne. -1) then
      print *, 'Wrong result when trying to extract more than capacity', num_elements
      num_errors = num_errors + 1
    end if

    num_elements = buffer % get(i_prod, data_large, capacity, .true.)
    if (num_elements .ne. 0) then
      print *, 'Buffer is not empty after reading its entire content in one call!!!', num_elements
      num_errors = num_errors + 1
    end if
  end do
  !---------------------------
  call buffer % full_barrier()
  !---------------------------

end function test_dcb_consumer


function test_dcb_producer(buffer, rank) result(num_errors)
  use iso_c_binding
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use circular_buffer_module
  use parameters
  implicit none

  interface
    subroutine init_array(array, rank)
      import :: DATA_ELEMENT
      implicit none
      integer(DATA_ELEMENT), dimension(:), intent(inout) :: array
      integer, intent(in) :: rank
    end subroutine init_array
  end interface

  class(distributed_circular_buffer), intent(inout) :: buffer
  integer, intent(in) :: rank
  integer :: num_errors
  integer :: producer_id
  logical :: success

  integer(C_INT) :: num_spaces, capacity
  integer :: i
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_SMALL) :: data_small
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_LARGE) :: data_large

  num_errors = 0

  capacity = buffer % get_capacity()
  producer_id = buffer % get_producer_id()

  print *, 'Capacity: ', capacity, buffer % get_num_spaces(.false.)
  print *, '          ', buffer % get_num_spaces(.true.)

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  print *, 'Partially putting a value'
  data_small(1) = SECRET_TEST_VALUE
  num_spaces = buffer % put(data_small, 1, .false.)

  if (num_spaces .ne. capacity - 1) then
    print *, 'Put something, but available space is wrong!', num_spaces, capacity - 1
    num_errors = num_errors + 1
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Consumer does checks
  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  print *, 'Fully committing buffer content'
  num_spaces = buffer % put(data_small, 0, .true.)
  if (num_spaces .ne. capacity - 1) then
    print *, 'Put something, but available space is wrong!', num_spaces, capacity - 1
    num_errors = num_errors + 1
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Consumer does checks
  ! Consumer peeks at data
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  num_spaces = buffer % get_num_spaces(.true.)
  if (num_spaces .ne. capacity - 1) then
    print *, 'Consumer only peeked, available space is wrong', num_spaces, capacity - 1
    num_errors = num_errors + 1
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Consumer extracts the data element
  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  num_spaces = buffer % get_num_spaces(.true.)
  if (num_spaces .ne. capacity) then
    print *, 'Consumer removed everything, available space is wrong', num_spaces, capacity
    num_errors = num_errors + 1
  end if

  call init_array(data_small, producer_id)
  num_spaces = buffer % put(data_small, NUM_DATA_ELEMENTS_SMALL, .true.)

  if (num_spaces < 0) then
    num_errors = num_errors + 1
    print *, 'Unable to insert stuff in the buffer!!!!!'
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  num_spaces = buffer % get_num_spaces(.true.)
  if (num_spaces .ne. capacity) then
    num_errors = num_errors + 1
    print *, 'There should be nothing in the buffer!'
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  call init_array(data_large, producer_id)
  do i = 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE
    num_spaces = buffer % put(data_large(i:i + STEP_SIZE - 1), STEP_SIZE, .true.)
  end do

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  num_spaces = buffer % get_num_spaces(.true.)
  if (num_spaces .ne. capacity) then
    num_errors = num_errors + 1
    print *, 'There should be nothing in the buffer!'
  end if

  ! Should return -1
  num_spaces = buffer % put(data_large, capacity + 1, .true.)
  if (num_spaces .ne. -1) then
    print *, 'Wrong return value when trying to put more than max capacity', num_spaces
    num_errors = num_errors + 1
  end if

  ! If call returns -1, we have a problem
  num_spaces = buffer % put(data_large, capacity, .true.)

  if (num_spaces .ne. 0) then
    print *, 'Error when filling buffer to capacity'
    num_errors = num_errors + 1
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

end function test_dcb_producer


program test_distributed_circular_buffer
  use ISO_C_BINDING
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use parameters
  implicit none

  include 'mpif.h'
  include 'io-server/common.inc'
  include 'test_common.inc'

  interface
  subroutine init_array(array, rank)
    import :: DATA_ELEMENT
    implicit none
    integer(DATA_ELEMENT), dimension(:), intent(inout) :: array
    integer, intent(in) :: rank
  end subroutine init_array
  function test_dcb_channel(buffer) result(num_errors)
    use distributed_circular_buffer_module, only : distributed_circular_buffer
    implicit none
    class(distributed_circular_buffer), intent(inout) :: buffer
    integer :: num_errors
  end function test_dcb_channel
  function test_dcb_consumer(buffer, rank) result(num_errors)
    use distributed_circular_buffer_module, only : distributed_circular_buffer
    implicit none
    class(distributed_circular_buffer), intent(inout) :: buffer
  integer, intent(in) :: rank
    integer :: num_errors
  end function test_dcb_consumer
  function test_dcb_producer(buffer, rank) result(num_errors)
    use distributed_circular_buffer_module, only : distributed_circular_buffer
    implicit none
    class(distributed_circular_buffer), intent(inout) :: buffer
    integer, intent(in) :: rank
    integer :: num_errors
  end function test_dcb_producer
  end interface

  integer :: error
  integer :: rank, comm_size
  logical :: success
  integer :: num_errors, tmp_errors
  integer :: server_comm, server_group, world_group
  integer, dimension(3, 1) :: incl_range

  type(distributed_circular_buffer) :: circ_buffer
  integer :: num_producers, consumer_id, channel_id, producer_id

  ! Initialization

  num_errors = 0

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)

  if (comm_size < 3) then
    print *, 'Need at least 3 processes for this test!'
    num_errors = num_errors + 1
    goto 777
  end if

  num_producers = comm_size - 2*NUM_CONSUMERS;

  server_comm = MPI_COMM_NULL
  call MPI_Comm_group(MPI_COMM_WORLD, world_group, error)

  if (rank < NUM_CONSUMERS * 2) then
    incl_range(1, 1) = 0
    incl_range(2, 1) = 2 * NUM_CONSUMERS - 1;
    incl_range(3, 1) = 1
    call MPI_Group_range_incl(world_group, 1, incl_range, server_group, error)
    call MPI_Comm_create_group(MPI_COMM_WORLD, server_group, 0, server_comm, error)
  end if


  ! Beginning of test
  success = circ_buffer % create(MPI_COMM_WORLD, server_comm, num_producers, NUM_CONSUMERS, NUM_BUFFER_ELEMENTS)

  if (.not. success) then
    print *, 'Could not create a circular buffer!', rank
    num_errors = num_errors + 1
    goto 777
  end if

  consumer_id = circ_buffer % get_consumer_id()
  channel_id  = circ_buffer % get_channel_id()
  producer_id = circ_buffer % get_producer_id()

!  print *, 'rank, prod, channel, consume ', rank, producer_id, channel_id, consumer_id


  if (.not. circ_buffer % is_valid()) then
    print *, 'Something wrong with the newly-created buffer!!!'
    num_errors = num_errors + 1
    goto 777
  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (consumer_id >= 0) then
    num_errors =  test_dcb_consumer(circ_buffer, rank)
  else if (producer_id >= 0) then
    num_errors = test_dcb_producer(circ_buffer, rank)
  else if (channel_id >= 0) then
    num_errors = test_dcb_channel(circ_buffer)
  else
    num_errors = 1
  end if

!  call circ_buffer % print()
  call circ_buffer % delete()
  if (consumer_id == 0) then
    call MPI_Group_free(server_group, error)
    call MPI_Comm_free(server_comm, error)
  end if

777 CONTINUE

  tmp_errors = num_errors
  call MPI_Reduce(tmp_errors, num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, error)

  if(consumer_id == 0) then  ! check that we got back what we sent
    if(num_errors > 0) then
      print *, 'ERRORS IN DISTRIBUTED BUFFER TEST ', num_errors
    else
      print *, 'Distributed circular buffer test has succeeded'
    end if
  endif

  call MPI_finalize(error)

  if (num_errors > 0) call exit(num_errors)

end program test_distributed_circular_buffer

subroutine init_array(array, rank)
  use circular_buffer_module, only: DATA_ELEMENT
  implicit none

  integer(DATA_ELEMENT), dimension(:), intent(inout) :: array
  integer, intent(in) :: rank

  integer :: i
  do i = 1, size(array)
    array(i) = (rank + 1) * 10000 + i
  end do
end subroutine init_array
