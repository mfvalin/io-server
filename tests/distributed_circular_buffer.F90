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

module parameters
  use ISO_C_BINDING
  implicit none
  public

  integer(C_SIZE_T), parameter :: STEP_SIZE = 5
  integer(C_SIZE_T), parameter :: BUFFER_SIZE_BYTE = 2044 * 4
  integer(C_SIZE_T), parameter :: NUM_DATA_ELEMENTS_SMALL = 10
  integer(C_SIZE_T), parameter :: NUM_DATA_ELEMENTS_LARGE = BUFFER_SIZE_BYTE / 4 * STEP_SIZE
  integer, parameter :: NUM_CONSUMERS = 2

  integer, parameter :: SECRET_TEST_VALUE = 123

  integer, parameter :: MAX_NUM_PROCS = 20

end module parameters


function test_dcb_channel(buffer) result(num_errors)
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none
  class(distributed_circular_buffer), intent(inout) :: buffer
  integer :: num_errors
  logical :: success

  success = buffer % start_listening()

  num_errors = 1
  if (success) num_errors = 0

  if (num_errors > 0) then
    print *, 'Error in CHANNEL process...'
    error stop 1
  end if
end function test_dcb_channel

function test_dcb_consumer_producer(buffer, rank) result(num_errors)
  use iso_c_binding
  use distributed_circular_buffer_module
  use circular_buffer_module
  use parameters
  use rpn_extra_module
  implicit none

  interface
    subroutine init_array(array, rank)
      implicit none
      integer, dimension(:), intent(inout) :: array
      integer, intent(in) :: rank
    end subroutine init_array
  end interface

  class(distributed_circular_buffer), intent(inout) :: buffer
  integer, intent(in) :: rank
  integer :: num_errors

  integer            :: i_prod, i_data
  integer(C_INT)     :: num_server_bound_instances, server_bound_server_id, server_bound_client_id
  integer(C_INT64_T) :: num_elements, num_spaces, capacity
  integer :: first_prod, num_prod_local, last_prod
  integer, dimension(NUM_DATA_ELEMENTS_SMALL) :: data_small, expected_data_small
  integer, dimension(NUM_DATA_ELEMENTS_LARGE) :: data_large, expected_data_large
  logical :: success
  logical :: is_server_bound_server, is_server_bound_client

  num_errors = 0

  server_bound_server_id     = buffer % get_server_bound_server_id()
  server_bound_client_id     = buffer % get_server_bound_client_id()
  num_server_bound_instances = buffer % get_num_server_bound_clients()

  is_server_bound_server = (server_bound_server_id >= 0)
  is_server_bound_client = (server_bound_client_id >= 0)

  ! Put dummy data to make it easier to detect errors later
  do i_data=1, NUM_DATA_ELEMENTS_SMALL
    data_small(i_data) = 1000 + i_data
    expected_data_small(i_data) = 2000 + i_data
  end do
  do i_data=1, NUM_DATA_ELEMENTS_LARGE
    data_large(i_data) = 30000 + i_data
    expected_data_large(i_data) = 40000 + i_data
  end do

  ! Initialization
  if (is_server_bound_server) then
    num_prod_local = ceiling(real(num_server_bound_instances) / NUM_CONSUMERS)
    first_prod     = server_bound_server_id * num_prod_local
    last_prod      = min(first_prod + num_prod_local, num_server_bound_instances) - 1

    print *, 'Server: first/last prod = ', first_prod, last_prod

    ! Check that the buffers are empty
    do i_prod = 0, num_server_bound_instances - 1
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 0) then 
        num_errors = num_errors + 1
        print *, 'New buffer is not empty!'
        error stop 1
      end if
    end do

    num_elements = buffer % get_num_elements(-1, CB_KIND_INTEGER_4) ! Should return an error
    if (num_elements >= 0) then
      print *, 'Invalid buffer ID still returned a valid number of elements', num_elements
      error stop 1
    end if

    ! Check that the buffers have the requested capacity (+/- one 8-byte element)
    do i_prod = 0, num_server_bound_instances - 1
      capacity = buffer % get_capacity(i_prod, CB_KIND_CHAR)
      if (capacity < BUFFER_SIZE_BYTE .or. capacity > BUFFER_SIZE_BYTE + 8) then
        print '(A,I10,A,I10)', 'Buffer has the wrong capacity! Requested/got', BUFFER_SIZE_BYTE, ' / ', capacity
        num_errors = num_errors + 1
        error stop 1
      end if
    end do


  else if (is_server_bound_client) then
    capacity = buffer % get_capacity(CB_KIND_INTEGER_4)

    print *, 'Client, rank = ', rank
    print *, 'Capacity: ', capacity, buffer % get_num_spaces(CB_KIND_INTEGER_4, .false.)
    print *, '          ', buffer % get_num_spaces(CB_KIND_INTEGER_4, .true.)

  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients partially insert a value
  if (is_server_bound_client) then
    print *, 'Partially putting a value'
    data_small(1) = SECRET_TEST_VALUE
    data_small(2) = SECRET_TEST_VALUE
    success = buffer % put_elems(data_small, 1_8, CB_KIND_INTEGER_8, .false.)
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .false.)

    if (num_spaces > capacity - 1 .or. .not. success) then
      print *, 'Put something, but available space is wrong!', num_spaces, capacity - 1
      num_errors = num_errors + 1
      error stop 1
    end if
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Servers check that the buffers still have no available data
  if (is_server_bound_server) then
    print *, 'Server consumer DOING CHECKS'
    do i_prod = 0, num_server_bound_instances - 1
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 0) then 
        num_errors = num_errors + 1
        print *, 'Buffer should be empty!!!! bahhhh'
        error stop 1
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients commit content and check that the space is now taken
  ! (not updating buffer status from server)
  if (is_server_bound_client) then
    print *, 'Fully committing buffer content'
    success = buffer % put_elems(data_small, 0_8, CB_KIND_INTEGER_8, .true.)
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_8, .false.)
    print *, 'Fully committed buffer content'

    if (num_spaces > (capacity - 1)  .or. .not. success) then
      print *, 'Put something, but available space is wrong!', num_spaces, capacity - 1
      num_errors = num_errors + 1
      error stop 1
    end if

    num_spaces = buffer % get_num_spaces(CB_KIND_CHAR, .false.)
    if (num_spaces > (capacity - 1) * 8) then 
      print *, 'Wrong number of spaces (char)', num_spaces
      num_errors = num_errors + 1
      error stop 1
    end if

    num_spaces = buffer % get_num_spaces(CB_KIND_REAL_8, .false.)
    if (num_spaces .ne. (capacity - 1) / 2) then 
      print *, 'Wrong number of spaces (real_8)', num_spaces
      num_errors = num_errors + 1
      error stop 1
    end if
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! "root" server checks that the test value was properly written in every CB (just peeking at it, no extraction)
  if (server_bound_server_id == 0) then
    do i_prod = 0, num_server_bound_instances - 1
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 2) then
        print *, 'There should be exactly 2 4-byte element!', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if

      num_elements = buffer % get_num_elements(i_prod, CB_KIND_CHAR)
      if (num_elements .ne. 8) then 
        print *, 'There should be exactly 8 characters', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if

      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_8)
      if (num_elements .ne. 1) then 
        print *, 'There should be 1 8-byte integer', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if

      success = buffer % peek_elems(i_prod, data_small, 1_8, CB_KIND_INTEGER_4)
      if (data_small(1) .ne. SECRET_TEST_VALUE) then
        print *, 'Value seems wrong (after peeking)', data_small(1), SECRET_TEST_VALUE
        num_errors = num_errors + 1
        error stop 1
      end if

      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 2 .or. .not. success) then
        print *, 'There should be exactly 2 elements (4 bytes each)!'
        num_errors = num_errors + 1
        error stop 1
      end if

      data_small(1) = -1
      success = buffer % get_elems(i_prod, data_small, 1_8, CB_KIND_INTEGER_4, .false.)
      if (data_small(1) .ne. SECRET_TEST_VALUE) then
        print *, 'Value seems wrong (after peeking)', data_small(1), SECRET_TEST_VALUE
        num_errors = num_errors + 1
        error stop 1
      end if

      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 0 .or. .not. success) then
        print *, 'There should be exactly 1 element!'
        num_errors = num_errors + 1
        error stop 1
      end if

    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients check that the value is still there
  if (is_server_bound_client) then
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .true.)
    if (num_spaces .ne. capacity - 2) then
      print *, 'Server only peeked, available space is wrong', num_spaces, capacity - 2
      num_errors = num_errors + 1
      error stop 1
    end if
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! root server extract the test value
  if (server_bound_server_id == 0) then
    do i_prod = 0, num_server_bound_instances - 1
      success = buffer % get_elems(i_prod, data_small, 0_8, CB_KIND_INTEGER_4, .true.)
      if (.not. success) then
        print *, 'ERROR, buffer%get_elems failed (0)'
        error stop 1
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients check that their CB is now empty
  if (is_server_bound_client) then
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .true.)
    if (num_spaces .ne. capacity) then
      print *, 'Server removed everything, available space is wrong', num_spaces, capacity
      num_errors = num_errors + 1
      error stop 1
    end if
  end if

  ! Clients init and insert a small message into their CB
  if (is_server_bound_client) then
    call init_array(data_small, server_bound_client_id)
    success = buffer % put_elems(data_small, NUM_DATA_ELEMENTS_SMALL, CB_KIND_INTEGER_4, .true.)

    if (.not. success) then
      num_errors = num_errors + 1
      print *, 'Unable to insert stuff in the buffer!!!!!'
      error stop 1
    end if
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Servers all check that every CB contains data with the expected size
  if (is_server_bound_server) then
    do i_prod = 0, num_server_bound_instances - 1
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. NUM_DATA_ELEMENTS_SMALL) then
        num_errors = num_errors + 1
        print *, 'Wrong number of elements in buffer !!!!!!!!', rank, i_prod, num_elements
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Servers extract the small message from their *assigned CBs* and check that it is correct
  if (is_server_bound_server) then
    do i_prod = first_prod, last_prod
      call init_array(expected_data_small, i_prod)

      success = buffer % get_elems(i_prod, data_small, NUM_DATA_ELEMENTS_SMALL, CB_KIND_INTEGER_4, .true.)
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)

      if (num_elements .ne. 0 .or. .not. success) then
        num_errors = num_errors + 1
        print *, 'Error, buffer should be empty but has ', num_elements, success
        error stop 1
      end if
      if (.not. all(data_small == expected_data_small)) then
        num_errors = num_errors + 1
        ! print *, 'Error, got ', data_small
        ! print *, 'Expected   ', expected_data_small
        print *, 'Data compare error!'
        error stop 1
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients check that their CB is now empty
  if (is_server_bound_client) then
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .true.)
    if (num_spaces .ne. capacity) then
      num_errors = num_errors + 1
      print *, 'There should be nothing in the buffer!'
      error stop 1
    end if
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

  ! Clients send the large message (larger than CB size), starting with elements of different sizes
  ! Since each server will read an entire CB message before going to the next, most CBs will become
  ! full before the message can be completely inserted
  if (is_server_bound_client) then
    call init_array(data_large, server_bound_client_id)

    ! Try putting char-sized elements
    success = buffer % put_elems(data_large(1:STEP_SIZE), STEP_SIZE * 4, CB_KIND_CHAR, .true.)
    if (.not. success) then
      print *, 'Error using buffer%put'
      error stop 1
    end if

    ! Try putting real8-sized elements
    success = buffer % put_elems(data_large(STEP_SIZE + 1), (STEP_SIZE - mod(STEP_SIZE, 2_8)) / 2, CB_KIND_REAL_8, .true.)
    if (mod(STEP_SIZE, 2_8) > 0) then 
      success = buffer % put_elems(data_large(STEP_SIZE + STEP_SIZE), 1_8, CB_KIND_REAL_4, .true.) .and. success
    end if
    if (.not. success) then 
        print *, 'Error using buffer%put'
        error stop 1
    end if
    
    ! The rest of the large message
    do i_data = STEP_SIZE * 2 + 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE
      success = buffer % put_elems(data_large(i_data:i_data + STEP_SIZE - 1), STEP_SIZE, CB_KIND_INTEGER_4, .true.)
      if (.not. success) then
        print *, 'Error using buffer%put'
        error stop 1
      end if
    end do

  ! In the meantime, servers just read the large message. (Not doing different element sizes since we tested that earlier)
  else if (is_server_bound_server) then
    do i_prod = first_prod, last_prod
      call init_array(expected_data_large, i_prod)
      do i_data = 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE
        success = buffer % get_elems(i_prod, data_large(i_data), STEP_SIZE, CB_KIND_INTEGER_4, .true.)
        if (.not. success) then
          print '(A, I4, A, I10)', 'ERROR, buffer%get_elems failed! (1). i_prod/i_data ', i_prod, ' / ', i_data
          if (server_bound_server_id == 0) call buffer % print(.true.)
          call sleep_us(500000)
          error stop 1
        end if

        if (.not. all(data_large(i_data:i_data + STEP_SIZE - 1) == expected_data_large(i_data : i_data + STEP_SIZE - 1))) then
          num_errors = num_errors + 1
          print *, 'Wrong data!!!', data_large(i_data:i_data+STEP_SIZE - 1), '--', expected_data_large(i_data : i_data + STEP_SIZE - 1)
          print *, 'Earlier data:', data_large(i_data - STEP_SIZE: i_data - 1), '--', expected_data_large(i_data - STEP_SIZE:i_data - 1)
          print *, 'Data (large) compare error!'
          error stop 1
        end if
      end do

      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 0) then
        print *, 'Buffer is not empty after doing all the reads!!!', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Clients check that their CB is empty
  ! They then try to add more than capacity in one call
  ! They then fill their CB exactly, in one call
  if (is_server_bound_client) then
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .true.)
    if (num_spaces .ne. capacity) then
      num_errors = num_errors + 1
      print *, 'There should be nothing in the buffer!'
      error stop 1
    end if

    ! Should fail
    success = buffer % put_elems(data_large, capacity + 1, CB_KIND_INTEGER_4, .true.)
    if (success) then
      print *, 'Wrong return value when trying to put more than max capacity', num_spaces
      num_errors = num_errors + 1
      error stop 1
    end if

    ! If this one fails, we have a problem
    success = buffer % put_elems(data_large, capacity, CB_KIND_INTEGER_4, .true.)
    num_spaces = buffer % get_num_spaces(CB_KIND_INTEGER_4, .false.)

    if (num_spaces .ne. 0 .or. .not. success) then
      print *, 'Error when filling buffer to capacity'
      num_errors = num_errors + 1
      error stop 1
    end if
  end if
  
  !---------------------------
  call buffer % full_barrier()
  !---------------------------
  ! Servers check that their assigned CBs are exactly full, then extract the data
  if (is_server_bound_server) then
    do i_prod = first_prod, last_prod
      capacity = buffer % get_capacity(i_prod, CB_KIND_INTEGER_4)
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if ((num_elements .ne. capacity) .or. (num_elements < 0)) then
        print *, 'Buffer should be completely full!', num_elements, capacity
        num_errors = num_errors + 1
        error stop 1
      end if

      ! Should return -1
      success = buffer % get_elems(i_prod, data_large, capacity + 1, CB_KIND_INTEGER_4, .true.)
      if (success) then
        print *, 'Wrong result when trying to extract more than capacity', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if

      success = buffer % get_elems(i_prod, data_large, capacity, CB_KIND_INTEGER_4, .true.)
      num_elements = buffer % get_num_elements(i_prod, CB_KIND_INTEGER_4)
      if (num_elements .ne. 0 .or. .not. success) then
        print *, 'Buffer is not empty after reading its entire content in one call!!!', num_elements
        num_errors = num_errors + 1
        error stop 1
      end if
    end do
  end if

  !---------------------------
  call buffer % full_barrier()
  !---------------------------

end function test_dcb_consumer_producer


program test_distributed_circular_buffer
  use ISO_C_BINDING
  use ioserver_mpi
  use distributed_circular_buffer_module
  use parameters
  implicit none

  interface
  subroutine init_array(array, rank)
    implicit none
    integer, dimension(:), intent(inout) :: array
    integer, intent(in) :: rank
  end subroutine init_array
  function test_dcb_channel(buffer) result(num_errors)
    use distributed_circular_buffer_module, only : distributed_circular_buffer
    implicit none
    class(distributed_circular_buffer), intent(inout) :: buffer
    integer :: num_errors
  end function test_dcb_channel
  function test_dcb_consumer_producer(buffer, rank) result(num_errors)
    use distributed_circular_buffer_module, only : distributed_circular_buffer
    implicit none
    class(distributed_circular_buffer), intent(inout) :: buffer
  integer, intent(in) :: rank
    integer :: num_errors
  end function test_dcb_consumer_producer
  end interface

  integer :: rank, comm_size
  logical :: success
  integer :: num_errors, tmp_errors
  integer :: server_comm, dcb_comm
  integer :: server_group, dcb_group
  integer, dimension(3, 1) :: incl_range
  integer(C_INT) :: communication_type

  type(distributed_circular_buffer) :: circ_buffer
  integer :: server_bound_server_id, channel_id, server_bound_client_id

  integer :: ierr

  ! Initialization

  num_errors = 0
  server_bound_server_id = -1

  call MPI_init(ierr)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, ierr)

  if (comm_size < 3) then
    print *, 'Need at least 3 processes for this test!'
    num_errors = num_errors + 1
    goto 777
  end if

  if (rank < MAX_NUM_PROCS) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, rank, dcb_comm, ierr)
  else
    call MPI_Comm_split(MPI_COMM_WORLD, 1, rank, dcb_comm, ierr)
    goto 777 ! Goto end of program
  end if

  call MPI_comm_rank(dcb_comm, rank, ierr)
  call MPI_comm_size(dcb_comm, comm_size, ierr)

  server_comm = MPI_COMM_NULL
  call MPI_Comm_group(dcb_comm, dcb_group, ierr)

  if (rank < NUM_CONSUMERS * 2) then
    incl_range(1, 1) = 0
    incl_range(2, 1) = 2 * NUM_CONSUMERS - 1;
    incl_range(3, 1) = 1
    call MPI_Group_range_incl(dcb_group, 1, incl_range, server_group, ierr)
    call MPI_Comm_create_group(dcb_comm, server_group, 0, server_comm, ierr)

    if (rank < NUM_CONSUMERS) then
      communication_type = DCB_SERVER_BOUND_TYPE
    else
      communication_type = DCB_CHANNEL_TYPE
    end if
  else
    communication_type = DCB_SERVER_BOUND_TYPE
  end if

  ! print *, 'Everyone here is on dcb_comm', rank

  ! Beginning of test
  success = circ_buffer % create_bytes(dcb_comm, server_comm, communication_type, BUFFER_SIZE_BYTE, 0_8)

  if (.not. success) then
    print *, 'Could not create a circular buffer!', rank
    num_errors = num_errors + 1
    error stop 1
    ! goto 777
  end if

  server_bound_server_id = circ_buffer % get_server_bound_server_id()
  channel_id             = circ_buffer % get_channel_id()
  server_bound_client_id = circ_buffer % get_server_bound_client_id()

!  print *, 'rank, prod, channel, consume ', rank, producer_id, channel_id, server_bound_server_id

  if (.not. circ_buffer % is_valid()) then
    print *, 'Something wrong with the newly-created buffer!!!'
    num_errors = num_errors + 1
    error stop 1
    ! goto 777
  end if

  !---------------------------------------
  call MPI_Barrier(dcb_comm, ierr)
  !---------------------------------------

  if (server_bound_server_id >= 0) then
    num_errors = test_dcb_consumer_producer(circ_buffer, rank)
  else if (server_bound_client_id >= 0) then
    num_errors = test_dcb_consumer_producer(circ_buffer, rank)
  else if (channel_id >= 0) then
    num_errors = test_dcb_channel(circ_buffer)
  else
    num_errors = 1
  end if

!  call circ_buffer % print()
  call circ_buffer % delete()
  if (server_bound_server_id == 0) then
    call MPI_Group_free(server_group, ierr)
    call MPI_Comm_free(server_comm, ierr)
  end if

777 CONTINUE

  tmp_errors = num_errors
  call MPI_Reduce(tmp_errors, num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (server_bound_server_id == 0) then  ! check that we got back what we sent
    if(num_errors > 0) then
      print *, 'ERRORS IN DISTRIBUTED BUFFER TEST ', num_errors
    else
      print *, 'Distributed circular buffer test has succeeded'
    end if
  endif

  call MPI_finalize(ierr)

  if (num_errors > 0) error stop 1

end program test_distributed_circular_buffer

subroutine init_array(array, rank)
  implicit none

  integer, dimension(:), intent(inout) :: array
  integer, intent(in) :: rank

  integer :: i
  do i = 1, size(array)
    array(i) = (rank + 1) * 10000 + i
  end do
end subroutine init_array
