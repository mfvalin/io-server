! Copyright (C) 2020  Environnement Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020
!     V. Magnoux, Recherche en Prevision Numerique, 2020

program test_distributed_circular_buffer

  use ISO_C_BINDING
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 200
  integer, parameter :: NUM_DATA_ELEMENTS_SMALL = 10
  integer, parameter :: NUM_DATA_ELEMENTS_LARGE = 500
  integer, parameter :: STEP_SIZE = 5
  integer, parameter :: NUM_CONSUMERS = 2

  include 'mpif.h'
  include 'io-server/common.inc'

  interface
  subroutine init_array(array, rank)
    use circular_buffer_module, only: DATA_ELEMENT
    implicit none
    integer(DATA_ELEMENT), dimension(:), intent(out) :: array
    integer, intent(in) :: rank
  end subroutine init_array
  end interface

  integer :: error, i
  integer :: rank, comm_size
  integer :: available
  logical :: success
  integer :: num_errors, tmp_errors

  type(distributed_circular_buffer)                   :: circ_buffer
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_SMALL) :: in_data_small, out_data_small, expected_data_small
  integer(DATA_ELEMENT), dimension(NUM_DATA_ELEMENTS_LARGE) :: in_data_large, out_data_large, expected_data_large
  integer :: num_producers, consumer_id, receiver_id, producer_id

  integer :: first_prod, num_prod_local, last_prod
  integer :: i_prod

  ! Initialization

  num_errors = 0

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)

  if (comm_size < 6) then
    print *, 'Need at least 6 processes for this test!'
    num_errors = num_errors + 1
    goto 777
  end if

  call init_array(in_data_small, rank)
  call init_array(in_data_large, rank)
  out_data_small(:) = -1
  out_data_large(:) = -1

  num_producers = comm_size - 2*NUM_CONSUMERS;

  ! Beginning of test

  success = circ_buffer % create(MPI_COMM_WORLD, num_producers, NUM_CONSUMERS, NUM_BUFFER_ELEMENTS)

  if (.not. success) then
    print *, 'Could not create a circular buffer!', rank
    num_errors = num_errors + 1
    goto 777
  end if

  consumer_id = circ_buffer % get_consumer_id()
  receiver_id = circ_buffer % get_receiver_id()
  producer_id = circ_buffer % get_producer_id()

  print *, 'rank, prod, receive, consume ', rank, producer_id, receiver_id, consumer_id

  num_prod_local = ceiling(real(num_producers) / NUM_CONSUMERS)
  first_prod = consumer_id * num_prod_local
  last_prod = min(first_prod + num_prod_local, num_producers) - 1


  if (.not. circ_buffer % check_integrity(.true.)) then
    print *, 'Something wrong with the newly-created buffer!!!'
    num_errors = num_errors + 1
    goto 777
  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (consumer_id >= 0) then
    do i = 0, num_producers - 1
      available = circ_buffer % get_num_elements(i)
!      print *, rank, i, 'Num elem = ', available
    end do
  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (producer_id >= 0) then

!    print *, 'Putting ', in_data_small
!    available = circ_buffer % put(in_data_small, NUM_DATA_ELEMENTS_SMALL)
    available = circ_buffer % put(in_data_small, NUM_DATA_ELEMENTS_SMALL)

    if (available < 0) then
      num_errors = num_errors + 1
      print *, 'Unable to insert stuff in the buffer!!!!!'
    end if
  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------


  if (consumer_id >= 0) then
    do i = 0, num_producers - 1
      available = circ_buffer % get_num_elements(i)
      if (available .ne. NUM_DATA_ELEMENTS_SMALL) then
        num_errors = num_errors + 1
        print *, 'Wrong number of elements in buffer !!!!!!!!', rank, i
  !      goto 777
      end if
    end do

  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (consumer_id >= 0) then
    do i_prod = first_prod, last_prod
      call init_array(expected_data_small, i_prod)

      available = circ_buffer % get(i_prod, out_data_small, NUM_DATA_ELEMENTS_SMALL)
!      print *, 'Got ', out_data_small

      if (available .ne. 0) then
        num_errors = num_errors + 1
      end if
      if (.not. all(out_data_small == expected_data_small)) then
        num_errors = num_errors + 1
      end if
!      print *, 'Read producer ', i_prod
    end do
  end if

!  goto 999

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (producer_id >= 0) then
    available = circ_buffer % get_num_elements()
    if (available .ne. 0) then
      num_errors = num_errors + 1
    end if
  end if


  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

  if (producer_id >= 0) then
!    call sleep_us(1000 * rank)
!    call circ_buffer % print()
    do i = 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE
!      print *, rank, 'Putting elements ', in_data_large(i:i + STEP_SIZE - 1)
!      available = circ_buffer % get_num_elements()
!      print *, rank, 'Elements before: ', available
      available = circ_buffer % put(in_data_large(i:i + STEP_SIZE - 1), STEP_SIZE)
!      print *, rank, 'Done putting elements'
!      available = circ_buffer % get_num_elements()
!      print *, rank, 'Elements after:  ', available
    end do

    !---------------------------------------
!    call MPI_Barrier(MPI_COMM_WORLD, error)
    !---------------------------------------

  else if (consumer_id >= 0) then
    !---------------------------------------
!    call MPI_Barrier(MPI_COMM_WORLD, error)
    !---------------------------------------

!    call sleep_us(100000 * rank)
!    call sleep_us(100000)
!    call circ_buffer % print()
    do i_prod = first_prod, last_prod
      call init_array(expected_data_large, i_prod)
      do i = 1, NUM_DATA_ELEMENTS_LARGE, STEP_SIZE

!        print *, rank, 'Getting elements ', expected_data_large(i : i + STEP_SIZE - 1), ' from ', i_prod
!        available = circ_buffer % get_num_elements(i_prod)
!        print *, 'Will read: ', available
!        available = circ_buffer % get(i_prod, out_data_large(i : i + STEP_SIZE - 1), available)
        available = circ_buffer % get(i_prod, out_data_large(i : i + STEP_SIZE - 1), STEP_SIZE)

        if (.not. all(out_data_large(i:i + STEP_SIZE - 1) == expected_data_large(i : i + STEP_SIZE - 1))) then
          num_errors = num_errors + 1
        end if

!        print *, rank, 'Done getting elements'
      end do
    end do
  end if

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, error)
  !---------------------------------------

999 CONTINUE

!  call circ_buffer % print()
  call circ_buffer % delete()

777 CONTINUE

  tmp_errors = num_errors
  call MPI_Reduce(tmp_errors, num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, error)

  if(rank == 0) then  ! check that we got back what we sent
    if(num_errors > 0) then
      print *, 'ERRORS IN DISTRIBUTED BUFFER TEST ', num_errors
    else
      print *, 'Distributed circular buffer test has succeeded'
    end if
  endif

  call MPI_finalize(error)

end program test_distributed_circular_buffer

subroutine init_array(array, rank)
  use circular_buffer_module, only: DATA_ELEMENT
  implicit none

  integer(DATA_ELEMENT), dimension(:), intent(out) :: array
  integer, intent(in) :: rank

  integer :: i
  do i = 1, size(array)
    array(i) = (rank + 1) * 10000 + i
  end do
end subroutine init_array
