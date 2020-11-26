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

program test_distributed_circular_buffer

  use ISO_C_BINDING
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 10000
  integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: NUM_CONSUMERS = 2

  include 'mpif.h'
  include 'distributed_circular_buffer.inc'

  integer :: error, i
  integer :: rank, comm_size
  integer :: available
  logical :: success

  type(distributed_circular_buffer)                 :: circ_buffer
  integer(CB_ELEMENT), dimension(NUM_DATA_ELEMENTS) :: in_data, out_data
  integer :: num_producers, consumer_id

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)


  num_producers = comm_size - NUM_CONSUMERS;
  consumer_id = rank - num_producers

  success = circ_buffer % create(MPI_COMM_WORLD, rank, comm_size, num_producers, NUM_BUFFER_ELEMENTS)

  if (.not. success) then
    print *, 'Could not create a circular buffer!', rank
    goto 777
  end if

  if (rank < num_producers) then

    call sleep_us(200)

    in_data(:) = -1
    out_data(:) = -2

    do i = 1, NUM_DATA_ELEMENTS
      in_data(i) = rank * 1000 + i
    end do

    available = circ_buffer % put(in_data, NUM_DATA_ELEMENTS)

    if (available < 0) then
      print *, 'Unable to insert stuff in the buffer!!!!!'
    else
      print *, 'There is now that many slots available in the buffer: ', available
    end if

  else
!    call sleep_us(20000)
    block
      integer :: first_prod, num_prod_local, last_prod
      integer :: i_prod

      num_prod_local = ceiling(real(num_producers) / NUM_CONSUMERS)
      first_prod = consumer_id * num_prod_local
      last_prod = min(first_prod + num_prod_local, num_producers) - 1

!      print *, 'Reading buffers from ', first_prod, last_prod
      do i_prod = first_prod, last_prod
        print *, 'Reading from producer ', i_prod
        available = circ_buffer % get(i_prod, out_data, NUM_DATA_ELEMENTS)
        print *, 'Read from producer: ', out_data
        print *, 'It now has: ', available
      end do

    end block

  end if

  call MPI_Barrier(MPI_COMM_WORLD, error)
!  call buffer_write_test(circ_buffer)

  call circ_buffer % print()
  call circ_buffer % delete()

777 CONTINUE

  call MPI_finalize(error)

end program test_distributed_circular_buffer
