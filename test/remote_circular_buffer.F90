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

program test_remote_circular_buffer

  use ISO_C_BINDING
  use remote_circular_buffer_module, only : remote_circular_buffer
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 10000
  integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: ROOT = 1

  include 'mpif.h'

  integer :: error, i
  integer :: rank, comm_size
  integer :: available
  logical :: success

  type(remote_circular_buffer)          :: circ_buffer
  integer, dimension(NUM_DATA_ELEMENTS) :: in_data, out_data

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)


  success = circ_buffer % create(MPI_COMM_WORLD, ROOT, rank, comm_size, NUM_BUFFER_ELEMENTS)

  if (.not. success) then
    print *, 'Could not create a circular buffer!', rank
    goto 777
  end if

  if (rank /= ROOT) then
    do i = 1, NUM_DATA_ELEMENTS
      in_data(i) = rank * 1000 + i;
    end do

    available = circ_buffer % put(in_data, NUM_DATA_ELEMENTS)
  else
    available = circ_buffer % get(0, out_data, NUM_DATA_ELEMENTS)
    print *, 'Read from 1st producer: ', out_data
    print *, 'It now has: ', available

    available = circ_buffer % get(1, out_data, NUM_DATA_ELEMENTS)
    print *, 'Read from 2nd producer: ', out_data
    print *, 'It now has: ', available
  end if

!  call MPI_Barrier(MPI_COMM_WORLD, error)
!  call buffer_write_test(circ_buffer)

  call circ_buffer % print()
  call circ_buffer % delete()

777 CONTINUE

  call MPI_finalize(error)

end program test_remote_circular_buffer
