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
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 10000
  integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: ROOT = 1

  include 'remote_circular_buffer.inc'
  include 'mpif.h'

  integer     :: error, i
  integer     :: rank, comm_size
  type(C_PTR) :: circ_buffer
  integer     :: available

  integer, dimension(NUM_DATA_ELEMENTS) :: in_data, out_data

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)


  circ_buffer = remote_circular_buffer_create(MPI_COMM_WORLD, ROOT, rank, comm_size, NUM_BUFFER_ELEMENTS)

  if (.not. c_associated(circ_buffer)) then
    print *, 'Could not create a circular buffer!', rank
    goto 777
  end if

  if (rank /= ROOT) then
    do i = 1, NUM_DATA_ELEMENTS
      in_data(i) = rank * 1000 + i;
    end do

    available = remote_circular_buffer_put(circ_buffer, in_data, NUM_DATA_ELEMENTS)
  else

  end if

  call MPI_Barrier(MPI_COMM_WORLD, error)
!  call buffer_write_test(circ_buffer)

  call remote_circular_buffer_print(circ_buffer)
  call remote_circular_buffer_delete(circ_buffer)

777 CONTINUE

  call MPI_finalize(error)

end program test_remote_circular_buffer
