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
program test_circular_buffer

  use ISO_C_BINDING
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 10000
  integer, parameter :: NUM_DATA_ELEMENTS = 10

  include 'memory_arena.inc'
!  include 'circular_buffer.inc'
  include 'mpif.h'

  integer     :: error, i
  integer     :: rank, comm_size
  integer     :: available
  type(C_PTR) :: do_not_use

  type(circular_buffer) :: circ_buffer
  integer(DATA_ELEMENT) :: in_data(NUM_DATA_ELEMENTS), out_data(NUM_DATA_ELEMENTS)

  call MPI_init(error)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, error)
  call MPI_comm_size(MPI_COMM_WORLD, comm_size, error)

  do_not_use = circ_buffer % create(NUM_BUFFER_ELEMENTS)

  if (.not. circ_buffer % is_valid()) then
    print *, 'Could not create a circular buffer!', rank
    goto 777
  endif

  out_data = -1
  do i = 1, NUM_DATA_ELEMENTS
    in_data(i) = i + 100 * rank
  enddo

  print *, 'in_data ', rank, ':', in_data

  available = circ_buffer % atomic_put(in_data, NUM_DATA_ELEMENTS)
  available = circ_buffer % atomic_get(out_data, NUM_DATA_ELEMENTS)

  print *, 'out_data', rank, ':', out_data


777 CONTINUE

  call MPI_finalize(error)

end program test_circular_buffer
