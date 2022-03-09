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
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

module circular_buffer_concurrent_module
  use ISO_C_BINDING
  use ioserver_mpi
  use circular_buffer_module
  use shared_mem_alloc_module
  implicit none

  integer(MPI_ADDRESS_KIND), parameter :: CB_SIZE_BYTES = 1000
  integer, parameter :: NUM_ENTRIES = 100

end module circular_buffer_concurrent_module

program circular_buffer_concurrent
  use circular_buffer_concurrent_module
  implicit none

  integer :: rank, size, ierr
  integer :: i_entry, i_pe
  type(circular_buffer) :: cb
  type(C_PTR) :: cb_memory
  logical :: success
  integer :: val, sum, expected_sum

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

  if (size < 3) then
    print *, 'ERROR. This test must use at the very least 3 processes'
  end if

  cb_memory = RPN_allocate_shared(CB_SIZE_BYTES, MPI_COMM_WORLD)

  if (rank == 0) then
    success = cb % create_bytes(cb_memory, CB_SIZE_BYTES)
    if (.not. success) then
      print *, 'ERROR: Unable to create CB from root'
      error stop 1
    end if
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (rank .ne. 0) then
    success = cb % create_bytes(cb_memory)
    if (.not. success) then
      print *, 'ERROR: Unable to create CB from non-root PE'
      error stop 1
    end if
  end if

  if (rank .ne. 0) then
    do i_entry = 1, NUM_ENTRIES
      success = cb % put(rank, 1_8, CB_KIND_INTEGER_4, .true., thread_safe = .true.)
      if (.not. success) then
        print *, 'ERROR putting data in CB from rank ', rank
        error stop 1
      end if
    end do

    !--------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !--------------------------------------

    success = cb % put(rank, 1_8, CB_KIND_INTEGER_4, .false., thread_safe = .true.)
    if (success) then
      print *, 'ERROR: That operation was supposed to fail...', rank
      error stop 1
    end if

  else
    expected_sum = 0
    do i_pe = 1, size - 1
      expected_sum = expected_sum + i_pe
    end do
    expected_sum = expected_sum * NUM_ENTRIES

    sum = 0
    do i_entry = 1, NUM_ENTRIES
      do i_pe = 1, size - 1
        success = cb % get(val, 1_8, CB_KIND_INTEGER_4, .true.)
        if (.not. success) then
          print *, 'ERROR retrieving data from CB'
          error stop 1
        end if
        sum = sum + val
      end do
    end do

    !--------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !--------------------------------------

    if (sum .ne. expected_sum) then
      print *,'ERROR: Got the wrong sum at the end', sum
      print *, 'expected ', expected_sum
      error stop 1
    end if

    print *, 'All tests successful'

  end if
  

  call MPI_Finalize(ierr)
end program circular_buffer_concurrent

