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

program test_simple_mutex
  use ISO_C_BINDING
  use ioserver_mpi
  use shared_mem_alloc_module
  use simple_mutex_module
  implicit none

  integer, parameter :: NUM_IT = 100000

  type(C_PTR) :: shared_mem, shmem_variables
  integer     :: num_pes, rank
  integer     :: i, i_rank

  type(simple_mutex), dimension(:), allocatable :: mutexes
  integer, dimension(:),   pointer :: mutex_values
  integer, dimension(:,:), pointer :: test_array

  integer :: num_errors
  integer :: ierr

  num_errors = 0

  ! ---------------------
  ! Initialize everything
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_pes, ierr)

  shared_mem      = RPN_allocate_shared(int(num_pes * 4, kind=8), MPI_COMM_WORLD)
  shmem_variables = RPN_allocate_shared(int(num_pes * 4 * 3, kind=8), MPI_COMM_WORLD)

  if (.not. C_ASSOCIATED(shared_mem) .or. .not. C_ASSOCIATED(shmem_variables)) then
    print *, 'ERROR: Could not allocated shared memory for the mutexes'
    error stop 1
  end if

  call C_F_POINTER(shared_mem, mutex_values, [num_pes])
  call C_F_POINTER(shmem_variables, test_array, [num_pes, 3])
  allocate(mutexes(num_pes))

  do i = 1, num_pes
    call mutexes(i) % init_from_int(mutex_values(i), rank)
  end do

  if (rank == 0) test_array(:,:) = 0

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! -------------------------
  ! Now we can start the test

  ! Test basic lock function
  do i = 1, NUM_IT
    do i_rank = 1, num_pes
      call mutexes(i_rank) % lock()
      if (.not. mutexes(i_rank) % is_locked_by_me()) num_errors = num_errors + 1
      if (.not. mutexes(i_rank) % is_locked())       num_errors = num_errors + 1
      test_array(i_rank, 1) = test_array(i_rank, 1) + 1
      call mutexes(i_rank) % unlock()
    end do
  end do

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! Test try_lock function
  do i = 1, NUM_IT
    do i_rank = 1, num_pes
      if (mutexes(i_rank) % try_lock()) then
        test_array(i_rank, 2) = test_array(i_rank, 2) + 1
      else
        call mutexes(i_rank) % lock()
        test_array(i_rank, 3) = test_array(i_rank, 3) + 1
      end if

      if (.not. mutexes(i_rank) % is_locked_by_me()) num_errors = num_errors + 1
      call mutexes(i_rank) % unlock()
    end do
  end do

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if (test_array(rank + 1, 1) .ne. NUM_IT * num_pes) then
    print *, 'ERROR: Wrong final value for incremented value. Correct vs actual: ', NUM_IT * num_pes, test_array(rank + 1, 1)
    error stop 1
  end if

  if (test_array(rank + 1, 2) + test_array(rank + 1, 3) .ne. NUM_IT * num_pes) then
    print *, 'ERROR: Wrong final value for try_lock test.', test_array(rank + 1, 2), test_array(rank + 1, 3)
    error stop 1
  end if

  if (num_errors > 0) then
    print *, 'ERROR: num_errors is non-zero', num_errors
    error stop 1
  end if

  call MPI_Finalize(ierr)

  if (rank == 0) then
    print *, 'Simple mutex test successful'
    print *, test_array(:, 2)
    print *, test_array(:, 3)
  end if
end program test_simple_mutex