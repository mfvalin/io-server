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
!> \author  V. Magnoux, Recherche en Prevision Numerique
!> \date    2020-2022

module atomic_test_module
  use ioserver_mpi
  use iso_c_binding

  use atomic_module
  use shared_mem_alloc_module

  integer(MPI_ADDRESS_KIND), parameter :: NUM_SHARED_MEM_BYTES = 8
  integer, parameter :: NUM_ITERATIONS = 1000

contains

subroutine test_atomic_int32_try_update(atomic_var, rank, size)
  implicit none
  type(atomic_int32), intent(inout) :: atomic_var
  integer, intent(in) :: rank, size

  integer :: ierr, i
  integer(C_INT32_T) :: old_val, local_sum, global_sum
  logical :: success

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  if (rank == 0) then
    old_val = atomic_var % read()
    success = atomic_var % try_update(old_val, 0)
    if (.not. success) then
      print *, 'ERROR: Failed'
      error stop 1
    end if
  end if

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  if (atomic_var % read() .ne. 0) then
    print *, 'ERROR: Failed'
    error stop 1
  end if

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  ! Add, only trying once
  local_sum = 0
  do i = 1, NUM_ITERATIONS
    old_val = atomic_var % read()
    if (atomic_var % try_update(old_val, old_val + 1)) local_sum = local_sum + 1
  end do

  call MPI_Reduce(local_sum, global_sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (rank == 0) then
    if (atomic_var % read() .ne. global_sum) then
      print *, 'ERROR: Got the wrong result', global_sum, atomic_var % read()
      error stop 1
    end if

    success = atomic_var % try_update(global_sum, 0)
  end if

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  ! Add, until we succeed
  do i = 1, NUM_ITERATIONS
    success = .false.
    do while (.not. success)
      old_val = atomic_var % read()
      success = atomic_var % try_update(old_val, old_val + 1)
    end do
  end do

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  if (atomic_var % read() .ne. NUM_ITERATIONS * size) then
    print *, 'ERROR: Wrong final sum'
    error stop 1
  end if


end subroutine test_atomic_int32_try_update

end module atomic_test_module

program atomic_test
  use atomic_test_module
  implicit none

  integer :: rank, size
  integer :: ierr, i

  type(C_PTR) :: cb_memory
  integer, pointer :: int_ptr
  type(atomic_int32) :: atomic_int

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
  
  if (size < 12) then
    print *, 'ERROR: We want at least 12 processes for this test'
    error stop 1
  end if

  cb_memory = RPN_allocate_shared(NUM_SHARED_MEM_BYTES, MPI_COMM_WORLD)

  call c_f_pointer(cb_memory, int_ptr)
  if (rank == 0) int_ptr = 0

  call atomic_int % init_from_int(int_ptr)

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  ! Test without atomic first
  do i = 1, NUM_ITERATIONS
    int_ptr = int_ptr + 1
  end do

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  if (int_ptr .ne. NUM_ITERATIONS * size) then
    print *, 'INFO: Result is wrong (as expected)!', int_ptr, NUM_ITERATIONS * size
  end if

  !-------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------------

  call test_atomic_int32_try_update(atomic_int, rank, size)

  call MPI_Finalize(ierr)

end program atomic_test
