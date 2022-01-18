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

module shmem_heap_basic_module
  use iso_c_binding
  use mpi_f08
  use heap_module
  implicit none

  integer(C_INT64_T), parameter :: KILO_BYTE = 1024
  integer(C_INT64_T), parameter :: MEGA_BYTE = 1024 * KILO_BYTE
  integer(C_INT64_T), parameter :: GIGA_BYTE = 1024 * MEGA_BYTE

  integer(C_INT64_T), parameter :: SHMEM_HEAP_SIZE_BYTE = 10 * GIGA_BYTE
contains

  subroutine run_test(the_heap)
    implicit none
    type(heap), intent(inout) :: the_heap

    integer :: rank, num_procs
    type(block_meta) :: array_info
    integer(kind=4), dimension(:), pointer :: array_i4_1

    type(C_PTR)         :: heap_address, block_address, heap_from_block_address
    integer(C_SIZE_T)   :: offset
    integer(C_INTPTR_T) :: dummy_intptr

    call MPI_Comm_rank(MPI_COMM_WORLD, rank)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs)

    array_info = the_heap % allocate(array_i4_1, [16], .true.) ! Allocate 'safely'
    offset = get_block_meta_offset(array_info)
    print *, 'Offset = ', offset

    if (.not. associated(array_i4_1)) then
      print '(A, I2, A, I6)','ERROR: Unable to allocate a 1D 32-bit integer array! Rank ', rank, ', offset ', offset
      error stop 1
    end if

    heap_address = the_heap % get_ptr()
    block_address = get_block_meta_address(array_info)
    heap_from_block_address = get_heap_from_address(block_address)

    if (.not. c_associated(heap_from_block_address, heap_address)) then
      print '(A, Z16, A, Z16, A, I2, A, I6)', 'ERROR: get_heap_from_address ', transfer(heap_from_block_address, dummy_intptr),' returns the wrong heap ptr ', &
            transfer(heap_address, dummy_intptr), '. Rank ', rank, ', offset ', offset
      print '(A, Z16)', 'Block address: ', block_address
      error stop 1
    end if

  end subroutine run_test

end module shmem_heap_basic_module

program shmem_heap_basic
  use shared_mem_alloc_module
  use shmem_heap_basic_module
  implicit none

  integer :: rank, num_procs
  type(MPI_Comm) :: node_comm
  integer :: node_size

  type(C_PTR) :: shared_mem
  type(C_PTR) :: tmp_ptr

  type(heap) :: the_heap
  integer    :: status

  !!!!!!!!!!!!!!!!!!!!!!
  ! Initialization
  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, node_comm)
  call MPI_Comm_size(node_comm, node_size)

  if (num_procs .lt. 4 .and. num_procs .ne. node_size) then
    print *, 'ERROR: Need at least 4 processes for this test, and can only run it on a single node'
    error stop 1
  end if

  shared_mem = RPN_allocate_shared(SHMEM_HEAP_SIZE_BYTE, MPI_COMM_WORLD);

  if (.not. c_associated(shared_mem)) then
    print *, 'ERROR: Could not allocate shared memory'
    error stop 1
  end if

  if (rank == 0) then
    tmp_ptr = the_heap % create(shared_mem, SHMEM_HEAP_SIZE_BYTE)
  else
    tmp_ptr = the_heap % clone(shared_mem)

  end if

  if (.not. c_associated(tmp_ptr)) then
    print *, 'ERROR: Heap creation failed'
    error stop 1
  end if

  status = the_heap % set_default()
  call the_heap % set_base(shared_mem)

  !-------------------------------
  call MPI_Barrier(MPI_COMM_WORLD)
  !-------------------------------
  if (rank .ne. 0) then
    status  = the_heap % register(shared_mem)
    if (status .ne. 1) then
      print *, 'ERROR: Heap registration should give 1 registered heap. Got ', status
      error stop 1
    end if
  end if

  if (.not. the_heap % check()) then
    print *, 'ERROR: Heap is not valid, before even starting the test!', rank
    error stop 1
  end if
  !-------------------------------
  call MPI_Barrier(MPI_COMM_WORLD)
  !-------------------------------

  !!!!!!!!!!!!!!!!
  ! Run the test
  call run_test(the_heap)

  !!!!!!!!!!!!!!
  ! We're done
  !-------------------------------
  call MPI_Barrier(MPI_COMM_WORLD)
  !-------------------------------
  call MPI_Finalize()

end program shmem_heap_basic
