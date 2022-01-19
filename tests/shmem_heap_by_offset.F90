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

program shmem_heap_by_offset
  use iso_c_binding
  use mpi_f08
  use heap_module
  use shared_mem_alloc_module
  implicit none

  integer :: num_procs, rank

  integer(C_SIZE_T),  parameter :: SHMEM_HEAP_SIZE_BYTE = 5000
  integer(C_INT64_T), parameter :: BLOCK_SIZE_INT_X = 100
  integer(C_INT64_T), parameter :: BLOCK_SIZE_INT_Y = 2

  type(C_PTR) :: shared_mem
  type(C_PTR) :: tmp_ptr

  type(heap) :: the_heap
  integer    :: status

  integer, dimension(:,:), pointer :: array
  type(block_meta_f08)             :: array_info
  type(C_PTR) :: p, p2
  ! integer(C_INT), dimension(MAX_ARRAY_RANK) :: dim
  ! integer(C_INT) :: tkr
  integer(HEAP_ELEMENT) :: offset

  !!!!!!!!!!!!!!!!!!!!!!
  ! Initialization
  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  if (num_procs .ne. 2) then
    print *, 'ERROR: Need exactly 2 processes for this test'
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
    status  = the_heap % register(shared_mem)
  end if

  status = the_heap % set_default()
  call the_heap % set_base(shared_mem)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! The test

  if (rank == 0) then
    array_info = the_heap % allocate(array, [BLOCK_SIZE_INT_X, BLOCK_SIZE_INT_Y])

    if (.not. associated(array)) then
      print *,'ERROR: Could not allocate array from shared mem heap!'
      error stop 1
    end if

    ! call block_meta_internals(array_info, p, dim, tkr, offset)        ! grep block_meta private contents
    p = array_info % get_ptr()
    offset = array_info % get_offset()

    if (.not. c_associated(p)) then
      print *,'ERROR: Corresponding C pointer is not associated!'
      error stop 1
    end if

    if (.not. the_heap % is_valid_block(p)) then
      print *, 'ERROR: C pointer does not point to a valid block!'
      error stop 1
    end if

    p2 = the_heap % get_address_from_offset(offset)
    if (.not. c_associated(p, p2)) then
      ! print *, 'ERROR: Retrieving block address from its offset gives the wrong address!', p2
      ! print *, 'Should be                                                              ', p
      error stop 1
    end if

  end if


  call MPI_Finalize()
end program shmem_heap_by_offset