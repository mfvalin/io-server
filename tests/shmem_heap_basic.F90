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
  use rpn_extra_module
  implicit none

  integer(C_INT64_T), parameter :: KILO_BYTE = 1024
  integer(C_INT64_T), parameter :: MEGA_BYTE = 1024 * KILO_BYTE
  integer(C_INT64_T), parameter :: GIGA_BYTE = 1024 * MEGA_BYTE

  integer(C_INT64_T), parameter :: SHMEM_HEAP_SIZE_BYTE = 10 * GIGA_BYTE

  integer(C_INT64_T), parameter :: SMALL_BLOCK_SIZE = 16
contains

  function get_val(rank, index) result(val)
    implicit none
    integer, intent(in) :: rank
    integer, intent(in) :: index

    integer(kind=1) :: val
    val = int(rank * 10 + index, kind = 1)
  end function get_val

  subroutine run_test(the_heap)
    implicit none
    type(heap), intent(inout) :: the_heap

    integer :: rank, num_procs

    call MPI_Comm_rank(MPI_COMM_WORLD, rank)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs)

    call concurrent_alloc_basic()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD)
    !------------------------

    call one_producer_one_consumer()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD)
    !------------------------

    call array_tkr_and_values()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD)
    !------------------------

    call big_alloc()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD)
    !------------------------

    !!!!!!!!!!!!!!!!!!!!!!!
    ! Gotta do this test last, because it does bad thing to the heap (and might leave it in an inconsistent state)
    call concurrent_alloc_many()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD)
    !------------------------

  contains

    subroutine concurrent_alloc_basic()
      implicit none
      type(block_meta_f08) :: array_info
      logical :: success
      integer(kind=4), dimension(:), pointer :: array_i4_1

      array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
      if (.not. associated(array_i4_1)) then
        print '(A, I2, A, I6)','ERROR: Unable to allocate a 1D 32-bit integer array! Rank ', rank, ', offset ', array_info % get_offset()
        error stop 1
      end if

      success = the_heap % free(array_info)
      if (.not. success) then
        print *, 'ERROR Could not free heap block'
        error stop 1
      end if
    end subroutine concurrent_alloc_basic

    !> Test allocation from multiple processes concurrently, many times
    !> Also test different free methods
    !> Test allocation when *not* using the safe method (should generate errors)
    subroutine concurrent_alloc_many()
      implicit none
      integer :: i
      logical :: success
      integer :: num_errors, total_errors

      type(block_meta_f08) :: array_info
      integer(kind=4), dimension(:), pointer :: array_i4_1

      do i = 1, 100
        ! Test freeing from block address
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
        success = the_heap % free(array_info % get_ptr()) ! Free from block address
        if (.not. success) then
          print *, 'ERROR: Could not free from block address'
          error stop 1
        end if

        ! Test freeing from block offset
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
        success = the_heap % free(array_info % get_offset()) ! Free from block address
        if (.not. success) then
          print *, 'ERROR: Could not free from block offset', array_info % get_offset()
          error stop 1
        end if
      end do

      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !------------------------

      num_errors = 0
      total_errors = 0
      do i = 1, 1000
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .false.)
        success = the_heap % free(array_info)
        if (.not. success) num_errors = num_errors + 1 ! Concurrency errors are usually seen when freeing the block
      end do

      call MPI_Reduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD)
      if (rank == 0) then
        if (total_errors == 0 .or. .not. the_heap % check()) then
          print *, 'There are 0 errors. This is suspect. Are you sure you are using many concurrent processes for this test?'
          error stop 1
        end if
      end if

    end subroutine concurrent_alloc_many

    !> One process allocates many times, sends the offsets, and another process frees everything
    !> Repeat many times, so that allocates and frees are done concurrently
    subroutine one_producer_one_consumer()
      implicit none
      integer,            parameter :: NUM_LOOPS = 1000 ! How many times to run the test. Seems to take a few hundred times to reach some edge cases
      integer,            parameter :: NUM_ALLOC = 200
      integer(C_INT64_T), parameter :: ALLOC_BASE_SIZE = NUM_ALLOC * 3
      integer,            parameter :: ALLOCATOR_RANK = 0
      integer,            parameter :: FREEER_RANK  = 1

      integer(HEAP_ELEMENT), dimension(NUM_ALLOC) :: offsets
      real(kind=8), dimension(:), pointer :: array, array2
      type(MPI_Status) :: status
      integer :: i, j
      logical :: success

      type(block_meta_f08) :: array_info, array_info_2

      if (rank == ALLOCATOR_RANK) then

        ! Alloc once and check that the other process was able to free it
        ! This shoudl also check coalescing small blocks into a bigger one
        array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE], .false.) ! Allocate *not* safely
        offsets(1) = array_info % get_offset()
        if (.not. the_heap % is_valid_block(array_info % get_ptr()) .or. .not. associated(array)) then
          print *, 'ERROR: Allocated block is not valid!'
          error stop 1
        end if

        array_info_2 = the_heap % allocate(array2, [ALLOC_BASE_SIZE], .false.)

        call MPI_Send(offsets(1), 1, MPI_INTEGER8, FREEER_RANK, 0, MPI_COMM_WORLD)
        call MPI_Recv(offsets(1), 1, MPI_INTEGER8, FREEER_RANK, 1, MPI_COMM_WORLD, status) ! Wait for reply to make sure the freeing is done
        
        success = the_heap % free(array_info)
        if (success) then
          print *, 'ERROR: Block should have been free already'
          error stop 1
        end if

        array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE*2], .false.)
        if (.not. associated(array)) then
          print *, 'ERROR: Could not allocate block when a smaller free block is available before an occupied one'
          error stop 1
        end if

        success = the_heap % free(array_info) .and. the_heap % free(array_info_2)
        if (.not. success) then
          print *, 'ERRROR: Unable to free 2 blocks'
          error stop 1
        end if

        do i = 1, NUM_LOOPS
          do j = 1, NUM_ALLOC
            array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE - 2*j], .false.)
            if (.not. associated(array)) then
              print '(A,I4)', 'ERROR: Not allocated (single allocator), i = ', i
              error stop 1
            end if
            offsets(j) = array_info % get_offset()
          end do
          call MPI_Send(offsets, NUM_ALLOC, MPI_INTEGER8, FREEER_RANK, 0, MPI_COMM_WORLD)
        end do

      else if (rank == FREEER_RANK) then
        ! Free the block allocated by another process
        call MPI_Recv(offsets(1), 1, MPI_INTEGER8, ALLOCATOR_RANK, 0, MPI_COMM_WORLD, status)
        success = the_heap % free(offsets(1))
        if (.not. success) then
          print *, 'ERROR: Unable to free... Offset ', offsets(1)
          error stop 1
        end if
        call MPI_Send(offsets(1), 1, MPI_INTEGER8, ALLOCATOR_RANK, 1, MPI_COMM_WORLD)

        do i = 1, NUM_LOOPS
          call MPI_Recv(offsets, NUM_ALLOC, MPI_INTEGER8, ALLOCATOR_RANK, 0, MPI_COMM_WORLD, status)
          do j = 1, NUM_ALLOC
            success = the_heap % free(offsets(j))
          end do
        end do
      end if
    end subroutine one_producer_one_consumer

    subroutine array_tkr_and_values()
      implicit none

      integer, parameter :: PRODUCER_RANK = 1
      integer, parameter :: CONSUMER_RANK = 0
      integer, parameter :: NUM_ARRAYS = 6

      integer(HEAP_ELEMENT), dimension(NUM_ARRAYS) :: offsets
      type(block_meta_f08),  dimension(NUM_ARRAYS) :: array_infos
      integer(kind=1), dimension(:, :, :, :, :), pointer :: array_i1_5
      integer(kind=2), dimension(:, :, :, :),    pointer :: array_i2_4
      integer(kind=4), dimension(:, :, :),       pointer :: array_i4_3
      integer(kind=8), dimension(:, :),          pointer :: array_i8_2
      real(kind=4),    dimension(:, :),          pointer :: array_r4_2
      real(kind=8),    dimension(:, :, :),       pointer :: array_r8_3

      type(MPI_Status) :: status
      integer :: i, j, k, l, m
      integer :: index
      logical :: success

      if (rank == PRODUCER_RANK) then
        array_infos(1) = the_heap % allocate(array_i1_5, [2_8, 2_8, 2_8, 2_8, 2_8])
        array_infos(2) = the_heap % allocate(array_i2_4, [3_8, 3_8, 3_8, 3_8])
        array_infos(3) = the_heap % allocate(array_i4_3, [4_8, 4_8, 4_8])
        array_infos(4) = the_heap % allocate(array_i8_2, [5_8, 5_8])
        array_infos(5) = the_heap % allocate(array_r4_2, [60_8, 60_8])
        array_infos(6) = the_heap % allocate(array_r8_3, [70_8, 70_8, 70_8])

        do i = 1, 6
          offsets(i) = array_infos(i) % get_offset()
        end do

        do m = 1, 2
          do l = 1, 2
            do k = 1, 2
              do j = 1, 2
                do i = 1, 2
                  index = ((((m-1) * 2 + (l-1)) * 2 + (k-1)) * 2 + (j-1)) * 2 + i
                  ! print *, 'index ', index
                  array_i1_5(i, j, k, l, m) = int(get_val(rank, index), kind=1)
                end do
              end do
            end do
          end do
        end do

        do l = 1, 3
          do k = 1, 3
            do j = 1, 3
              do i = 1, 3
                index = (((l-1)*3 + (k-1)) * 3 + (j-1)) * 3 + i
                array_i2_4(i, j, k, l) = int(get_val(rank, index), kind=2)
                ! print *, 'index = ', index, array_i2_4(i, j, k, l)
              end do
            end do
          end do
        end do

        do k = 1, 4
          do j = 1, 4
            do i = 1, 4
              index = ((k-1)*4 + (j-1)) * 4 + i
              array_i4_3(i, j, k) = int(get_val(rank, index), kind=4)
            end do
          end do
        end do

        do j = 1, 5
          do i = 1, 5
            index = (j-1) * 5 + i
            array_i8_2(i, j) = int(get_val(rank, index), kind=8)
          end do
        end do

        do j = 1, 60
          do i = 1, 60
            index = (j-1) * 60 + i
            array_r4_2(i, j) = real(get_val(rank, index), kind=4)
          end do
        end do

        do k = 1, 70
          do j = 1, 70
            do i = 1, 70
              index = ((k-1)*70 + (j-1)) * 70 + i
              array_r8_3(i, j, k) = real(get_val(rank, index), kind=8)
            end do
          end do
        end do

        call MPI_Send(offsets, 6, MPI_INTEGER8, CONSUMER_RANK, 0, MPI_COMM_WORLD)
      else if (rank == CONSUMER_RANK) then

        call MPI_Recv(offsets, 6, MPI_INTEGER8, PRODUCER_RANK, 0, MPI_COMM_WORLD, status)

        call c_f_pointer(the_heap % get_address_from_offset(offsets(1)), array_i1_5, [2_8, 2_8, 2_8, 2_8, 2_8])
        call c_f_pointer(the_heap % get_address_from_offset(offsets(2)), array_i2_4, [3_8, 3_8, 3_8, 3_8])
        call c_f_pointer(the_heap % get_address_from_offset(offsets(3)), array_i4_3, [4_8, 4_8, 4_8])
        call c_f_pointer(the_heap % get_address_from_offset(offsets(4)), array_i8_2, [5_8, 5_8])
        call c_f_pointer(the_heap % get_address_from_offset(offsets(5)), array_r4_2, [60_8, 60_8])
        call c_f_pointer(the_heap % get_address_from_offset(offsets(6)), array_r8_3, [70_8, 70_8, 70_8])

        do m = 1, 2
          do l = 1, 2
            do k = 1, 2
              do j = 1, 2
                do i = 1, 2
                  index = ((((m-1) * 2 + (l-1)) * 2 + (k-1)) * 2 + (j-1)) * 2 + i
                  if (array_i1_5(i, j, k, l, m) .ne. int(get_val(PRODUCER_RANK, index), kind=1)) then
                    print *, 'ERROR: Wrong value i1', i, j, k, l, m
                    error stop 1
                  end if
                end do
              end do
            end do
          end do
        end do

        do l = 1, 3
          do k = 1, 3
            do j = 1, 3
              do i = 1, 3
                index = (((l-1)*3 + (k-1)) * 3 + (j-1)) * 3 + i
                if (array_i2_4(i, j, k, l) .ne. int(get_val(PRODUCER_RANK, index), kind=2)) then
                  print *, 'ERROR: Wrong value i2', i, j, k, l
                  error stop 1
                end if
              end do
            end do
          end do
        end do

        do k = 1, 4
          do j = 1, 4
            do i = 1, 4
              index = ((k-1)*4 + (j-1)) * 4 + i
              if (array_i4_3(i, j, k) .ne. int(get_val(PRODUCER_RANK, index), kind=4)) then
                print *, 'ERROR: Wrong value i4', i, j, k
                error stop 1
              end if
            end do
          end do
        end do

        do j = 1, 5
          do i = 1, 5
            index = (j-1) * 5 + i
            if (array_i8_2(i, j) .ne. int(get_val(PRODUCER_RANK, index), kind=8)) then
              print *, 'ERROR: Wrong value i8', i, j
              error stop 1
            end if
          end do
        end do

        do j = 1, 60
          do i = 1, 60
            index = (j-1) * 60 + i
            if (abs(array_r4_2(i, j) - real(get_val(PRODUCER_RANK, index), kind=4)) > 0.0) then
              print *, 'ERROR: Wrong value r4', i, j
              error stop 1
            end if
          end do
        end do

        do k = 1, 70
          do j = 1, 70
            do i = 1, 70
              index = ((k-1)*70 + (j-1)) * 70 + i
              if (abs(array_r8_3(i, j, k) - real(get_val(PRODUCER_RANK, index), kind=8)) > 0.0_8) then
                print *, 'ERROR: Wrong value r8', i, j, k
                error stop 1
              end if
            end do
          end do
        end do
      end if

      do i = 1, 6
        success = the_heap % free(offsets(i))
      end do
    end subroutine array_tkr_and_values

    subroutine big_alloc()
      implicit none
      type(block_meta_f08) :: array_info1, array_info2
      integer(kind=8), dimension(:), pointer :: kinda_big_array1, kinda_big_array2, big_array
      integer(kind=8) :: kinda_big_alloc_size, big_alloc_size
      integer :: num_errors, total_errors
      logical :: success

      kinda_big_alloc_size = SHMEM_HEAP_SIZE_BYTE / int(num_procs * 8 * 2, 8) + 1
      big_alloc_size = the_heap % get_size() / 8 - 100

      array_info1 = the_heap % allocate(kinda_big_array1, [kinda_big_alloc_size], .true.)
      array_info2 = the_heap % allocate(kinda_big_array2, [kinda_big_alloc_size], .true.)

      num_errors = 0
      total_errors = 0
      if (.not. associated(kinda_big_array1)) num_errors = num_errors + 1
      if (.not. associated(kinda_big_array2)) num_errors = num_errors + 1

      call MPI_Reduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD)
      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !------------------------
      if (rank == 0) then
        if (total_errors .ne. 1) then
          print *, 'ERROR: There should be exactly 1 array that could not be allocated', total_errors
          error stop 1
        end if
      end if
      
      success = the_heap % free(array_info1) .and. the_heap % free(array_info2)

      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !------------------------

      if (rank == 0) then
        array_info1 = the_heap % allocate(big_array, [big_alloc_size])
        if (.not. associated(big_array)) then
          print *, 'ERROR: Unable to allocate array of size ', big_alloc_size
          error stop 1
        end if

        success = the_heap % free(array_info1)
        if (.not. success) then
          print *, 'ERROR: Unable to free the huge array of size ', big_alloc_size
          error stop 1
        end if

        array_info1 = the_heap % allocate(big_array, [big_alloc_size + 100])
        if (associated(big_array)) then
          print *, 'ERROR: Should not have been able to allocate that much data!'
          error stop 1
        end if
      end if

    end subroutine big_alloc

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

  type(heap) :: the_heap
  logical    :: success

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
    success = the_heap % create(shared_mem, SHMEM_HEAP_SIZE_BYTE)
  else
    success = the_heap % clone(shared_mem)
  end if

  if (.not. success) then
    print *, 'ERROR: Heap creation failed'
    error stop 1
  end if

  ! status = the_heap % set_default()
  ! call the_heap % set_base(shared_mem)

  !-------------------------------
  call MPI_Barrier(MPI_COMM_WORLD)
  !-------------------------------
  ! if (rank .ne. 0) then
  !   status  = the_heap % register(shared_mem)
  !   if (status .ne. 1) then
  !     print *, 'ERROR: Heap registration should give 1 registered heap. Got ', status
  !     error stop 1
  !   end if
  ! end if

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

  if (rank == 0) print *, 'All good'

end program shmem_heap_basic
