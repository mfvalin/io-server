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
  use ioserver_mpi

  use shmem_heap_module
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

  function test_alloc(the_heap, array, expected, dim1, dim2) result(success)
    implicit none
    type(shmem_heap), intent(inout) :: the_heap
    integer(C_INT16_T), dimension(:, :, :), contiguous, pointer, intent(inout) :: array
    integer, intent(in) :: expected
    integer(C_INT64_T), dimension(:), intent(in) :: dim1
    integer(C_INT64_T), dimension(:), intent(in), optional :: dim2
    logical :: success

    type(block_meta) :: info
    integer, dimension(size(dim1)) :: default_dim2
    default_dim2(:) = 1

    if (present(dim2)) then
      default_dim2(:) = dim2(1:size(dim1))
      info = the_heap % allocate(array, dim1, dim2)
    else
      info = the_heap % allocate(array, dim1)
    end if

    success = .false.
    if (info % get_status() == expected) success = .true.
    if (associated(array) .and. expected .ne. SHMEM_HEAP_ALLOC_SUCCESS) success = .false.
    if (.not. success) then
      if (expected == SHMEM_HEAP_ALLOC_SUCCESS) then
        print *, 'ERROR: Unable to allocate: ', dim1, default_dim2
      else
        print *, 'ERROR: Should NOT have allocated: ', size(array), dim1, default_dim2
      endif
    end if

    if (associated(array)) then
      success = the_heap % free(info) .and. success
    end if

    if (.not. success) error stop 1
  end function test_alloc

  function test_alloc4(the_heap, array, expected, dim1, dim2) result(success)
    implicit none
    type(shmem_heap), intent(inout) :: the_heap
    integer(C_INT16_T), dimension(:, :, :), contiguous, pointer, intent(inout) :: array
    integer, intent(in) :: expected
    integer(C_INT32_T), dimension(:), intent(in) :: dim1
    integer(C_INT32_T), dimension(:), intent(in), optional :: dim2
    logical :: success

    integer(C_INT64_T), dimension(size(dim1)) :: dim1_8

    dim1_8(:) = dim1(:)

    if (present(dim2)) then
      block
        integer(C_INT64_T), dimension(:), allocatable :: dim2_8
        allocate(dim2_8(size(dim2)))
        dim2_8(:) = dim2(:)
        success = test_alloc(the_heap, array, expected, dim1_8, dim2_8)
        deallocate(dim2_8)
      end block
    else
      success = test_alloc(the_heap, array, expected, dim1_8)
    end if
  end function test_alloc4

  subroutine run_test(the_heap)
    implicit none
    type(shmem_heap), intent(inout) :: the_heap

    integer :: rank, num_procs
    integer :: ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

    if (rank == 0) print *, 'Concurrent alloc'
    call concurrent_alloc_basic()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    if (rank == 0) print *, 'One producer, one consumer'
    call one_producer_one_consumer()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    if (rank == 0) print *, 'Array TKR and values'
    call array_tkr_and_values()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    if (rank == 0) print *, 'Timeout test (please be patient)'
    call timeout_test()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    if (rank == 0) print *, 'Big alloc'
    call big_alloc()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    if (rank == 0) print *, 'Various inputs'
    call various_params()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

    !!!!!!!!!!!!!!!!!!!!!!!
    ! Gotta do this test last, because it does bad thing to the heap (and might leave it in an inconsistent state)
    if (rank == 0) print *, 'Concurrent alloc many'
    call concurrent_alloc_many()

    !------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !------------------------

  contains

    subroutine concurrent_alloc_basic()
      implicit none
      type(block_meta) :: array_info
      logical :: success
      integer(kind=4), dimension(:), contiguous, pointer :: array_i4_1

      array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
      if (.not. associated(array_i4_1) .or. array_info % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
        print '(A, I2, A, I6, L2, I3)','ERROR: Unable to allocate a 1D 32-bit integer array! Rank ', rank,            &
              ', offset ', array_info % get_offset(), associated(array_i4_1), array_info % get_status()
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

      type(block_meta) :: array_info
      integer(kind=4), dimension(:), contiguous, pointer :: array_i4_1

      do i = 1, 100
        ! Test freeing from block address
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
        success = the_heap % free(array_info) ! Free from block
        if (.not. success) then
          print *, 'ERROR: Could not free from block address'
          error stop 1
        end if

        ! Test freeing from block offset
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .true.) ! Allocate 'safely'
        success = the_heap % free(array_info % get_offset()) ! Free from block offset
        if (.not. success) then
          print *, 'ERROR: Could not free from block offset', array_info % get_offset()
          error stop 1
        end if
      end do

      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      !------------------------

      num_errors = 0
      total_errors = 0
      do i = 1, 1000
        array_info = the_heap % allocate(array_i4_1, [SMALL_BLOCK_SIZE], .false.)
        success = the_heap % free(array_info)
        if (.not. success) num_errors = num_errors + 1 ! Concurrency errors are usually seen when freeing the block
      end do

      call MPI_Reduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
      if (rank == 0) then
        if (.not. the_heap % check() .or. total_errors == 0) then
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
      real(kind=8), dimension(:), contiguous, pointer :: array, array2
      integer, dimension(MPI_STATUS_SIZE) :: status
      integer :: i, j, ierr
      logical :: success

      type(block_meta) :: array_info, array_info_2

      if (rank == ALLOCATOR_RANK) then

        ! Alloc once and check that the other process was able to free it
        ! This shoudl also check coalescing small blocks into a bigger one
        array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE], .false.) ! Allocate *not* safely
        offsets(1) = array_info % get_offset()
        if (.not. the_heap % is_valid_block(c_loc(array)) .or. .not. associated(array) .or. array_info % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
          print *, 'ERROR: Allocated block is not valid!'
          error stop 1
        end if

        array_info_2 = the_heap % allocate(array2, [ALLOC_BASE_SIZE], .false.)

        call MPI_Send(offsets(1), 1, MPI_INTEGER8, FREEER_RANK, 0, MPI_COMM_WORLD, ierr)
        call MPI_Recv(offsets(1), 1, MPI_INTEGER8, FREEER_RANK, 1, MPI_COMM_WORLD, status, ierr) ! Wait for reply to make sure the freeing is done
        
        success = the_heap % free(array_info)
        if (success) then
          print *, 'ERROR: Block should have been free already'
          error stop 1
        end if

        array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE*2], .false.)
        if (.not. associated(array) .or. array_info % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
          print *, 'ERROR: Could not allocate block when a smaller free block is available before an occupied one'
          error stop 1
        end if

        success = the_heap % free(array_info)
        success = the_heap % free(array_info_2) .and. success
        if (.not. success) then
          print *, 'ERRROR: Unable to free 2 blocks'
          error stop 1
        end if

        do i = 1, NUM_LOOPS
          do j = 1, NUM_ALLOC
            array_info = the_heap % allocate(array, [ALLOC_BASE_SIZE - 2*j], .false.)
            if (.not. associated(array) .or. array_info % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
              print '(A,I4)', 'ERROR: Not allocated (single allocator), i = ', i
              error stop 1
            end if
            offsets(j) = array_info % get_offset()
          end do
          call MPI_Send(offsets, NUM_ALLOC, MPI_INTEGER8, FREEER_RANK, 0, MPI_COMM_WORLD, ierr)
        end do

      else if (rank == FREEER_RANK) then
        ! Free the block allocated by another process
        call MPI_Recv(offsets(1), 1, MPI_INTEGER8, ALLOCATOR_RANK, 0, MPI_COMM_WORLD, status, ierr)
        success = the_heap % free(offsets(1))
        if (.not. success) then
          print *, 'ERROR: Unable to free... Offset ', offsets(1)
          error stop 1
        end if
        call MPI_Send(offsets(1), 1, MPI_INTEGER8, ALLOCATOR_RANK, 1, MPI_COMM_WORLD, ierr)

        do i = 1, NUM_LOOPS
          call MPI_Recv(offsets, NUM_ALLOC, MPI_INTEGER8, ALLOCATOR_RANK, 0, MPI_COMM_WORLD, status, ierr)
          do j = 1, NUM_ALLOC
            success = the_heap % free(offsets(j))
          end do
        end do
      end if
    end subroutine one_producer_one_consumer

    subroutine various_params()
      implicit none
      type(block_meta) :: info
      logical :: success
      integer(C_INT16_T), dimension(:, :, :), contiguous, pointer :: array

      if (rank .ne. 0) return

      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [2, 2])
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [2, 2, 2, 1])       .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [2, 2, 3, 1, 1, 1]) .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [2, 2, 2, 2])      .and. success

      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [1, 1, 1], [2, 2, 1])           .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [1, 4, 1], [2, 4, 1])           .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5, -1], [-2, 2])              .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5, -1, 1, 1], [-2, 2, 1, 1])  .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5, -1, 1, 1], [-5, 2, 1, 1])  .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1, 1], [2, 2, 1])             .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1, 1, 1], [2, 2])             .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1, 1, 1], [2, 0, 1])          .and. success
      success = test_alloc4(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1, 2, 1], [2, 0, 1])          .and. success

      success = test_alloc(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [1_8, 1_8, 1_8], [2_8, 2_8, 1_8])               .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [1_8, 4_8, 1_8], [2_8, 4_8, 1_8])               .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5_8, -1_8], [-2_8, 2_8])                      .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5_8, -1_8, 1_8, 1_8], [-2_8, 2_8, 1_8, 1_8])  .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_ALLOC_SUCCESS, [-5_8, -1_8, 1_8, 1_8], [-5_8, 2_8, 1_8, 1_8])  .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1_8, 1_8], [2_8, 2_8, 1_8])                   .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1_8, 1_8, 1_8], [2_8, 2_8])                   .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1_8, 1_8, 1_8], [2_8, 0_8, 1_8])              .and. success
      success = test_alloc(the_heap, array, SHMEM_HEAP_INVALID_BOUNDS, [1_8, 2_8, 1_8], [2_8, 0_8, 1_8])              .and. success

    end subroutine various_params

    subroutine timeout_test()
      use ioserver_timer_module
      use rpn_extra_module, only: sleep_us
      implicit none
      type(block_meta) :: info
      integer(kind=8), dimension(:), contiguous, pointer :: a
      integer(kind=8) :: big_size
      logical :: success
      type(ioserver_timer) :: timer
      real :: time

      integer, parameter :: ALLOC_RANK_0 = 0
      integer, parameter :: ALLOC_RANK_1 = 1

      call timer % create()

      big_size = the_heap % get_size() / (8 * 2) + 10     ! A bit more than half the heap

      if (rank == ALLOC_RANK_0) then

        info = the_heap % allocate(a, [big_size], use_safe_alloc = .true.)
        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------

        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------
        call sleep_us(100000)
        success = the_heap % free(info)
      else if (rank == ALLOC_RANK_1) then
        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------
        call timer % start()
        info = the_heap % allocate(a, [big_size])
        call timer % stop()
        time = timer % get_latest_time_ms()

        if (associated(a) .or. info % get_status() .ne. SHMEM_HEAP_ALLOC_TIMEOUT) then
          print *, 'ERROR: Allocation should have failed'
          error stop 1
        end if

        if (time < DEFAULT_ALLOC_TIMEOUT_MS) then
          print *, 'ERROR: Did not wait long enough (default is 30 seconds)'
          error stop 1
        else if (time > DEFAULT_ALLOC_TIMEOUT_MS + 2000) then
          print *, 'ERROR: Took too long to fail!'
          error stop 1
        end if

        call timer % start()
        info = the_heap % allocate(a, [int(big_size, kind = 4)], use_safe_alloc = .true., timeout_ms = 0)
        call timer % stop()
        time = timer % get_latest_time_ms()

        if (associated(a) .or. info % get_status() .ne. SHMEM_HEAP_ALLOC_TIMEOUT) then
          print *, 'ERROR: Allocation should have failed'
          error stop 1
        end if

        if (time > 10) then
          print *, 'ERROR: Took too long to fail!'
          error stop 1
        end if

        call timer % start()
        info = the_heap % allocate(a, [1_8], [big_size], timeout_ms = 20)
        call timer % stop()
        time = timer % get_latest_time_ms()

        if (associated(a) .or. info % get_status() .ne. SHMEM_HEAP_ALLOC_TIMEOUT) then
          print *, 'ERROR: Allocation should have failed'
          error stop 1
        end if

        if (time < 20) then
          print *, 'ERROR: Failed too quickly'
          error stop 1
        else if (time > 30) then
          print *, 'ERROR: Failed too slowly!'
          error stop 1
        end if

        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------

        call timer % start()
        info = the_heap % allocate(a, [1], [int(big_size, kind=4)], timeout_ms = 100)
        call timer % stop()
        time = timer % get_latest_time_ms()

        if (.not. associated(a) .or. info % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
          print *, 'ERROR: Should have succeeded with the allocation!'
          error stop 1
        end if

        if (time < 95) then
          print *, 'WARNING: Allocation was suspiciously fast ', time
        end if

        success = the_heap % free(info)

      else
        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------
        !------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !------------------------

      end if

      call timer % delete()
    end subroutine timeout_test

    subroutine array_tkr_and_values()
      implicit none

      integer, parameter :: PRODUCER_RANK = 1
      integer, parameter :: CONSUMER_RANK = 0
      integer, parameter :: NUM_ARRAYS = 6

      integer(HEAP_ELEMENT), dimension(NUM_ARRAYS) :: offsets
      type(block_meta),  dimension(NUM_ARRAYS) :: array_infos
      integer(kind=1), dimension(:, :, :, :, :), contiguous, pointer :: array_i1_5
      integer(kind=2), dimension(:, :, :, :),    contiguous, pointer :: array_i2_4
      integer(kind=4), dimension(:, :, :),       contiguous, pointer :: array_i4_3
      integer(kind=8), dimension(:, :),          contiguous, pointer :: array_i8_2
      real(kind=4),    dimension(:, :),          contiguous, pointer :: array_r4_2
      real(kind=8),    dimension(:, :, :),       contiguous, pointer :: array_r8_3

      integer, dimension(MPI_STATUS_SIZE) :: status
      integer :: ierr
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

        call MPI_Send(offsets, 6, MPI_INTEGER8, CONSUMER_RANK, 0, MPI_COMM_WORLD, ierr)
      else if (rank == CONSUMER_RANK) then

        call MPI_Recv(offsets, 6, MPI_INTEGER8, PRODUCER_RANK, 0, MPI_COMM_WORLD, status, ierr)

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

        do i = 1, 6
          success = the_heap % free(offsets(i))
          if (.not. success) then
            print *, 'ERROR: heap % free()'
            error stop 1
          end if
        end do
      end if
    end subroutine array_tkr_and_values

    subroutine big_alloc()
      implicit none
      type(block_meta) :: array_info1, array_info2
      integer(kind=8), dimension(:), contiguous, pointer :: kinda_big_array1, kinda_big_array2, big_array
      integer(kind=8) :: kinda_big_alloc_size, big_alloc_size
      integer :: num_errors, total_errors
      logical :: success

      kinda_big_alloc_size = SHMEM_HEAP_SIZE_BYTE / int(num_procs * 8 * 2, 8) + 1
      big_alloc_size = the_heap % get_size() / 8 - 100

      array_info1 = the_heap % allocate(kinda_big_array1, [kinda_big_alloc_size], .true.)
      array_info2 = the_heap % allocate(kinda_big_array2, [kinda_big_alloc_size], .true.)

      num_errors = 0
      total_errors = 0
      if (.not. associated(kinda_big_array1) .or. array_info1 % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) num_errors = num_errors + 1
      if (.not. associated(kinda_big_array2) .or. array_info2 % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) num_errors = num_errors + 1

      call MPI_Reduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      !------------------------
      if (rank == 0) then
        if (total_errors .ne. 1) then
          print *, 'ERROR: There should be exactly 1 array that could not be allocated', total_errors
          error stop 1
        end if
      end if
      
      success = the_heap % free(array_info1)
      success = the_heap % free(array_info2) .and. success

      !------------------------
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      !------------------------

      if (rank == 0) then
        if (abs(the_heap % get_size() - SHMEM_HEAP_SIZE_BYTE) > 200) then
          print '(A,I12,A,I12)', 'ERROR: The heap should be able to contain approximately the same amount as we asked. Asked for ', &
              SHMEM_HEAP_SIZE_BYTE, ', got ', the_heap % get_size()
          error stop 1
        end if

        array_info1 = the_heap % allocate(big_array, [big_alloc_size])
        if (.not. associated(big_array) .or. array_info1 % get_status() .ne. SHMEM_HEAP_ALLOC_SUCCESS) then
          print *, 'ERROR: Unable to allocate array of size ', big_alloc_size
          error stop 1
        end if

        success = the_heap % free(array_info1)
        if (.not. success) then
          print *, 'ERROR: Unable to free the huge array of size ', big_alloc_size
          error stop 1
        end if

        array_info1 = the_heap % allocate(big_array, [the_heap % get_size() / 8 + 1], timeout_ms = 0)
        if (associated(big_array) .or. array_info1 % get_status() == SHMEM_HEAP_ALLOC_SUCCESS) then
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
  integer :: node_comm
  integer :: node_size
  integer :: ierr

  type(C_PTR) :: shared_mem

  type(shmem_heap) :: the_heap
  logical    :: success

  !!!!!!!!!!!!!!!!!!!!!!
  ! Initialization
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, node_comm, ierr)
  call MPI_Comm_size(node_comm, node_size, ierr)

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
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
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
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------

  !!!!!!!!!!!!!!!!
  ! Run the test
  call run_test(the_heap)

  !!!!!!!!!!!!!!
  ! We're done
  !-------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !-------------------------------
  call MPI_Finalize(ierr)

  if (rank == 0) print *, 'All good'

end program shmem_heap_basic
