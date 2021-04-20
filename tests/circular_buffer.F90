! Copyright (C) 2021  Environnement et Changement climatique Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

program test_circular_buffer
  implicit none

  call shared_mem_test()

end program test_circular_buffer


subroutine shared_mem_test()

  use ISO_C_BINDING
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  implicit none

  include 'mpif.h'

  interface
  subroutine init_array(array, rank)
    use circular_buffer_module, only: DATA_ELEMENT
    implicit none
    integer(DATA_ELEMENT), dimension(:), intent(out) :: array
    integer, intent(in) :: rank
  end subroutine init_array
  end interface

  integer, parameter :: NUM_BUFFER_ELEMENTS = 128
  integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: NPTEST = 200
  integer, parameter :: STEP_SIZE = 5

  integer(MPI_ADDRESS_KIND), parameter :: WINDOW_SIZE = 1024 * 1024


  type(circular_buffer) :: buffer_a, buffer_b
  type(C_PTR)           :: shmem_ptr_a, shmem_ptr_b
  integer(DATA_ELEMENT) :: dummy_element
  logical               :: success, dummy_bool

  integer(DATA_ELEMENT), dimension(NPTEST) :: local_data, received_data, source_data

  integer(KIND=MPI_ADDRESS_KIND) :: base_mem_ptr, target_mem_ptr, target_size

  integer :: my_rank, num_procs
  integer :: ierr, i, n, errors, tmp_errors
  integer :: window, disp_unit, target_disp_unit
  integer :: target_proc, source_proc
  integer(C_INT) :: capacity

  errors = 0

  ! Initialize MPI
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

  if (num_procs < 2) then
    print *, 'This test needs at least 2 processes'
    errors = 1
    goto 999
  end if

  target_proc = mod(my_rank + 1, num_procs)             ! next in the "ring"
  source_proc = mod(my_rank + num_procs - 1, num_procs) ! previous in the "ring"

!  print *, 'This is PE', my_rank + 1, ' of', num_procs

  ! Allocate MPI window in shared memory
  disp_unit = C_SIZEOF(dummy_element)
  call MPI_Win_allocate_shared(WINDOW_SIZE, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, base_mem_ptr, window, ierr)
  call MPI_Win_shared_query(window, target_proc, target_size, target_disp_unit, target_mem_ptr, ierr)  ! get my victim's base address

  ! Initialize local data
  call init_array(local_data, my_rank)
  call init_array(source_data, source_proc)
  received_data(:) = -1

  shmem_ptr_a  = transfer(base_mem_ptr, C_NULL_PTR)   ! pointer to my circular buffer
  shmem_ptr_b  = transfer(target_mem_ptr, C_NULL_PTR) ! pointer to my target's circular buffer
  success = buffer_a % create(shmem_ptr_a, NUM_BUFFER_ELEMENTS)  ! create my circular buffer
  dummy_bool = buffer_b % create(shmem_ptr_b)                      ! point to target's circular buffer

  capacity = buffer_a % get_capacity()
  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (.not. success .or. .not. buffer_a % is_valid() .or. .not. buffer_b % is_valid()) then
    print *, 'Buffer initialisation failed ', buffer_a % is_valid(), buffer_b % is_valid()
    errors = errors + 1
  end if

  if ((buffer_a % get_num_elements() .ne. 0) .or. (buffer_b % get_num_elements() .ne. 0)) then
    print *, 'GOT ERROR 0'
    errors = errors + 1
  end if

  if (buffer_a % get_num_spaces() .ne. buffer_b % get_num_spaces()) then
    print *, 'GOT ERROR: buffer spaces are ', buffer_a % get_num_spaces(), buffer_b % get_num_spaces()
    errors = errors + 1
  end if


  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  n = buffer_b % atomic_put(local_data, STEP_SIZE, .false.) ! inject data into target's circular buffer

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (buffer_a % get_num_elements() .ne. 0) then
    print *, 'GOT ERROR. We did not commit the transaction, but there is data in the buffer!'
    errors = errors + 1
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  n = buffer_b % atomic_put(local_data, 0, .true.)

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (buffer_a % get_num_elements() .ne. STEP_SIZE) then
    print *, 'GOT ERROR. We just committed the previous transaction, so there should be exactly that many elements: ', STEP_SIZE
    errors = errors + 1
  end if

  n = buffer_a % peek(received_data, STEP_SIZE)

  if ((buffer_a % get_num_elements() .ne. STEP_SIZE) .or. (n .ne. STEP_SIZE)) then
    print *, 'GOT ERROR. We just peeked at the buffer, but the resulting number of elements is wrong!', n, STEP_SIZE
    errors = errors + 1
  end if

  n = buffer_a % atomic_get(received_data, STEP_SIZE, .false.)
  if (buffer_a % get_num_elements() .ne. 0) then
    print *, 'GOT ERROR. We just read the data, but it looks like the buffer is *not* empty', n
    errors = errors + 1
  end if

  if (buffer_a % get_num_spaces() .ne. capacity - STEP_SIZE) then
    print *, 'GOT ERROR. We only read the data without extracting it. The space should not be available'
    errors = errors + 1
  end if

  n = buffer_a % atomic_get(received_data, 0, .true.)
  if ((buffer_a % get_num_elements() .ne. 0) .or. (buffer_a % get_num_spaces() .ne. capacity)) then
    print *, 'GOT ERROR. Buffer should be completely empty'
    errors = errors + 1
  end if

  print *, my_rank, 'Got to this point'

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  n = buffer_b % atomic_put(local_data, STEP_SIZE, .true.) ! inject data into target's circular buffer

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  n = buffer_a % atomic_get(received_data, STEP_SIZE - 1, .true.) ! get from my own buffer (put there by source_proc)

  if (.not. all(received_data(1:STEP_SIZE - 2) == source_data(1:STEP_SIZE - 2))) then
    print *, 'GOT ERROR, data put directly in buffer by neighbor process (first call)'
    errors = errors + 1
  end if

  n = buffer_a % atomic_get(received_data(STEP_SIZE), 1, .true.)  ! get from my own buffer (remainder of what was put)

  if (.not. all(received_data(1:STEP_SIZE - 2) == source_data(1:STEP_SIZE - 2))) then
    print *, 'GOT ERROR, data put directly in buffer by neighbor process (second call)'
    errors = errors + 1
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr) ! we want no interference from further down
  !--------------------------------------

  do i = 1, NPTEST, STEP_SIZE  ! ring test with wraparound , make sure NPTEST > size of circular buffer
    if(my_rank == 0) then
      n = buffer_b % atomic_put(local_data(i) , STEP_SIZE, .true.) ! send to next in ring
      n = buffer_a % atomic_get(received_data(i), STEP_SIZE, .true.) ! then get from previous in ring

      if (.not. all(local_data(i:i + STEP_SIZE - 1) == received_data(i:i + STEP_SIZE - 1))) then
        print *, 'GOT ERROR in ring data'
        errors = errors + 1
      end if

    else
      n = buffer_a % atomic_get(received_data(i) , STEP_SIZE, .true.) ! get from previous in ring
      n = buffer_b % atomic_put(received_data(i) , STEP_SIZE, .true.) ! pass to next in ring
    endif

  enddo
  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if ((buffer_a % get_num_elements() .ne. 0) .or. (buffer_b % get_num_elements() .ne. 0)) then
    print *, 'GOT ERROR, there is some data left after the entire ring transmission is over'
    errors = errors + 1
  end if

  tmp_errors = errors
  call MPI_Reduce(tmp_errors, errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if(my_rank == 0) then  ! check that we got back what we sent
    if(errors > 0) then
      print *, 'RING ERRORS: ', errors
      print 3,received_data(1:NPTEST)
    else
      print *, 'Shared memory circular buffer test has succeeded'
    end if
3   format(25I6)
  endif

999 continue

  success = buffer_a % delete()
  success = buffer_b % delete()

  call MPI_Win_free(window, ierr)
  call MPI_Finalize(ierr)

  if (errors > 0) error stop 1

end subroutine shared_mem_test

subroutine init_array(array, rank)
  use circular_buffer_module, only: DATA_ELEMENT
  implicit none

  integer(DATA_ELEMENT), dimension(:), intent(out) :: array
  integer, intent(in) :: rank

  integer :: i
  do i = 1, size(array)
    array(i) = (rank + 1) * 10000 + i
  end do
end subroutine init_array
