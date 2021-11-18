
program test_server_stream_open
  use ISO_C_BINDING
  use mpi_f08

  use server_stream_module
  use shared_mem_alloc_module
  use simple_mutex_module
  implicit none

  integer, parameter :: NUM_FILES = 50

  integer :: rank, num_pes
  type(C_PTR) :: shared_mem, file_shared_mem
  integer(C_SIZE_T) :: shared_mem_size
  type(simple_mutex), dimension(:), allocatable :: mutexes

  integer(C_INT), dimension(:), pointer :: int_array
  type(shared_server_stream), dimension(:), pointer :: file_array

  type(shared_server_stream) :: dummy_server_stream
  character(len=11) :: file_name

  logical :: success
  integer :: num_errors, i

  num_errors = 0

  ! ---------------------
  ! Initialize everything
  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, num_pes)

  shared_mem_size = (NUM_FILES * 4) + (NUM_FILES * storage_size(dummy_server_stream) / 8)
  shared_mem = RPN_allocate_shared(shared_mem_size, MPI_COMM_WORLD)

  call c_f_pointer(shared_mem, int_array, [NUM_FILES + 4])
  file_shared_mem = c_loc(int_array(NUM_FILES + 1))
  call c_f_pointer(file_shared_mem, file_array, [NUM_FILES])

  allocate(mutexes(NUM_FILES))
  do i = 1, NUM_FILES
    call mutexes(i) % init_from_int(int_array(i), rank)
  end do

  if (rank == 0) then
    do i = 1, NUM_FILES
      file_array(i) = shared_server_stream()
    end do
  end if

  call MPI_Barrier(MPI_COMM_WORLD)

  ! ---------------
  ! Do the test
  do i = 1, NUM_FILES
    if (mutexes(i) % try_lock()) then
      if (.not. file_array(i) % is_valid()) then
        write(file_name  ,'(A6,I5.5)') "SERVER", i
        success = file_array(i) % open(rank + 1, file_name, rank)
        if (.not. success) then
          print *, 'ERROR: unable to open stream file ', file_name, rank
          num_errors = num_errors + 1
        end if
        ! print *, int_array(1:NUM_FILES + 2), i, rank
      end if

      if (.not. mutexes(i) % is_locked_by_me()) then
        print *, 'ERROR: mutex is not owned by me!!!', i, rank
        print *, int_array(1:NUM_FILES)
      end if

      call mutexes(i) % unlock()
    end if
  end do


  call MPI_Barrier(MPI_COMM_WORLD)

  if (rank == 0) then
    do i = 1, NUM_FILES
      if (.not. file_array(i) % is_open()) then
        print *, 'ERROR: File is not open!', i
        num_errors = num_errors + 1
      end if
    end do
  end if

  ! -----------------
  if (num_errors > 0) then
    print *, 'There were errors!'
    error stop 1
  end if

  deallocate(mutexes)
  call MPI_Finalize()

end program test_server_stream_open
