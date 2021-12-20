
module disk_bandwidth_module
  use test_helper_module
  implicit none
  contains
end module disk_bandwidth_module

program disk_bandwidth
  use mpi_f08
  use disk_bandwidth_module
  implicit none
  integer(C_INT64_T) :: time0, time1
  integer, parameter :: MAX_NUM_PROCS = 12
  integer, parameter :: TOTAL_DATA_GB = 20
  integer, parameter :: MAX_BUFFER_SIZE_KB = 5000
  integer, parameter :: NUM_BUFFER_ELEMENTS = MAX_BUFFER_SIZE_KB * 1000 / 4
  integer, dimension(:), allocatable :: buffer
  integer :: process_data_kb, num_writes

  real :: total_time
  integer :: size, rank
  integer :: i, j
  integer :: max_proc

  integer, parameter :: NUM_BUF_ELEM_COUNTS = 7
  integer, dimension(NUM_BUF_ELEM_COUNTS) :: buf_elem_counts
  real,    dimension(NUM_BUF_ELEM_COUNTS) :: rates
  integer :: i_buf_elem_count, num_elem

  integer           :: file_unit
  character(len=14) :: file_name

  allocate(buffer(NUM_BUFFER_ELEMENTS))

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, size)

  write(file_name,'(A6, I4.4, A4)') 'SERVER', rank, '.out'
  print *, 'Writing to file: ', file_name

  if (rank == 0) print *, 'Num writes: ', num_writes

  do i = 1, NUM_BUFFER_ELEMENTS
    buffer(i) = rank * NUM_BUFFER_ELEMENTS + i
  end do

  max_proc = size
  if (max_proc > MAX_NUM_PROCS) max_proc = MAX_NUM_PROCS

  buf_elem_counts(1) = 1000
  buf_elem_counts(2) = 10000
  buf_elem_counts(3) = 50000
  buf_elem_counts(4) = 100000
  buf_elem_counts(5) = 200000
  buf_elem_counts(6) = 500000
  buf_elem_counts(7) = 1000000

  if (rank == 0) then
    print *, 'Total data: ', TOTAL_DATA_GB, ' GB'
    print *, 'Rates in GB/s'
    print '(A3, I7, I7, I7, I7, I7, I7, I7, A)', '', buf_elem_counts(:) * 4 / 1000, ' (Buffer size in kB)'
  end if

  do j = 1, max_proc

    do i_buf_elem_count = 1, NUM_BUF_ELEM_COUNTS
      num_elem = buf_elem_counts(i_buf_elem_count)
      process_data_kb = TOTAL_DATA_GB * 1000000 !/ j
      num_writes = process_data_kb / num_elem * 1000 / 4
      ! if (rank == 0) then
      !   print *, 'num_elem, process data Kb, num writes: ', num_elem, process_data_kb, num_writes
      ! end if
      !---------------------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !---------------------------------------
      time0 = get_current_time_us()
      !---------------------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !---------------------------------------
      if (rank < j) then
        open(newunit = file_unit, file = file_name, status = 'replace', form = 'unformatted')
        do i = 1, num_writes
            write(file_unit) buffer(1:num_elem)
        end do
        close(file_unit)
      end if
      !---------------------------------------
      call MPI_Barrier(MPI_COMM_WORLD)
      !---------------------------------------
      time1 = get_current_time_us()
      total_time =  real(time1 - time0, 4) / 1000000.0
      rates(i_buf_elem_count) = TOTAL_DATA_GB * j / total_time
      ! if (rank == 0) then
      !   print *, 'total time, rate', total_time, rates(i_buf_elem_count)
      ! end if
    end do

    if (rank == 0) then
        print '(I3, F7.1, F7.1, F7.1, F7.1, F7.1, F7.1, F7.1)', j, rates(:)
        ! print *, TOTAL_DATA_GB / total_time, ' GB/s'
        ! print *, 'Took ', (time1 - time0) / 1000000.0, ' s'
    end if
  end do

  call MPI_Finalize()
  deallocate(buffer)
end program disk_bandwidth
