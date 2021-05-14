
module disk_bandwidth_module
  use test_helper_module
  implicit none
  include 'mpif.h'

  contains
end module disk_bandwidth_module

program disk_bandwidth
  use disk_bandwidth_module
  implicit none
  integer(C_INT64_T) :: time0, time1
  integer, parameter :: TOTAL_DATA_GB = 30
  integer, parameter :: BUFFER_SIZE_KB = 2000
  integer, parameter :: NUM_BUFFER_ELEMENTS = BUFFER_SIZE_KB * 1000 / 4
  integer, dimension(NUM_BUFFER_ELEMENTS) :: buffer
  integer :: process_data_kb, num_writes

  real :: total_time
  integer :: size, rank
  integer :: ierror, i, j
  integer :: max_proc

  integer           :: file_unit
  character(len=14) :: file_name

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

  write(file_name,'(A6, I4.4, A4)') 'SERVER', rank, '.out'
  print *, 'Writing to file: ', file_name

  if (rank == 0) print *, 'Num writes: ', num_writes

  do i = 1, NUM_BUFFER_ELEMENTS
    buffer(i) = rank * NUM_BUFFER_ELEMENTS + i
  end do

  max_proc = size
  if (max_proc > 20) max_proc = 20

  do j = 1, max_proc
    process_data_kb = TOTAL_DATA_GB * 1000000 / j
    num_writes = process_data_kb / BUFFER_SIZE_KB
    !---------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    !---------------------------------------
    time0 = get_current_time_us()
    !---------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    !---------------------------------------
    if (rank < j) then
      open(newunit = file_unit, file = file_name, status = 'replace', form = 'unformatted')
      do i = 1, num_writes
          write(file_unit) buffer(:)
      end do
      close(file_unit)
    end if
    !---------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    !---------------------------------------
    time1 = get_current_time_us()

    if (rank == 0) then
        total_time =  (time1 - time0) / 1000000.0
        print '(I3, A7, I3, A7, F7.2, A4, F5.2, A6)', &
              j, ' Wrote ', TOTAL_DATA_GB, ' GB in ', total_time, ' s (', TOTAL_DATA_GB / total_time, ' GB/s)'
        ! print *, TOTAL_DATA_GB / total_time, ' GB/s'
        ! print *, 'Took ', (time1 - time0) / 1000000.0, ' s'
    end if
  end do

  call MPI_Finalize(ierror)
end program disk_bandwidth
