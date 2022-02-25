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


module disk_bandwidth_module
  use test_helper_module
  implicit none
  contains
end module disk_bandwidth_module

program disk_bandwidth
  use ioserver_mpi
  use disk_bandwidth_module
  implicit none
  integer(C_INT64_T) :: time0, time1
  integer, parameter :: MAX_NUM_PROCS = 16
  integer, parameter :: TOTAL_DATA_GB = 20
  integer, parameter :: MAX_BUFFER_SIZE_KB = 5000
  integer, parameter :: NUM_BUFFER_ELEMENTS = MAX_BUFFER_SIZE_KB * 1000 / 4
  integer, dimension(:), allocatable :: buffer
  integer :: process_data_kb, num_writes

  real :: total_time
  integer :: size, rank
  integer :: i, i_proc, i_node
  integer :: max_proc, num_nodes
  integer :: node_comm, controller_comm
  integer :: node_rank, node_size, node_id
  integer :: ierr

  integer, parameter :: NUM_BUF_ELEM_COUNTS = 7
  integer, dimension(NUM_BUF_ELEM_COUNTS) :: buf_elem_counts
  real,    dimension(NUM_BUF_ELEM_COUNTS) :: rates
  integer :: i_buf_elem_count, num_elem

  integer           :: file_unit
  character(len=14) :: file_name

  allocate(buffer(NUM_BUFFER_ELEMENTS))

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

  call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, node_comm, ierr)
  call MPI_Comm_rank(node_comm, node_rank, ierr)
  call MPI_Comm_size(node_comm, node_size, ierr)

  ! Find out how many MPI nodes there are
  block
    integer :: count
    integer :: tmp_comm
    count = 0
    if (node_rank == 0) count = 1
    call MPI_Allreduce(count, num_nodes, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

    if (node_rank == 0) then
      call MPI_Comm_split(MPI_COMM_WORLD, 0, rank, controller_comm, ierr)
      call MPI_Comm_rank(controller_comm, node_id, ierr)
    else
      call MPI_Comm_split(MPI_COMM_WORLD, 1, rank, tmp_comm, ierr)
    end if

    call MPI_Bcast(node_id, 1, MPI_INTEGER, 0, node_comm, ierr)
  end block

  write(file_name,'(A6, I4.4, A4)') 'SERVER', rank, '.out'
  ! print *, 'Writing to file: ', file_name

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

  !---------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !---------------------------------------
  if (rank == 0) then
    print *, 'Total data: ', TOTAL_DATA_GB, ' GB'
    print *, 'Rates in GB/s'
    print '(A6, I7, I7, I7, I7, I7, I7, I7, A)', '', buf_elem_counts(:) * 4 / 1000, ' (Buffer size in kB)'
  end if

  do i_node = 1, num_nodes
    do i_proc = 1, max_proc

      if (i_proc > 1 .and. mod(i_proc, 2) .ne. 0) cycle
      if (i_proc > 1 .and. i_node > 1 .and. mod(i_proc, 4) .ne. 0) cycle

      do i_buf_elem_count = 1, NUM_BUF_ELEM_COUNTS
        if (i_proc < 5) then
          process_data_kb = TOTAL_DATA_GB * 1000000 
        else if (i_proc < 13) then
          process_data_kb = TOTAL_DATA_GB * 1000000 / 2
        else
          process_data_kb = TOTAL_DATA_GB * 1000000 / 4
        end if

        num_elem   = buf_elem_counts(i_buf_elem_count)
        num_writes = process_data_kb / num_elem * 1000 / 4
        ! if (rank == 0) then
        !   print *, 'num_elem, process data Kb, num writes: ', num_elem, process_data_kb, num_writes
        ! end if
        !---------------------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !---------------------------------------
        time0 = get_current_time_us()
        !---------------------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !---------------------------------------
        if (node_rank < i_proc .and. node_id < i_node) then
          open(newunit = file_unit, file = file_name, status = 'replace', form = 'unformatted')
          do i = 1, num_writes
              write(file_unit) buffer(1:num_elem)
          end do
          close(file_unit)
        end if
        !---------------------------------------
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !---------------------------------------
        time1 = get_current_time_us()
        total_time =  real(time1 - time0, 4) / 1000000.0
        rates(i_buf_elem_count) = process_data_kb / 1000000 * i_proc * i_node / total_time
        ! if (rank == 0) then
        !   print *, 'total time, rate', total_time, rates(i_buf_elem_count)
        ! end if
      end do

      if (rank == 0) then
          print '(I2, 1X, I3, F7.1, F7.1, F7.1, F7.1, F7.1, F7.1, F7.1)', i_node, i_proc, rates(:)
          ! print *, TOTAL_DATA_GB / total_time, ' GB/s'
          ! print *, 'Took ', (time1 - time0) / 1000000.0, ' s'
      end if
    end do
  end do

  call MPI_Finalize(ierr)
  deallocate(buffer)
end program disk_bandwidth
