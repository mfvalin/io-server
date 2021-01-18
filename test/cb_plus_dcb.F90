! Copyright (C) 2021 Recherche en Prevision Numerique
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation,
! version 2 of the License.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this program; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.

program test_both_buffers
  implicit none
  call run_test()
end program test_both_buffers

module parameters
  implicit none
  public

  integer, parameter :: NUM_IO_BUFFER_ELEMENTS = 128
  integer, parameter :: NUM_WORKER_BUFFER_ELEMENTS = 128
  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 64

  integer, parameter :: NUM_TEST_WORKER_DATA = 100000
  integer, parameter :: WORKER_DATA_CHUNK_SIZE = 5

contains
  function compute_data_val(node_id, node_rank, id) result(val)
    implicit none
    integer, intent(in) :: node_id, node_rank, id
    integer :: val

    val = node_id * 100000000 + node_rank * 1000000 + id
  end function compute_data_val

end module parameters



subroutine run_test()

!  use ISO_C_BINDING
!  use circular_buffer_module, only: circular_buffer, DATA_ELEMENT
  use distributed_circular_buffer_module, only: distributed_circular_buffer
  use parameters
  implicit none

  include 'mpif.h'

  integer :: global_rank, num_procs
  integer :: node_comm, io_comm
  integer :: node_id, num_worker_nodes, num_worker_per_node, num_consumer_procs, num_channels
  logical :: is_io, is_receiver, is_relay, is_worker
  integer :: ierr

  integer :: tmp_errors, num_errors = 0

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)

  if (num_procs .lt. 8) then
    print *, 'Wait, this test program needs at least 8 processes to run! (2 IO, 2 receivers, and 2+ worker nodes with 2+ processes each)'
    num_errors = num_errors + 1
    goto 999
  end if

  call init_comms(global_rank, num_procs, node_comm, io_comm, node_id, num_worker_nodes, num_worker_per_node, \
                  num_consumer_procs, num_channels, is_io, is_receiver, is_relay, is_worker)

  if (is_io) then
    call run_io_server(node_comm, io_comm, num_consumer_procs, num_worker_nodes, num_worker_per_node, num_channels, num_errors)
  else if (is_receiver) then
    call run_receiver_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)
  else if (is_relay) then
    call run_relay_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)
  else if (is_worker) then
    call run_worker_process(node_comm, node_id, num_errors)
  else
    print *, 'Something wrong!'
    num_errors = num_errors + 1
  end if

999 continue

  tmp_errors = num_errors
  call MPI_Reduce(tmp_errors, num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (global_rank == 0) then
    if (num_errors > 0) then
      print *, 'THERE WERE ERRORS: ', num_errors
    else
      print *, 'Test over (successfully).'
    end if
  end if

  call MPI_Finalize(ierr)

end subroutine run_test

subroutine init_comms(global_rank, num_procs, node_comm, io_comm, node_id, \
                      num_worker_nodes, num_worker_per_node, num_consumer_procs, num_channels, \
                      is_io, is_receiver, is_relay, is_worker)
  implicit none
  include 'mpif.h'

  integer, intent(IN) :: global_rank
  integer, intent(IN) :: num_procs
  integer, intent(out) :: node_comm
  integer, intent(out) :: io_comm
  integer, intent(out) :: node_id
  integer, intent(out) :: num_worker_nodes, num_worker_per_node, num_consumer_procs, num_channels
  logical, intent(out) :: is_io, is_receiver, is_relay, is_worker

  integer :: num_worker_procs = 0
  integer :: num_total_nodes = 0
  integer :: error

  is_io = .false.
  is_receiver = .false.
  is_relay = .false.
  is_worker = .false.

  num_worker_nodes = -1
  num_channels = -1

  if (num_procs >= 8 .and. num_procs <= 10) then
    ! 2 Relays, 2 IO, the rest are workers

    num_channels = 2
    if (mod(num_procs, 2) == 1) num_channels = 1 ! 1 receiver process per channel
    num_consumer_procs = 2
    num_worker_nodes = 2 ! = number of relay processes
    num_total_nodes = 3
    num_worker_procs = num_procs - (num_consumer_procs + num_channels + num_worker_nodes)
    num_worker_per_node = num_worker_procs / num_worker_nodes
  else
    print *, 'init_comms NOT implemented for that number of processes...'
    return
  end if

  ! First classify the processes, according to their rank
  if (global_rank < num_worker_nodes) then
    is_relay = .true.
  else if (global_rank < num_worker_nodes + num_channels) then
    is_receiver = .true.
  else if (global_rank < num_worker_nodes + num_channels + num_consumer_procs) then
    is_io = .true.
  else
    is_worker = .true.
  end if

  if (is_io .or. is_receiver) then
    node_id = 0
  else
    node_id = mod(global_rank, num_worker_nodes) + 1
  end if

  print *, 'global rank, node id: ', global_rank, node_id

  ! Create the individual node communicators
  call MPI_Comm_split(MPI_COMM_WORLD, node_id, 0, node_comm, error)

  ! Create the IO communicator
  if (.not. is_worker) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, 0, io_comm, error)
  else
    call MPI_Comm_split(MPI_COMM_WORLD, MPI_UNDEFINED, 0, io_comm, error)
  end if




end subroutine init_comms


subroutine run_io_server(node_comm, io_comm, num_consumer_procs, num_worker_nodes, num_worker_per_node, num_channels, num_errors)

  use distributed_circular_buffer_module, only: distributed_circular_buffer
  use circular_buffer_module
  use parameters

  implicit none

  integer, intent(in)    :: node_comm, io_comm
  integer, intent(in)    :: num_consumer_procs, num_worker_nodes, num_worker_per_node, num_channels
  integer, intent(inout) :: num_errors

  integer :: consumer_id
  integer :: i_data, i_node, i_check, i_worker

  type(distributed_circular_buffer) :: io_buffer
  logical :: success
  integer :: num_elements, total_received

  integer(DATA_ELEMENT), dimension(MAX_NUM_WORKER_PER_NODE, NUM_WORKER_BUFFER_ELEMENTS) :: received_data

  success = io_buffer % create(io_comm, node_comm, num_worker_nodes, num_channels, NUM_IO_BUFFER_ELEMENTS)

  consumer_id = io_buffer % get_consumer_id()

!  call sleep_us(1000000)

  total_received = 0
  do i_data = 1, NUM_TEST_WORKER_DATA, WORKER_DATA_CHUNK_SIZE
    do i_node = 1 + consumer_id, num_worker_nodes, num_consumer_procs
      num_elements = io_buffer % get(i_node - 1, received_data, WORKER_DATA_CHUNK_SIZE * num_worker_per_node)
      total_received = total_received + WORKER_DATA_CHUNK_SIZE * num_worker_per_node

      do i_worker = 1, num_worker_per_node
        do i_check = 1, WORKER_DATA_CHUNK_SIZE
          if (received_data(i_worker, i_check) .ne. compute_data_val(i_node, i_worker, i_check + i_data)) then
            num_errors = num_errors + 1
          end if
        end do
      end do



!      print *, 'Total received: ', total_received
    end do
  end do
!  print *, 'END OF RECEIVING'


  call io_buffer % delete()

end subroutine run_io_server


!> Do whatever a receiver process has to do: create the buffer, loop to refresh any incoming data, then delete the buffer
subroutine run_receiver_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)

  use distributed_circular_buffer_module, only: distributed_circular_buffer
  use parameters

  implicit none

  integer, intent(in)    :: node_comm, io_comm
  integer, intent(in)    :: num_worker_nodes, num_channels
  integer, intent(inout) :: num_errors

  integer :: io_rank, num_io_procs
  integer :: error, return_value

  type(distributed_circular_buffer) :: io_buffer
  logical :: success

  call MPI_Comm_rank(io_comm, io_rank, error)
  call MPI_Comm_size(io_comm, num_io_procs, error)

  success = io_buffer % create(io_comm, node_comm, num_worker_nodes, num_channels, NUM_IO_BUFFER_ELEMENTS)
  return_value = io_buffer % start_receiving()

  if (return_value .ne. 0) num_errors = num_errors + 1

  call io_buffer % delete()

end subroutine run_receiver_process


subroutine run_relay_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)

  use circular_buffer_module
  use distributed_circular_buffer_module, only: distributed_circular_buffer
  use parameters

  implicit none
  include 'mpif.h'

  integer, intent(in)    :: node_comm, io_comm
  integer, intent(in)    :: num_worker_nodes, num_channels
  integer, intent(inout) :: num_errors

  integer :: io_rank, num_io_procs, node_rank, num_node_processes
  integer :: window, disp_unit
  integer(MPI_ADDRESS_KIND) :: window_size
  integer :: error, i, j
  integer :: num_elements, num_spaces, total_sent

  type(distributed_circular_buffer) :: io_buffer
  type(circular_buffer), dimension(MAX_NUM_WORKER_PER_NODE) :: worker_buffers
  integer, dimension(MAX_NUM_WORKER_PER_NODE, WORKER_DATA_CHUNK_SIZE) :: processed_data
  integer, dimension(WORKER_DATA_CHUNK_SIZE) :: received_data
  integer(DATA_ELEMENT)     :: dummy_element
  integer(MPI_ADDRESS_KIND) :: base_mem_ptr
  type(C_PTR)               :: shmem_ptr
  logical :: success

  call MPI_Comm_rank(io_comm, io_rank, error)
  call MPI_Comm_size(io_comm, num_io_procs, error)
  call MPI_Comm_rank(node_comm, node_rank, error)
  call MPI_Comm_size(node_comm, num_node_processes, error)


  ! Create the circular buffers for every worker on this node
  disp_unit = C_SIZEOF(dummy_element)
  window_size = 0
  call MPI_Win_allocate_shared(window_size, disp_unit, MPI_INFO_NULL, node_comm, base_mem_ptr, window, error)

  print *, 'Relay process, rank/size', node_rank, num_node_processes

  do i = 1, num_node_processes - 1
    call MPI_Win_shared_query(window, i, window_size, disp_unit, base_mem_ptr, error)
    shmem_ptr = transfer(base_mem_ptr, C_NULL_PTR)
    success = worker_buffers(i) % create(shmem_ptr)
    if (.not. success) num_errors = num_errors + 1
  end do

  ! Create the distributed circular buffer for sharing with the IO node
  print *, 'creating DCB (relay)'
  success = io_buffer % create(io_comm, MPI_COMM_NULL, num_worker_nodes, num_channels, 0)
  print *, 'created DCB (relay)'


  total_sent = 0
  do i = 1, NUM_TEST_WORKER_DATA, WORKER_DATA_CHUNK_SIZE

!    print *, 'receiving from worker'

    do j = 1, num_node_processes - 1
      num_elements = worker_buffers(j) % atomic_get(received_data, WORKER_DATA_CHUNK_SIZE)
      processed_data(j,:) = received_data
    end do

!    print *, 'sending ', processed_data(1:num_node_processes-1, :)
    num_spaces = io_buffer % put(processed_data, WORKER_DATA_CHUNK_SIZE * (num_node_processes - 1))
    total_sent = total_sent + WORKER_DATA_CHUNK_SIZE * (num_node_processes - 1)
!    print *, 'Total sent: ', total_sent, io_rank
!    print *, 'num spaces: ', num_spaces
  end do
!  print *, 'END OF SENDING'

  ! Cleanup
  call io_buffer % delete()
  call MPI_Win_free(window, error)

end subroutine run_relay_process

subroutine run_worker_process(node_comm, node_id, num_errors)

  use circular_buffer_module
  use parameters
  implicit none
  include 'mpif.h'

  integer, intent(in)    :: node_comm
  integer, intent(in)    :: node_id
  integer, intent(inout) :: num_errors

  integer :: num_node_processes, node_rank
  integer :: window, disp_unit
  integer(MPI_ADDRESS_KIND) :: window_size
  integer(DATA_ELEMENT)     :: dummy_element
  integer(MPI_ADDRESS_KIND) :: base_mem_ptr
  type(C_PTR)               :: shmem_ptr
  type(circular_buffer)     :: buffer
  integer :: error, i, j
  integer :: num_spaces
  logical :: success
  integer, dimension(WORKER_DATA_CHUNK_SIZE) :: data_entry

  call MPI_Comm_rank(node_comm, node_rank, error)
  call MPI_Comm_size(node_comm, num_node_processes, error)
  print *, 'Worker process, rank/size', node_rank, num_node_processes

  disp_unit = C_SIZEOF(dummy_element)
  window_size = NUM_WORKER_BUFFER_ELEMENTS * disp_unit
  call MPI_Win_allocate_shared(window_size, disp_unit, MPI_INFO_NULL, node_comm, base_mem_ptr, window, error)

  shmem_ptr = transfer(base_mem_ptr, C_NULL_PTR)
  success = buffer % create(shmem_ptr, NUM_WORKER_BUFFER_ELEMENTS)

  if (.not. success) num_errors = num_errors + 1

  do i = 1, NUM_TEST_WORKER_DATA, WORKER_DATA_CHUNK_SIZE
    do j = 1, WORKER_DATA_CHUNK_SIZE
      data_entry(j) = compute_data_val(node_id, node_rank, i + j)
    end do
    num_spaces = buffer % atomic_put(data_entry, WORKER_DATA_CHUNK_SIZE)
  end do

  call MPI_Win_free(window, error)

end subroutine run_worker_process



