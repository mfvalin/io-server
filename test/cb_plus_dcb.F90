! Copyright (C) 2020 Recherche en Prevision Numerique
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
  integer :: node_id, num_worker_nodes, num_channels
  logical :: is_io, is_relay, is_worker
  integer :: ierr

  integer :: tmp_errors, num_errors = 0

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)

  if (num_procs .lt. 6) then
    print *, 'Wait, this test program needs at least 6 processes to run! (2 IO, and 2+ worker nodes with 2+ processes each)'
    num_errors = num_errors + 1
    goto 999
  end if

  call init_comms(global_rank, num_procs, node_comm, io_comm, node_id, num_worker_nodes, num_channels, is_io, is_relay, is_worker)

  if (is_io) then
    call run_io_server(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)
  else if (is_relay) then
    call run_relay_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)
  else if (is_worker) then
    call run_worker_process()
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

subroutine init_comms(global_rank, num_procs, node_comm, io_comm, node_id, num_worker_nodes, num_channels, is_io, is_relay, is_worker)
  implicit none
  include 'mpif.h'

  integer, intent(IN) :: global_rank
  integer, intent(IN) :: num_procs
  integer, intent(out) :: node_comm
  integer, intent(out) :: io_comm
  integer, intent(out) :: node_id
  integer, intent(out) :: num_worker_nodes, num_channels
  logical, intent(out) :: is_io, is_relay, is_worker

  integer :: num_worker_procs = 0
  integer :: num_total_nodes = 0
  integer :: ierr

  is_io = .false.
  is_relay = .false.
  is_worker = .false.

  num_worker_nodes = -1
  num_channels = -1

  if (num_procs >= 6 .and. num_procs <= 8) then
    print *, 'GOTTA CHECK THAT'
    return
    ! 2 Relays, 2 IO, the rest are workers
    num_worker_nodes = 2
    num_total_nodes = 3
    num_worker_procs = num_procs - 4

    ! First classify the processes, according to their rank
    if (global_rank < 2) then
      is_relay = .true.
    else if (global_rank < 4) then
      is_io = .true.
    else
      is_worker = .true.
    end if

    if (is_io) then
      node_id = 0
    else
      node_id = mod(global_rank, num_worker_nodes) + 1
    end if

!    print *, 'global rank, node id: ', global_rank, node_id

    ! Create the individual node communicators
    call MPI_Comm_split(MPI_COMM_WORLD, node_id, 0, node_comm)

    ! Create the IO communicator
    if (.not. is_worker) then
      call MPI_Comm_split(MPI_COMM_WORLD, 0, 0, io_comm)
    else
      call MPI_Comm_split(MPI_COMM_WORLD, MPI_UNDEFINED, 0, io_comm)
    end if

  else
    print *, 'init_comms NOT implemented for that number of processes...'

  end if



end subroutine init_comms


subroutine run_io_server(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)

  use distributed_circular_buffer_module, only: distributed_circular_buffer
  use parameters

  implicit none

  integer, intent(in)    :: node_comm, io_comm
  integer, intent(in)    :: num_worker_nodes, num_channels
  integer, intent(inout) :: num_errors

  integer :: io_rank, num_io_procs
  integer :: ierr

  type(distributed_circular_buffer) :: io_buffer
  logical :: success

  call MPI_Comm_rank(io_comm, io_rank, ierr)
  call MPI_Comm_size(io_comm, num_io_procs, ierr)

  success = io_buffer % create(io_comm, num_worker_nodes, num_channels, NUM_IO_BUFFER_ELEMENTS)
  call io_buffer % delete()

end subroutine run_io_server

subroutine run_relay_process(node_comm, io_comm, num_worker_nodes, num_channels, num_errors)

  use distributed_circular_buffer_module, only: distributed_circular_buffer

  implicit none

  integer, intent(in)    :: node_comm, io_comm
  integer, intent(in)    :: num_worker_nodes, num_channels
  integer, intent(inout) :: num_errors

  integer :: io_rank, num_io_procs
  integer :: ierr

  type(distributed_circular_buffer) :: io_buffer
  logical :: success

  call MPI_Comm_rank(io_comm, io_rank, ierr)
  call MPI_Comm_size(io_comm, num_io_procs, ierr)

  success = io_buffer % create(io_comm, num_worker_nodes, num_channels, 0)
  call io_buffer % delete()

end subroutine run_relay_process

subroutine run_worker_process()
end subroutine run_worker_process



