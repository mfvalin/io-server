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

module default_model_module
  implicit none
  contains

  function am_server_node(node_rank, node_size, single_node)
    use mpi_f08
    implicit none

    integer, intent(out) :: node_rank, node_size
    logical, intent(out) :: single_node
    logical :: am_server_node

    type(MPI_Comm) :: node_comm
    integer :: global_rank, node_root_global_rank
    integer :: global_size

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm)
    call MPI_Comm_rank(node_comm, node_rank)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.

    call MPI_Comm_size(MPI_COMM_WORLD, global_size)
    call MPI_Comm_size(node_comm, node_size)

    single_node = .false.
    if (global_size == node_size) single_node = .true.
  end function am_server_node
end module default_model_module

program default_model
  use default_model_module
  use ioserver_run_module
  implicit none

  integer :: status

  logical :: server_node, single_node
  integer :: node_rank, node_size

  integer :: num_relay_per_node, num_noop, num_server_processes, num_channels

  call mpi_init(status)

  server_node = am_server_node(node_rank, node_size, single_node)

  num_channels         = 2
  num_server_processes = num_channels + 2
  num_relay_per_node   = 2

  num_noop = 0
  if (.not. single_node) num_noop = node_size - num_server_processes

  if (.not. server_node) then
    call ioserver_run_model_node(num_relay_per_node, use_debug_mode_in = .true.)
  else
    if (node_rank < num_server_processes) then
      call ioserver_run_server_node(num_server_processes - num_channels, num_channels, num_noop, use_debug_mode_in = .true.)
    else 
      if (single_node) then
        call ioserver_run_model_node(num_relay_per_node, use_debug_mode_in = .true.)
      else
        call ioserver_run_server_node(num_server_processes - num_channels, num_channels, num_noop, use_debug_mode_in = .true.)
      end if
    end if

  endif
  call mpi_finalize(status)
end program default_model