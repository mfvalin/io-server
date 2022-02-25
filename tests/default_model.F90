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
  use ioserver_mpi
  implicit none
  contains

  function am_server_node(node_rank, node_size, single_node)
    implicit none

    integer, intent(out) :: node_rank, node_size
    logical, intent(out) :: single_node
    logical :: am_server_node

    integer :: node_comm
    integer :: global_rank, node_root_global_rank
    integer :: global_size
    integer :: ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm, ierr)
    call MPI_Comm_rank(node_comm, node_rank, ierr)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm, ierr)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.

    call MPI_Comm_size(MPI_COMM_WORLD, global_size, ierr)
    call MPI_Comm_size(node_comm, node_size, ierr)

    single_node = .false.
    if (global_size == node_size) single_node = .true.
  end function am_server_node
end module default_model_module

program default_model
  use default_model_module
  use ioserver_run_module
  implicit none

  logical :: server_node, single_node, success
  integer :: node_rank, node_size, num_server_processes, global_size
  integer :: ierr

  type(ioserver_input_parameters) :: params

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, global_size, ierr)

  if (global_size < 9) then
    print *, 'ERROR: Need at least 9 processes to run this test! Only have ', global_size
    error stop 1
  end if

  params % debug_mode = .true.

  server_node = am_server_node(node_rank, node_size, single_node)

  params % num_channels = 2
  params % num_server_bound_server = 2
  params % num_relay_per_node = 2
  params % num_grid_processors = 1

  num_server_processes = params % num_channels + params % num_server_bound_server + params % num_grid_processors

  if (server_node) then
    if (.not. single_node .or. node_rank < num_server_processes) params % is_on_server = .true.
  end if

  if (params % is_on_server) then
      success = ioserver_run_server_node(params)
  else
    success = ioserver_run_model_node(params)
  end if

  if (.not. success) then
    print *, 'ERROR while trying to run the default IO server'
    error stop 1
  end if

  call MPI_Finalize(ierr)

end program default_model