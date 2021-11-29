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

module model_write_parameters
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
end module model_write_parameters

#include <serializer.hf>

program pseudomodelandserver
  use ISO_C_BINDING
  use mpi_f08

  use ioserver_constants
  use ioserver_run_module
  use model_write_parameters
  implicit none

  integer :: status, input
  integer :: nio_node, num_channels

  integer :: nserv !, noops
  character(len=128) :: arg
  type(comm_rank_size) :: local_crs

  logical :: server_node, single_node
  integer :: node_rank, node_size

  integer :: num_relay_per_node, num_noop

  logical :: debug_mode
  logical :: CHECK_CB_MESSAGES
  logical :: CHECK_DCB_MESSAGES

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

  debug_mode = .false.
  ! debug_mode = .true. ! activate debug mode

  ! Arguments
  ! 1. Check messages or not
  ! 2. Number of active server processes
  ! 3. Number of channel processes
  ! 4. Number of relay processes per node

  ! Arg 1
  CHECK_CB_MESSAGES = .false.
  CHECK_DCB_MESSAGES = .false.
  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg, *) input
  if (input > 0) then
    CHECK_CB_MESSAGES = .true.
    CHECK_DCB_MESSAGES = .true.
  end if

  ! Arg 2
  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)
  read(arg,*) nserv
  nserv = nserv

  ! Arg 3
  arg = '1'
  if (COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg, *) num_channels
  nserv = nserv + num_channels

  ! Arg 4
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 4) call GET_COMMAND_ARGUMENT(4, arg)
  read(arg,*) nio_node                    ! number of relay processes per node

  server_node = am_server_node(node_rank, node_size, single_node)

  num_relay_per_node = nio_node
  num_noop = 0
  if (.not. single_node) then
    num_noop = node_size - nserv
  end if

  ! if(rank >= nserv) then
  ! if(mod(rank, NUM_NODES) .ne. 0) then
  if (.not. server_node) then
    call ioserver_run_model_node(num_relay_per_node, do_expensive_checks_in = CHECK_CB_MESSAGES)
  else
    if (node_rank < nserv) then
      call ioserver_run_server_node(nserv - num_channels, num_channels, num_noop, do_expensive_checks_in = CHECK_DCB_MESSAGES)
    else 
      if (single_node) then
        call ioserver_run_model_node(num_relay_per_node, do_expensive_checks_in = CHECK_CB_MESSAGES)
      else
        call ioserver_run_server_node(nserv - num_channels, num_channels, num_noop, do_expensive_checks_in = CHECK_DCB_MESSAGES)
      end if
    end if

  endif
  call mpi_finalize(status)
end program
