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

program launch_server
  use ioserver_mpi
  use ioserver_run_module

  type(ioserver_input_parameters) :: params
  logical :: success
  integer :: ierr
  integer :: rank

  integer            :: iarg, num_args
  character(len=128) :: arg
  character(len=:), allocatable :: trimmed_arg

  params % is_on_server             = .true.
  params % num_server_bound_server  = 2
  params % num_grid_processors      = 1
  params % num_channels             = 2
  params % debug_level              = 1

  ! Parse command line inputs and initialize IO-server input parameters
  num_args = command_argument_count()
  iarg = 1
  do while (iarg <= num_args)
    call get_command_argument(iarg, arg)
    iarg = iarg + 1

    trimmed_arg = trim(arg)

    if (trimmed_arg == '--num-server-bound') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % num_server_bound_server
    else if (trimmed_arg == '--num-model-bound') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % num_model_bound_server
    else if (trimmed_arg == '--num-grid-processors') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % num_grid_processors
    else if (trimmed_arg == '--num-channels') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % num_channels
    else if (trimmed_arg == '--max-streams') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % max_num_concurrent_streams
    else if (trimmed_arg == '--debug-level') then
      call get_command_argument(iarg, arg)
      iarg = iarg + 1
      read(arg, *) params % debug_level
    else
      print *, 'got something different: ', trimmed_arg
    end if
  end do

  call MPI_Init(ierr)

  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  if (rank == 0) then
    call params % print()
  end if

  success = ioserver_run_server_node(params)

  if (.not. success) then
    print *, 'ERROR while running the IO server'
    error stop 1 
  end if

  call MPI_Finalize(ierr)

  if (.not. success) error stop 1
end program launch_server
