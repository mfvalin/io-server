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
module launch_pseudo_model_module
  implicit none
contains
function model_work(context) result(success)
  use ioserver_context_module
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical :: success
  
  type(comm_rank_size) :: model_crs

  model_crs = context % get_crs(MODEL_COLOR)
  print *, 'I am a pseudo model who does nothing', model_crs % rank

  success = .true.

end function model_work
end module launch_pseudo_model_module

program launch_model
  use ioserver_mpi_f08
  use ioserver_run_module
  use launch_pseudo_model_module
  implicit none

  type(ioserver_input_parameters) :: params
  logical :: success

  procedure(model_function_template), pointer :: model_fn_ptr

  call MPI_Init()

  model_fn_ptr => model_work

  params % is_on_server             = .false.
  params % num_relay_per_node       = 2 ! Should not be important for the server
  ! params % num_server_bound_server  = -1
  ! params % num_grid_processors      = -1
  ! params % num_channels             = -1
  params % debug_mode = .true.

  success = ioserver_run_model_node(params, model_function = model_fn_ptr)

  if (.not. success) then
    print *, 'ERROR while running the pseudo-model'
    ! error stop 1 
  end if
  call MPI_Finalize()
end program launch_model
