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
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

module many_streams_module
  use ioserver_context_module
  implicit none

contains
  function relay_pe(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    print *, 'I am relay'
    success = .true.
  end function relay_pe

  function model_pe(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    print *, 'I am model'
    success = .true.
  end function model_pe


end module many_streams_module

program many_streams
  use many_streams_module
  implicit none

  type(ioserver_context)          :: context
  type(ioserver_input_parameters) :: params
  logical :: success

  integer :: ierr

  call MPI_Init(ierr)

  params % num_relay_per_node = 2
  params % is_on_server       = .false.
  params % debug_level        = 1

  success = context % init(params)

  if (.not. success) then
    print *, 'ERROR: io-server context did not initialize properly'
    error stop 1
  end if

  success = .false.
  if (context % is_relay()) then
    success = relay_pe(context)
  else if (context % is_model()) then
    success = model_pe(context)
  end if 

  call context % finalize()
  call MPI_Finalize(ierr)

end program many_streams
