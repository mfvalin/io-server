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

program model_integration
  use ioserver_context_module
  use ioserver_run_module
  use ioserver_mpi
  use jar_module
  implicit none

#include <serializer.hf>

  type(ioserver_context)          :: context
  type(ioserver_input_parameters) :: params
  logical :: success

  type(model_stream), pointer :: stream
  type(jar) :: command
  integer(JAR_ELEMENT) :: num_elem
  character(len=1), dimension(9) :: file_name
  character(len=32) :: file_name_char
  integer :: jar_ok

  integer :: ierr
  integer :: i

  call MPI_Init(ierr)

  params % num_relay_per_node = 3
  params % is_on_server       = .false.
  params % debug_mode         = .true.

  success = context % init(params)

  if (.not. success) then
    print *, 'ERROR: io-server context did not initialize properly'
    error stop 1
  end if

  !------------------------
  ! Relay stuff
  if (context % is_relay()) then
    if (context % is_server_bound()) then
      success = ioserver_server_bound_relay(context)
    else if (context % is_model_bound()) then
      success = ioserver_model_bound_relay(context)
    else
      print *, 'ERROR: Bad PE type'
      error stop 1
    end if

    if (.not. success) then
      print *, 'ERROR: Relay did not terminate successfully'
      error stop 1
    end if

    ! print *, 'Finishing relay'
    call context % finalize()
    call MPI_Finalize(ierr)
    stop
  end if
  ! Relay is done now, everything else is model stuff
  !-----------------------------------------------------

  call context % open_stream_model('test_file', stream)

  jar_ok = command % new(20)
  do i = 1, 50
    call command % reset
    write(file_name_char, '(A6, I3.3)') 'test_f', i
    ! file_name_char = 'test_file'
    file_name(1:9) = transfer(file_name_char, file_name)

    num_elem = JAR_PUT_ITEMS(command, file_name)
    if (num_elem <= 0) then 
      print *, 'ERROR: could not put stuff in command jar!'
      error stop 1
    end if
    success = stream % send_command(command)
  end do

  ! print *, 'Finishing model'
  call context % finalize()
  call MPI_Finalize(ierr)

end program model_integration
