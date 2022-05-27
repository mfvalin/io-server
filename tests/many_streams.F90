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
  use ioserver_run_module
  implicit none

  type :: stream_pointer
    type(model_stream), pointer :: p
  end type stream_pointer

contains
  function relay_pe(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success

    success = .false.
    if (context % is_server_bound()) then
      success = ioserver_server_bound_relay(context)
    else if (context % is_model_bound()) then
      success = ioserver_model_bound_relay(context)
    end if
  end function relay_pe

  function model_pe(context) result(model_success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: model_success
    
    integer :: max_num_streams
    type(stream_pointer), dimension(:), allocatable :: streams
    type(model_stream), pointer :: additional_stream
    integer :: i_stream, i_open
    logical :: success
    
    model_success = .false.
    
    max_num_streams = context % get_max_num_streams()
    allocate(streams(max_num_streams))

    do i_stream = 1, max_num_streams
      call context % open_stream_model(streams(i_stream) % p)
      if (.not. associated(streams(i_stream) % p)) then
        print '(A, I5)', 'ERROR: Unable to open stream ', i_stream
        return
      end if
    end do

    call context % open_stream_model(additional_stream)
    if (associated(additional_stream)) then
      print '(A)', 'ERROR: Should not have been able to open another stream!'
      return
    end if

    do i_open = 0, 200000 - 1 
      i_stream = mod(i_open, max_num_streams / 2) + 1
      success = streams(i_stream) % p % close()
      if (.not. success) then
        print '(A, I5, A, I5)', 'ERROR: Unable to close stream ', i_open, ', rank ', i_stream
        return
      end if

      call context % open_stream_model(streams(i_stream) % p)
      if (.not. associated(streams(i_stream) % p)) then
        print '(A, I5, A, I5)', 'ERROR: Unable to open stream ', i_open, ', rank ', i_stream
        return
      end if
    end do

    do i_stream = 1, max_num_streams
      success = streams(i_stream) % p % close()
      if (.not. success) then
        print '(A, I5)', 'ERROR: Unable to close stream rank ', i_stream
        return
      end if
    end do

    deallocate(streams)
    model_success = .true.
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
  ! params % debug_level        = 1

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

  if (.not. success) then
    print *, 'ERROR: Running PE'
    error stop 1
  end if

  call context % finalize()
  call MPI_Finalize(ierr)

end program many_streams
