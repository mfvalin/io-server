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
  use ioserver_mpi

  use shmem_heap_module
  use ioserver_context_module
  use ioserver_message_module
  use ioserver_run_module
  use jar_module
  use process_command_module
  implicit none

#include <serializer.hf>

  type(ioserver_context)          :: context
  type(ioserver_input_parameters) :: params
  logical :: success

  type(model_stream), pointer :: stream

  type(command_header) :: header
  type(jar) :: command

  type(comm_rank_size) :: model_crs

  character(len=32) :: file_name

  integer :: ierr
  integer :: i

  call MPI_Init(ierr)

  params % num_relay_per_node = 3
  params % is_on_server       = .false.
  params % debug_level        = 1

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

  model_crs = context % get_crs(MODEL_COLOR)

  call context % open_stream_model(stream)
  if (.not. associated(stream)) then
    print *, 'ERROR: Could not open model stream'
    error stop 1
  end if

  success = command % new(20)
  file_name = ''
  do i = 1, 1


    !---------------------
    ! Open file command
    call command % reset()
    write(file_name, '(A6, I3.3)') 'blah_f', i
    
    header % command_type = COMMAND_TYPE_OPEN_FILE
    header % size_bytes   = len_trim(file_name)

    ! print *, 'MODEL into jar'
    success = JAR_PUT_ITEM(command, header) .and. success
    success = JAR_PUT_ITEM(command, trim(file_name)) .and. success
    ! print *, 'MODEL into jar done'
    if (.not. success) then 
      print *, 'ERROR: could not put stuff in command jar!'
      error stop 1
    end if

    success = stream % send_command(command)
    if (.not. success) then
      print *, 'ERROR: Failed to send open file command'
      error stop 1
    end if

    !--------------
    ! Send data
    block
      type(shmem_heap)     :: data_heap
      type(subgrid_t)      :: local_grid
      type(grid_t)         :: input_grid, output_grid
      type(block_meta_f08) :: array_info
      type(model_grid)     :: m_grid

      integer(kind=8), dimension(:), pointer :: model_data

      data_heap = context % get_local_heap()


      local_grid % offset(1) = model_crs % rank + 1 ! Model PEs ordered in a a line, ordered by rank
      local_grid % size(:) = 1

      input_grid % id = 1
      input_grid % size(1) = model_crs % size  ! Line, along first dimension
      input_grid % elem_size = 8

      output_grid % id = 1

      ! Generate data to send
      array_info = data_heap % allocate(model_data, [1_8])
      if (.not. associated(model_data)) then
        print *, 'ERROR: Could not allocated data from model heap'
        error stop 1
      end if
      model_data(1) = model_crs % rank

      ! Create a command for what to do with the data
      call command % reset()

      ! Command header
      header % command_type = COMMAND_TYPE_WRITE_DATA
      header % size_bytes   = (storage_size(m_grid) + 7) / 8
      success = JAR_PUT_ITEM(command, header)

      ! Grid metadata
      m_grid % dims      = input_grid % size
      m_grid % elem_size = input_grid % elem_size
      success = JAR_PUT_ITEM(command, m_grid) .and. success

      ! Send the data + command
      success = stream % send_data(array_info, local_grid, input_grid, output_grid, command = command) .and. success
      if (.not. success) then
        print *, 'Unable to send data!!!'
        error stop 1
      end if

      ! Send a second set of data, to be displayed
      array_info = data_heap % allocate(model_data, [1_8])  ! Don't forget to allocate new space for that data
      model_data(1) = model_crs % rank * 2
      header % command_type = COMMAND_TYPE_WRITE_DISPLAY_DATA
      call command % reset()
      success = JAR_PUT_ITEM(command, header)
      success = JAR_PUT_ITEM(command, m_grid) .and. success
      success = stream % send_data(array_info, local_grid, input_grid, output_grid, command = command) .and. success

      if (.not. success) then
        print *, 'ERROR: Unable to send second set of data'
        error stop 1
      end if
    end block

    !---------------------
    ! Close file command
    header % command_type = COMMAND_TYPE_CLOSE_FILE
    header % size_bytes = 0

    call command % reset()
    success = JAR_PUT_ITEM(command, header) .and. success
    if (.not. success) then 
      print *, 'ERROR: could not put stuff in command jar!'
      error stop 1
    end if

    success = stream % send_command(command)
    if (.not. success) then
      print *, 'ERROR: Failed to send close file command'
      error stop 1
    end if

  end do

  success = stream % close()
  if (.not. success) then
    print *, 'ERROR: Unable to close stream'
    error stop 1
  end if

  ! print *, 'Finishing model'
  call context % finalize()
  call MPI_Finalize(ierr)

end program model_integration
