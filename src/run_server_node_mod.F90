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

module run_server_node_module
  use distributed_circular_buffer_module
  use ioserver_constants
  use ioserver_context_module
  use ioserver_message_module
  use server_stream_module
  implicit none
  private

  public :: run_server_node

contains

subroutine run_server_node(num_server_bound, num_channels, num_noop, use_debug_mode_in)
  implicit none
  integer, intent(in) :: num_server_bound, num_channels, num_noop
  logical, optional, intent(in) :: use_debug_mode_in
  
  type(ioserver_context) :: context
  logical :: success
  logical :: use_debug_mode

  use_debug_mode = .false.
  if (present(use_debug_mode_in)) use_debug_mode = use_debug_mode_in

  success = context % init(.true., 0, num_server_bound, num_channels, num_noop, in_debug_mode = use_debug_mode)

  if (.not. success) then
    print *, 'ERROR, could not initialize IO-server context for server process!'
    error stop 1
  end if

  if (context % is_channel()) then
    call channel_process(context)
  else if (context % is_server_bound()) then
    call server_bound_server_process(context, use_debug_mode)
  else if (context % is_model_bound()) then
    call model_bound_server_process(context)
  else
    print *, 'ERROR, server process is neither server-bound nor model-bound.'
    error stop 1
  end if

  call context % finalize()

end subroutine run_server_node

subroutine server_bound_server_process(context, use_debug_mode_in)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical, optional,      intent(in)    :: use_debug_mode_in

  logical :: use_debug_mode

  type(comm_rank_size)              :: consumer_crs
  type(distributed_circular_buffer) :: data_buffer
  type(local_server_stream), pointer :: file_ptr

  integer :: server_id, num_consumers, num_producers
  logical, dimension(:), allocatable :: active_producers
  logical :: server_finished, producer_finished, success
  integer :: i_producer, producer_id, i_file
  integer :: num_errors

  num_errors = 0
  use_debug_mode = .false.
  if (present(use_debug_mode_in)) use_debug_mode = use_debug_mode_in

  ! Prepare reception of messages
  data_buffer = context % get_dcb()
  consumer_crs = context % get_crs(SERVER_COLOR + NODE_COLOR)

  if (data_buffer % get_server_bound_server_id() < 0) then
    print *, 'ERROR, server process does not seem to be a server within the DCB...'
    error stop 1
  end if

  server_id     = data_buffer % get_server_bound_server_id()
  num_consumers = data_buffer % get_num_server_consumers()
  num_producers = data_buffer % get_num_server_bound_clients()

  allocate(active_producers(0:num_producers - 1))
  active_producers(:) = .true.

  ! Now, we repeatedly loop through all buffers this consumer is responsible for
  ! If the buffer is empty, just go on to the next
  ! If the buffer has something, the first value is the size of the data block to read, so we read it
  ! If the first value to read is 0, it means this buffer is done and won't send anything anymore
  ! When all buffer have a first value of 0, the entire test is finished
  server_finished = .false.
  do while (.not. server_finished)
    server_finished = .true.
    ! do i_producer = server_id + 1, num_producers, num_consumers
    do i_producer = server_id, num_producers - 1, num_consumers
      ! producer_id = active_producers(i_producer)
      producer_id = i_producer
      if (.not. active_producers(i_producer)) cycle

      producer_finished = receive_message(context, data_buffer, producer_id)
      if (producer_finished) then
        active_producers(i_producer) = .false.
        print *, 'Producer has sent the STOP signal', producer_id
      else
        server_finished = .false.
      end if
    end do
  end do
  print *, 'Server done receiving. Will now close owned files', server_id

  do i_file = 1, MAX_NUM_SERVER_STREAMS
    file_ptr => context % get_stream(i_file)
    success = context % close_stream_server(i_file, .true.)
    if (.not. success) then
      print *, 'ERROR: Unable to close server stream ', i_file, server_id
      error stop 1
    end if
  end do

  ! Final check on the buffers' content
  if (use_debug_mode) then
    do i_producer = server_id, num_producers - 1, num_consumers
      if (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) .ne. 0) then
        num_errors = num_errors + 1 
        write (6, '(A, I8, I3, I3)') 'ERROR: buffer should be empty at the end of test', data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4), i_producer, server_id
        call data_buffer % print(.true.)
      end if
    end do
    if (num_errors > 0) error stop 1
  end if
end subroutine server_bound_server_process

subroutine model_bound_server_process(context)
  implicit none
  type(ioserver_context), intent(inout) :: context

  if (context % has_debug_mode()) print *, 'Model-bound server process'
end subroutine model_bound_server_process

subroutine channel_process(context)
  implicit none
  type(ioserver_context), intent(inout) :: context

  type(distributed_circular_buffer) :: dcb
  logical :: success

  dcb = context % get_dcb()
  if (dcb % get_channel_id() >= 0) then
    success = dcb % start_listening()
  else
    print *, 'ERROR: Channel process does not seem to be a channel within the DCB'
  end if
end subroutine channel_process

function receive_message(context, dcb, client_id) result(finished)
  use ioserver_data_check_module
  implicit none
  type(ioserver_context),            intent(inout) :: context
  type(distributed_circular_buffer), intent(inout) :: dcb
  integer,                           intent(in) :: client_id
  logical :: finished

  logical :: success

  ! File management
  character(len=:), allocatable      :: filename
  type(local_server_stream), pointer :: stream_ptr
  integer                            :: i_file, num_flushed

  ! Message reading
  integer(C_INT64_T)   :: capacity
  integer              :: header_tag
  type(message_header) :: header
  type(message_cap)    :: end_cap

  ! Data extraction/processing
  type(model_record) :: record
  integer(C_INT64_T) :: num_data
  integer(kind = 8), dimension(:), pointer, contiguous, save :: model_data => NULL()

  integer :: consumer_id

  ! data_heap = context % get_node_heap()
  capacity = dcb % get_capacity(client_id, CB_KIND_INTEGER_8)
  consumer_id = dcb % get_server_bound_server_id()

  ! print '(A, I3, A I4)', 'Server ', consumer_id, ' receiving message from client ', client_id

  if (.not. associated(model_data)) allocate(model_data(capacity))

  ! print *, 'Receiving a message'
  finished = .false.

  ! Flush any completed grid for owned files
  do i_file = 1, MAX_NUM_SERVER_STREAMS
    stream_ptr => context % get_stream(i_file)
    num_flushed = stream_ptr % flush_data()
    if (num_flushed > 0) then
      print '(A, I4, A)', 'Flushed ', num_flushed, ' completed grids'
    end if
  end do

  success = dcb % peek_elems(client_id, header_tag, 1_8, CB_KIND_INTEGER_4)

  if (.not. success) then
    print *, 'Error after peeking into DCB'
    error stop 1
  end if

  if (header_tag .ne. MSG_HEADER_TAG) then
    print *, 'ERROR: Message header tag is wrong', header_tag, MSG_HEADER_TAG
    error stop 1
  end if

  success = dcb % get_elems(client_id, header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)

  if (.not. success) then
    print *, 'ERROR getting message header from DCB', consumer_id
    error stop 1
  end if

  ! call print_message_header(header)

  if (header % content_length > capacity) then
    print *, 'Message is larger than what we can deal with. That is problematic.'
    print *, 'Message size:  ', header % content_length
    print *, 'capacity     = ', capacity
    print *, 'client id    = ', client_id
    error stop 1
  end if

  !-------
  ! Data
  if (header % command == MSG_COMMAND_DATA) then
    ! print *, 'Got DATA message', consumer_id
    success = dcb % get_elems(client_id, record, model_record_size_int(), CB_KIND_INTEGER_4, .true.)
    if (.not. success) then
      print *, 'Error reading record'
      error stop 1
    end if

    ! if (record % tag == 2) then
    !   call print_model_record(record)
    ! end if

    ! TODO manage compression + other metadata

    num_data = (record % data_size_byte + 7) / 8
    success = dcb % get_elems(client_id, model_data, num_data, CB_KIND_INTEGER_8, .true.) ! Extract data

    stream_ptr => context % get_stream(header % stream_id)   ! Retrieve stream ptr
    success = stream_ptr % put_data(record, model_data)      ! Put data in its proper place within a global grid
    if (.not. success) then
      print *, 'ERROR: Could not put data into partial grid!'
      error stop 1
    end if

  !---------------
  ! Open a file
  else if (header % command == MSG_COMMAND_OPEN_FILE) then
    allocate(character(len=(header % content_length)) :: filename)
    ! print *, 'Got OPEN message', consumer_id
    success = dcb % get_elems(client_id, filename, INT(header % content_length, kind=8), CB_KIND_CHAR, .true.)
    ! print *, 'Opening a file named ', filename
    stream_ptr => context % open_stream_server(filename, header % stream_id)
    if (.not. stream_ptr % is_open()) then
      if (stream_ptr % is_owner()) then
        print *, 'Failed (?) to open file ', filename
        error stop 1
      else
        ! print *, "DEBUG: File is not open, be we're not the owner, so it's OK"
      end if
    end if

  !----------------
  ! Close a file
  else if (header % command == MSG_COMMAND_CLOSE_FILE) then
    ! print *, 'Got CLOSE FILE message', consumer_id
    success = context % close_stream_server(header % stream_id, .false.)
    if (.not. success) then
      stream_ptr => context % get_stream(header % stream_id)
      if (stream_ptr % is_owner()) then
        print *, 'DEBUG: could not close file at this time', header % stream_id, consumer_id
        ! error stop 1
      end if
    end if

  !----------------
  ! Misc. message
  else if (header % command == MSG_COMMAND_DUMMY) then
    print *, 'Got a DUMMY message!', consumer_id

  !---------------------------------
  ! Stop receiving from this relay
  else if (header % command == MSG_COMMAND_RELAY_STOP) then
    print *, 'Got a RELAY STOP message', consumer_id
    finished = .true.

    ! if (associated(model_data)) then
    !   deallocate(model_data)
    !   nullify(model_data)
    ! end if
    ! if (associated(expected_data)) then
    !   deallocate(expected_data)
    !   nullify(expected_data)
    ! end if

  else if (header % command == MSG_COMMAND_MODEL_STOP) then
    ! print *, 'Got a MODEL STOP message', consumer_id

  !------------
  ! Big no-no
  else
    print *, 'ERROR Unhandled message type!', header % command
    error stop 1
  end if

  success = dcb % get_elems(client_id, end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.)

  if ((.not. success) .or. (end_cap % msg_length .ne. header % content_length) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
    print *, 'Discrepancy between message length and end cap', header % content_length, end_cap % msg_length, end_cap % cap_tag
    call print_message_header(header)
    ! call dcb % print(.true.)
    error stop 1
  end if

end function receive_message

end module run_server_node_module
