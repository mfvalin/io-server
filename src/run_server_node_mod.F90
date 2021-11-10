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

subroutine run_server_node(num_channels, num_noop, do_expensive_checks_in)
  implicit none
  integer, intent(in) :: num_channels, num_noop
  logical, optional, intent(in) :: do_expensive_checks_in
  
  type(ioserver_context) :: context
  logical :: success
  logical :: do_expensive_checks

  do_expensive_checks = .false.
  if (present(do_expensive_checks_in)) do_expensive_checks = do_expensive_checks_in

  success = context % init(.true., 0, num_channels, num_noop, in_debug_mode = do_expensive_checks)

  if (.not. success) then
    print *, 'ERROR, could not initialize IO-server context for server process!'
    error stop 1
  end if

  if (context % is_channel()) then
    call channel_process(context)
  else if (context % is_server_bound()) then
    call server_bound_server_process(context, do_expensive_checks_in = do_expensive_checks)
  else if (context % is_model_bound()) then
    call model_bound_server_process(context)
  else
    print *, 'ERROR, server process is neither server-bound nor model-bound.'
    error stop 1
  end if

  call context % finalize()

end subroutine run_server_node

subroutine server_bound_server_process(context, do_expensive_checks_in)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical, optional,      intent(in)    :: do_expensive_checks_in

  logical :: do_expensive_checks

  type(comm_rank_size)              :: consumer_crs
  type(distributed_circular_buffer) :: data_buffer

  integer :: server_id, num_consumers, num_producers
  logical, dimension(:), allocatable :: active_producers
  logical :: server_finished, producer_finished
  integer :: i_producer, producer_id
  integer :: num_errors

  num_errors = 0
  do_expensive_checks = .false.
  if (present(do_expensive_checks_in)) do_expensive_checks = do_expensive_checks_in

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

      producer_finished = receive_message(context, data_buffer, producer_id, do_expensive_checks)
      if (producer_finished) then
        active_producers(i_producer) = .false.
        print *, 'Producer has sent the STOP signal', producer_id
      else
        server_finished = .false.
      end if
    end do
  end do
  print *, 'Server done receiving', server_id

  ! Final check on the buffers' content
  if (do_expensive_checks) then
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

  print *, 'Model-bound server process'
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

function receive_message(context, dcb, client_id, verify_message) result(finished)
  use ioserver_data_check_module
  use ioserver_memory_mod
  implicit none
  type(ioserver_context),            intent(inout) :: context
  type(distributed_circular_buffer), intent(inout) :: dcb
  integer,                           intent(in) :: client_id
  logical,                           intent(in) :: verify_message
  logical :: finished

  logical :: success

  ! File management
  character(len=:), allocatable :: filename
  type(server_stream), pointer  :: file_ptr
  integer                       :: i_file, num_flushed

  ! Message reading
  integer(C_INT64_T)   :: capacity
  integer              :: message_size, end_cap
  type(message_header) :: header

  ! Data extraction/processing
  type(model_record) :: record
  integer(C_INT64_T) :: num_data
  integer            :: i_data_check ! Only for verifying data
  integer, dimension(:),    pointer, contiguous, save :: model_data => NULL(), expected_data => NULL()
  integer, dimension(:, :), pointer, contiguous       :: data_ptr


  capacity = dcb % get_capacity(client_id, CB_KIND_INTEGER_4)

  if (.not. associated(model_data)) allocate(model_data(capacity))
  if (.not. associated(expected_data)) allocate(expected_data(capacity))

  ! print *, 'Receiving a message'
  finished = .false.

  ! Flush any completed grid for owned files
  do i_file = 1, MAX_NUM_SERVER_STREAMS
    file_ptr => context % get_stream(i_file)
    if (file_ptr % get_owner_id() == dcb % get_server_bound_server_id()) then
      num_flushed = file_ptr % flush_data()
      if (num_flushed > 0) then
        print '(A, I4, A)', 'Flushed ', num_flushed, ' completed grids'
      end if
    end if
  end do

  success = dcb % peek_elems(client_id, message_size, 1_8, CB_KIND_INTEGER_4)

  if (.not. success) then
    print *, 'Error after peeking into DCB'
    error stop 1
  end if

  if (message_size > capacity) then
    print *, 'Message is larger than what we can deal with. That is problematic.'
    print *, 'Message size:  ', message_size
    print *, 'capacity     = ', capacity
    print *, 'client id    = ', client_id
    error stop 1
  end if

  success = dcb % get_elems(client_id, header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)

  if (.not. success) then
    print *, 'ERROR getting message header from DCB'
    error stop 1
  end if

  ! call message_header_print(header)

  !-------
  ! Data
  if (header % command == MSG_COMMAND_DATA) then
    print *, 'Got DATA message'
    success = dcb % get_elems(client_id, record, model_record_size_int(), CB_KIND_INTEGER_4, .true.)
    if (.not. success) then
      print *, 'Error reading record'
      error stop 1
    end if

    ! TODO manage compression + other metadata

    num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size (tkr) into account
    success = dcb % get_elems(client_id, model_data, num_data, CB_KIND_INTEGER_4, .true.)

    if (verify_message) then
      do i_data_check = 1, int(num_data, 4)
          expected_data(i_data_check) = compute_data_point(header % sender_global_rank, record % tag, i_data_check)
      end do
      if (.not. all(expected_data(1:num_data) == model_data(1:num_data))) then
        print *, 'Expected: ', expected_data(1:num_data)
        print *, 'Received: ', model_data(1:num_data)
        error stop 1
      end if
    end if

    data_ptr(1:record % ni, 1:record % nj) => model_data
    file_ptr => context % get_stream(header % stream_id)
    success = file_ptr % put_data(record, data_ptr)
    if (.not. success) then
      print *, 'ERROR: Could not put data into partial grid!'
      error stop 1
    end if

  !---------------
  ! Open a file
  else if (header % command == MSG_COMMAND_OPEN_FILE) then
    allocate(character(len=(header % length)) :: filename)
    print *, 'Got OPEN message'
    success = dcb % get_elems(client_id, filename, INT(header % length, kind=8), CB_KIND_CHAR, .true.)
    print *, 'Opening a file named ', filename
    file_ptr => context % open_file_server(filename, header % stream_id)
    if (.not. file_ptr % is_open()) then
      print *, 'Failed to open file ', filename
      error stop 1
    end if

  !----------------
  ! Close a file
  else if (header % command == MSG_COMMAND_CLOSE_FILE) then
    print *, 'Got CLOSE FILE message'
    success = context % close_file_server(header % stream_id)
    if (.not. success) then
      print *, 'ERROR, could not close file', header % stream_id
      error stop 1
    end if

  !----------------
  ! Misc. message
  else if (header % command == MSG_COMMAND_DUMMY) then
    print *, 'Got a DUMMY message!'

  !---------------------------------
  ! Stop receiving from this relay
  else if (header % command == MSG_COMMAND_RELAY_STOP) then
    print *, 'Got a RELAY STOP message'
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
    print *, 'Got a MODEL STOP message'

  !------------
  ! Big no-no
  else
    print *, 'ERROR Unhandled message type!', header % command
    error stop 1
  end if

  success = dcb % get_elems(client_id, end_cap, 1_8, CB_KIND_INTEGER_4, .true.)
  if (end_cap .ne. header % length) then
    print *, 'Discrepancy between message length and end cap', header % length, end_cap
    error stop 1
  end if

end function receive_message

end module run_server_node_module
