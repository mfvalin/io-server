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

  public :: run_server_node, server_function_template

  type, private :: server_receiver_state
    private
    integer :: lowest_tag          = 0
    integer :: previous_lowest_tag = 0
    integer(C_INT64_T), dimension(:), pointer, contiguous :: model_data => NULL()
  end type server_receiver_state

  abstract interface
    function server_function_template(context) result(server_success)
      import ioserver_context
      implicit none
      type(ioserver_context), intent(inout) :: context !< IO server context with which the server process will operate
      logical :: server_success !< Whether the function terminated successfully
    end function server_function_template
  end interface

contains

function run_server_node(params, custom_channel_fn, custom_server_bound_fn, custom_model_bound_fn,    &
  custom_grid_processor_fn, custom_no_op_fn, context_out) result(success)
  implicit none
  type(ioserver_input_parameters), intent(in) :: params
  procedure(server_function_template),  intent(in), pointer, optional :: custom_channel_fn
  procedure(server_function_template),  intent(in), pointer, optional :: custom_server_bound_fn
  procedure(server_function_template),  intent(in), pointer, optional :: custom_model_bound_fn
  procedure(server_function_template),  intent(in), pointer, optional :: custom_grid_processor_fn
  procedure(no_op_function_template),   intent(in), pointer, optional :: custom_no_op_fn
  type(ioserver_context),               intent(out),         optional :: context_out
  logical :: success
  
  type(ioserver_context) :: context
  procedure(server_function_template), pointer :: channel_fn
  procedure(server_function_template), pointer :: receiver_fn
  procedure(server_function_template), pointer :: producer_fn
  procedure(server_function_template), pointer :: grid_processor_fn
  procedure(no_op_function_template),  pointer :: no_op_fn

  success = .false.

  !-------------------------------------
  ! Set up functions to call
  channel_fn        => default_channel
  receiver_fn       => default_server_bound
  producer_fn       => default_model_bound
  grid_processor_fn => default_grid_processor
  no_op_fn          => default_no_op

  if (present(custom_channel_fn)) then
    if (associated(custom_channel_fn)) channel_fn => custom_channel_fn
  end if
  if (present(custom_server_bound_fn)) then
    if (associated(custom_server_bound_fn)) receiver_fn => custom_server_bound_fn
  end if
  if (present(custom_model_bound_fn)) then
    if (associated(custom_model_bound_fn)) producer_fn => custom_model_bound_fn
  end if
  if (present(custom_grid_processor_fn)) then
    if (associated(custom_grid_processor_fn)) grid_processor_fn => custom_grid_processor_fn
  end if
  if (present(custom_no_op_fn)) then
    if (associated(custom_no_op_fn)) no_op_fn => custom_no_op_fn
  end if
  !-------------------------------------

  ! Basic check on input params
  if (.not. params % is_on_server) then
    print *, 'ERROR: Trying to launch the server node, but setting "is_on_server = .false."'
    return
  end if

  ! Context initialization
  success = context % init(params)

  if (.not. success) then
    print *, 'ERROR: Could not initialize IO-server context for server process!'
    return
  end if

  ! Do whatever server PEs are supposed to do, depending on their role
  success = .false.
  if (context % is_no_op()) then
    success = no_op_fn(context)
  else if (context % is_channel()) then
    success = channel_fn(context)
  else if (context % is_server_bound()) then
    success = receiver_fn(context)
  else if (context % is_model_bound()) then
    success = producer_fn(context)
  else if (context % is_grid_processor()) then
    success = grid_processor_fn(context)
  else
    print *, 'ERROR: Server process is not of a known type.'
    return
  end if

  if (.not. success) then
    print *, 'ERROR: Running server process'
    return
  end if

  call context % finalize()

  if (present(context_out)) context_out = context
end function run_server_node

function default_server_bound(context) result(server_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: server_success

  type(comm_rank_size)                :: consumer_crs
  type(distributed_circular_buffer)   :: data_buffer
  type(local_server_stream), pointer  :: file_ptr
  type(server_receiver_state)         :: current_state

  integer :: server_id, num_consumers, num_producers
  logical, dimension(:), allocatable :: active_producers
  logical :: server_finished, producer_finished, success
  integer :: i_producer, producer_id, i_file
  integer :: num_errors, ierr

  num_errors = 0
  server_success = .false.

  ! Prepare reception of messages
  data_buffer = context % get_dcb()
  consumer_crs = context % get_crs(SERVER_COLOR + SERVER_BOUND_COLOR)

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
    current_state % previous_lowest_tag = current_state % lowest_tag
    current_state % lowest_tag = huge(current_state % lowest_tag)
    do i_producer = server_id, num_producers - 1, num_consumers
      ! producer_id = active_producers(i_producer)
      producer_id = i_producer
      if (.not. active_producers(i_producer)) cycle

      producer_finished = receive_message(context, data_buffer, producer_id, current_state)
      if (producer_finished) then
        active_producers(i_producer) = .false.
        if (context % debug_mode()) print '(A, I3, A)', 'DEBUG: Producer ', producer_id, ' has sent the STOP signal'
      else
        server_finished = .false.
      end if
    end do
  end do
  if (context % debug_mode()) print '(A, I3, A)', 'DEBUG: Server ', server_id, ' done receiving. Will now close owned files.'

  do i_file = 1, MAX_NUM_SERVER_STREAMS
    file_ptr => context % get_stream(i_file)
    if (file_ptr % is_open()) then
      success = context % close_stream_server(i_file)
      if (.not. success) then
        print *, 'ERROR: Unable to try closing server stream ', i_file, server_id
        error stop 1
      end if
    end if
  end do

  ! Final check on the buffers' content
  if (context % debug_mode()) then
    do i_producer = server_id, num_producers - 1, num_consumers
      if (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) .ne. 0) then
        num_errors = num_errors + 1 
        write (6, '(A, I8, I3, I3)') 'ERROR: buffer should be empty at the end of test', data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4), i_producer, server_id
        call data_buffer % print(.true.)
      end if
    end do
    if (num_errors > 0) error stop 1
  end if

  ! Wait for all server-bound server processes to finish
  call MPI_Barrier(consumer_crs % comm, ierr)

  server_success = .true.
end function default_server_bound

function  default_model_bound(context) result(server_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: server_success

  server_success = .false.
  if (context % debug_mode()) print *, 'DEBUG: Model-bound server process'
  server_success = .true.
end function default_model_bound

function default_channel(context) result(channel_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: channel_success

  type(distributed_circular_buffer) :: dcb

  channel_success = .false.

  dcb = context % get_dcb()
  if (dcb % get_channel_id() >= 0) then
    channel_success = dcb % start_listening()
  else
    print *, 'ERROR: Channel process does not seem to be a channel within the DCB'
  end if
end function default_channel

function default_grid_processor(context) result(server_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: server_success

  type(local_server_stream), pointer  :: stream_ptr
  integer :: i_stream
  logical :: finished

  type(comm_rank_size) :: grid_proc_crs

  server_success = .false.

  grid_proc_crs = context % get_crs(GRID_PROCESSOR_COLOR)

  if (context % debug_mode()) then
    print '(A, I2, A, I2)', 'DEBUG: Grid processor process rank ', grid_proc_crs % rank, ' of ', grid_proc_crs % size
  end if
  
  finished = .false.
  ! do while (.not. finished)
  do while (.not. context % is_time_to_quit())
    if (context % is_time_to_quit()) finished = .true.
    do i_stream = 1, MAX_NUM_SERVER_STREAMS
      ! print *, 'Stream, grid proc ', i_stream, grid_proc_crs % rank
      stream_ptr => context % get_stream(i_stream)
      if (.not. stream_ptr % process_stream()) finished = .false.
    end do
  end do

  if (context % debug_mode()) then
    print '(A, I2, A, I2)', 'DEBUG: Grid processor process DONE. Rank ', grid_proc_crs % rank, ' of ', grid_proc_crs % size
  end if

  server_success = .true.
end function default_grid_processor

function receive_message(context, dcb, client_id, state) result(finished)
  implicit none
  type(ioserver_context),            intent(inout) :: context   !< IO server context in which we are operating
  type(distributed_circular_buffer), intent(inout) :: dcb       !< Handle to the DCB used for transmission
  integer,                           intent(in)    :: client_id !< ID of the specific client we are receiving from
  type(server_receiver_state),       intent(inout) :: state     !< Current state of the receiver process
  logical :: finished

  logical :: success

  ! File management
  character(len=:), allocatable      :: filename
  type(local_server_stream), pointer :: stream_ptr

  ! Message reading
  integer(C_INT64_T)   :: capacity
  type(message_header) :: header
  type(message_cap)    :: end_cap

  ! Data extraction/processing
  type(data_record) :: record
  integer(C_INT64_T) :: num_data
  integer(C_INT64_T), dimension(:), pointer, contiguous, save :: model_data => NULL()

  integer :: consumer_id

  ! data_heap = context % get_node_heap()
  capacity = dcb % get_capacity(client_id, CB_KIND_INTEGER_8)
  consumer_id = dcb % get_server_bound_server_id()

  ! print '(A, I3, A I4)', 'Server ', consumer_id, ' receiving message from client ', client_id

  if (.not. associated(model_data)) allocate(model_data(capacity))

  ! print *, 'Receiving a message'
  finished = .false.

  success = dcb % peek_elems(client_id, header, message_header_size_byte(), CB_KIND_CHAR)

  if (.not. success) then
    print *, 'Error after peeking into DCB'
    error stop 1
  end if

  if (header % header_tag .ne. MSG_HEADER_TAG) then
    print *, 'ERROR: Message header tag is wrong', header % header_tag, MSG_HEADER_TAG
    error stop 1
  end if

  ! Skip this one if we have lower tags to deal with
  if (header % message_tag - state % previous_lowest_tag > context % get_server_pipeline_depth()) then
    ! print '(A,I12,A,I12)', 'Previous lowest: ', state % previous_lowest_tag, ', current tag: ', header % message_tag
    ! print *, 'SKIPPING'
    ! error stop 1
    return
  end if

  ! Update lowest tag
  state % lowest_tag = min(state % lowest_tag, header % message_tag)

  ! Actually extract the header
  success = dcb % get_elems(client_id, header, message_header_size_byte(), CB_KIND_CHAR, .true.)

  ! call print_message_header(header)

  if (header % content_size_int8 > capacity) then
    print *, 'ERROR: Message is larger than what we can deal with. That is problematic. (server)'
    print *, '   Message size:  ', header % content_size_int8
    print *, '   capacity     = ', capacity
    print *, '   client id    = ', client_id
    error stop 1
  end if

  !-------
  ! Data
  if (header % command == MSG_COMMAND_DATA) then
    ! print *, 'Got DATA message', consumer_id
    success = dcb % get_elems(client_id, record, data_record_size_byte(), CB_KIND_CHAR, .true.)
    if (.not. success) then
      print *, 'ERROR: reading record'
      error stop 1
    end if

    ! if (record % tag == 2) then
    !   call print_data_record(record)
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
  else if (header % command == MSG_COMMAND_OPEN_FILE .or. header % command == MSG_COMMAND_CREATE_STREAM) then
    allocate(character(len=(header % content_size_int8 * 8)) :: filename)
    ! print *, 'Got OPEN message', consumer_id
    success = dcb % get_elems(client_id, filename, header % content_size_int8, CB_KIND_INTEGER_8, .true.)
    ! print *, 'Opening a file named ', filename
    stream_ptr => context % open_stream_server(filename, header % stream_id)
    if (.not. stream_ptr % is_open()) then
      ! if (stream_ptr % is_owner()) then
      !   print *, 'Failed (?) to open file ', filename
      !   error stop 1
      ! else
      !   ! print *, "DEBUG: File is not open, be we're not the owner, so it's OK"
      ! end if
    end if

  !----------------
  ! Close a file
  else if (header % command == MSG_COMMAND_CLOSE_FILE) then
    ! print *, 'Got CLOSE FILE message', consumer_id
    success = context % close_stream_server(header % stream_id)
    if (.not. success) then
      print *, 'ERROR: File does not seem closable', header % stream_id, consumer_id
      error stop 1
    end if

  !--------------------
  ! Execute a command
  else if (header % command == MSG_COMMAND_SERVER_CMD) then
    ! print *, 'Got a SERVER_CMD message!'

    block
      integer(C_INT64_T), dimension(200) :: buffer
      success = dcb % get_elems(client_id, buffer, header % content_size_int8, CB_KIND_INTEGER_8, .true.)
      stream_ptr => context % get_stream(header % stream_id)   ! Retrieve stream ptr
      ! print '(A, 10(I20))', 'Putting command ', buffer(1:header % content_size_int8)
      success = stream_ptr % put_command(buffer(1:header % content_size_int8), header % message_tag)
    end block

  !----------------
  ! Misc. message
  else if (header % command == MSG_COMMAND_DUMMY) then
    if (context % debug_mode()) print *, 'DEBUG: Got a DUMMY message!', consumer_id

  !---------------------------------
  ! Stop receiving from this relay
  else if (header % command == MSG_COMMAND_RELAY_STOP) then
    if (context % debug_mode()) print '(A, I3)', 'DEBUG: Got a RELAY STOP message from relay ', consumer_id
    finished = .true.

  !------------------------------------
  ! Should not receive MODEL_STOP command, relays don't transmit these...
  else if (header % command == MSG_COMMAND_MODEL_STOP) then
    ! print *, 'Got a MODEL STOP message', consumer_id
    print *, 'ERROR: [server] Received a MODEL_STOP command'

  !------------
  ! Big no-no
  else
    print *, 'ERROR: [server] Unhandled message type!', header % command
    error stop 1
  end if

  success = dcb % get_elems(client_id, end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)

  if ((.not. success) .or. (end_cap % msg_length .ne. header % content_size_int8) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
    print *, 'Discrepancy between message length and end cap', header % content_size_int8, end_cap % msg_length, end_cap % cap_tag
    call print_message_header(header)
    ! call dcb % print(.true.)
    error stop 1
  end if

end function receive_message

end module run_server_node_module
