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
  use circular_buffer_module
  use distributed_circular_buffer_module
  use ioserver_constants
  use ioserver_context_module
  use ioserver_message_module
  use rpn_extra_module
  use server_stream_module
  implicit none
  private

  public :: run_server_node, server_function_template

  type, private :: server_receiver_state
    private
    logical :: done_receiving      = .false.
    integer :: lowest_tag          = 0
    integer :: previous_lowest_tag = 0
    integer(C_INT64_T), dimension(:), pointer, contiguous :: model_data     => NULL()
    integer(C_INT64_T), dimension(:), pointer, contiguous :: command_buffer => NULL()

    logical,             dimension(:), allocatable :: active_producers
    type(cb_stats),      dimension(:), allocatable :: stats_min, stats_max
    type(cb_stats_real), dimension(:), allocatable :: stats_avg, stats_stdev
    integer,             dimension(:), allocatable :: num_stats
    integer :: num_producers = 0

    contains
    procedure, pass :: reset_pass
    procedure, pass :: allocate_model_data
    procedure, pass :: allocate_command_buffer
    procedure, nopass, private :: allocate_buffer
    procedure, pass :: set_num_producers
    procedure, pass :: add_stats
    final :: server_receive_state_final
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
    print '(A)', 'ERROR: Trying to launch the server node, but setting "is_on_server = .false."'
    return
  end if

  ! Context initialization
  success = context % init(params)

  if (.not. success) then
    print '(A)', 'ERROR: Could not initialize IO-server context for server process!'
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
    print '(A)', 'ERROR: Server process is not of a known type.'
    return
  end if

  if (.not. success) then
    print '(A)', 'ERROR: Running server process'
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
  type(server_receiver_state)         :: current_state

  integer :: server_id, num_consumers, num_producers
  integer :: i_producer
  integer :: ierr
  logical :: receive_success

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

  call current_state % set_num_producers(num_producers + 1)

  ! Now, we repeatedly loop through all buffers this consumer is responsible for
  receive_success = .true.
  do while (.not. current_state % done_receiving)
    call current_state % reset_pass()

    do i_producer = server_id, num_producers - 1, num_consumers
      ! Get and process the next message
      receive_success = receive_message(context, data_buffer, i_producer, current_state)
      if (.not. receive_success) then
        print *, 'ERROR: While receiving server message'
        current_state % done_receiving = .true.
        exit
      end if
    end do
  end do

  if (context % get_debug_level() >= 1) then
    print '(A, I3, A)', 'DEBUG: Server ', server_id, ' done receiving.'

    ! Final check on the buffers' content
    do i_producer = server_id, num_producers - 1, num_consumers
      if (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) .ne. 0) then
        print '(A, I8, I3, I3)', 'ERROR: buffer should be empty when server is done!', data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4), i_producer, server_id
        call data_buffer % print(.true.)
      end if
    end do
  end if

  ! Combine statistics
  block
    integer, dimension(:), allocatable :: ranks, ids
    integer :: i_rank, receiver_id
    integer :: dummy_integer
    integer, dimension(MPI_STATUS_SIZE) :: status
    type(cb_stats)      :: global_min, global_max
    type(cb_stats_real) :: global_mean, global_variance
    integer :: total_num_pes
    real :: weight

    allocate(ranks(0 : num_consumers - 1))
    allocate(ids(0 : num_consumers - 1))

    if (consumer_crs % rank == 0) print '(20I12)', ids
    call MPI_Gather(server_id, 1, MPI_INTEGER, ids, 1, MPI_INTEGER, 0, consumer_crs % comm, ierr)
    if (consumer_crs % rank == 0) print '(20I12)', ids

    ! PE other than root send their stats data to root
    if (consumer_crs % rank .ne. 0) then
      do i_producer = server_id, num_producers - 1, num_consumers
        call MPI_Send(current_state % stats_min(i_producer), storage_size(current_state % stats_min) / storage_size(dummy_integer), MPI_INTEGER, 0, server_id, consumer_crs % comm, ierr)
        call MPI_Send(current_state % stats_max(i_producer), storage_size(current_state % stats_max) / storage_size(dummy_integer), MPI_INTEGER, 0, server_id, consumer_crs % comm, ierr)
        call MPI_Send(current_state % stats_avg(i_producer), storage_size(current_state % stats_avg) / storage_size(dummy_integer), MPI_INTEGER, 0, server_id, consumer_crs % comm, ierr)
        call MPI_Send(current_state % stats_stdev(i_producer), storage_size(current_state % stats_stdev) / storage_size(dummy_integer), MPI_INTEGER, 0, server_id, consumer_crs % comm, ierr)
        call MPI_Send(current_state % num_stats(i_producer), storage_size(current_state % num_stats) / storage_size(dummy_integer), MPI_INTEGER, 0, server_id, consumer_crs % comm, ierr)
      end do
    end if

    ! Root receives data, compiles and prints it
    if (consumer_crs % rank == 0) then
      print '(20I12)', ids
      do i_rank = 0, consumer_crs % size - 1
        ranks(ids(i_rank)) = i_rank
      end do

      ! Get data from other server processes
      do i_producer = 0, num_producers - 1
        receiver_id = mod(i_producer, num_consumers)  ! Receiver (server) to which this producer is assigned
        if (receiver_id .ne. server_id) then
          call MPI_Recv(current_state % stats_min(i_producer), storage_size(current_state % stats_min) / storage_size(dummy_integer), MPI_INTEGER, receiver_id, receiver_id, consumer_crs % comm, status, ierr)
          call MPI_Recv(current_state % stats_max(i_producer), storage_size(current_state % stats_max) / storage_size(dummy_integer), MPI_INTEGER, receiver_id, receiver_id, consumer_crs % comm, status, ierr)
          call MPI_Recv(current_state % stats_avg(i_producer), storage_size(current_state % stats_avg) / storage_size(dummy_integer), MPI_INTEGER, receiver_id, receiver_id, consumer_crs % comm, status, ierr)
          call MPI_Recv(current_state % stats_stdev(i_producer), storage_size(current_state % stats_stdev) / storage_size(dummy_integer), MPI_INTEGER, receiver_id, receiver_id, consumer_crs % comm, status, ierr)
          call MPI_Recv(current_state % num_stats(i_producer), storage_size(current_state % num_stats) / storage_size(dummy_integer), MPI_INTEGER, receiver_id, receiver_id, consumer_crs % comm, status, ierr)
        end if
      end do

      ! Combine the data
      global_min = maxed_cb_stats()
      total_num_pes = sum(current_state % num_stats(:))
      do i_producer = 0, num_producers - 1
        if (current_state % num_stats(i_producer) == 0) cycle
        global_min = cb_stats_min(global_min, current_state % stats_min(i_producer))
        global_max = cb_stats_max(global_max, current_state % stats_max(i_producer))
        weight = real(current_state % num_stats(i_producer)) / total_num_pes
        call global_mean     % stats_add(current_state % stats_avg(i_producer), real(current_state % num_stats(i_producer)))
        ! print *, 'time + num for prod', current_state % stats_avg(i_producer) % total_write_time_ms, current_state % num_stats(i_producer), i_producer
        call global_variance % stats_add(current_state % stats_stdev(i_producer), 1.0)
      end do

      call global_mean % stats_mult_scalar(1.0 / total_num_pes)

      do i_producer = 0, num_producers - 1
        if (current_state % num_stats(i_producer) == 0) cycle
        call global_variance % stats_add_diff_sq(global_mean, current_state % stats_avg(i_producer), real(current_state % num_stats(i_producer), kind=8))
        call current_state % stats_stdev(i_producer) % stats_mult_scalar(real(1.0 / (current_state % num_stats(i_producer) - 1), kind=4))
        ! print *, 'time + num for prod', current_state % stats_avg(i_producer) % total_write_time_ms, current_state % num_stats(i_producer), i_producer
      end do

      call global_variance % stats_mult_scalar(1.0 / (total_num_pes - 1))

      print '(A)', '-------------------------------------------------------------------------'
      print '(A)', '  Model write buffers (cumulative per node)'
      do i_producer = 0, num_producers - 1
        call print_cumulated_stats(                                                               &
                current_state % stats_avg(i_producer), current_state % stats_stdev(i_producer),   &
                current_state % stats_min(i_producer), current_state % stats_max(i_producer),     &
                i_producer == 0)
        ! print *, 'time + num for prod', current_state % stats_avg(i_producer) % total_write_time_ms, current_state % num_stats(i_producer), i_producer
      end do
      print '(A)', ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '
      call print_cumulated_stats(global_mean, global_variance, global_min, global_max)
      print '(A)', '-------------------------------------------------------------------------'

    end if
  end block

  ! Wait for all server-bound server processes to finish
  call MPI_Barrier(consumer_crs % comm, ierr)

  server_success = receive_success
end function default_server_bound

function  default_model_bound(context) result(server_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: server_success

  server_success = .false.
  if (context % get_debug_level() >= 2) print '(A, A)', 'DEBUG: Doing the work - ', context % get_detailed_pe_name()
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
  type(comm_rank_size) :: grid_proc_crs
  logical :: early_stop
  integer :: i_stream

  server_success = .false.
  early_stop     = .false.

  grid_proc_crs = context % get_crs(GRID_PROCESSOR_COLOR)

  if (context % get_debug_level() >= 2) then
    print '(A, I2, A, I2)', 'DEBUG: Grid processor process rank ', grid_proc_crs % rank, ' of ', grid_proc_crs % size
  end if

  do while (.not. context % is_time_to_quit() .and. .not. early_stop)
    ! if (context % is_time_to_quit()) finished = .true.
    do i_stream = 1, MAX_NUM_STREAMS
      ! print *, 'Stream, grid proc ', i_stream, grid_proc_crs % rank
      call context % get_stream(i_stream, stream_ptr)
      if (stream_ptr % is_owner()) then
        if (.not. stream_ptr % process_stream()) then
          print '(A, I3, A, I4)', 'ERROR: when processing stream rank ', i_stream, ', ID ', stream_ptr % get_id()
          early_stop = .true.
          exit
        end if
      end if
    end do
  end do

  if (context % get_debug_level() >= 1) then
    print '(A, I2, A, I2)', 'DEBUG: Grid processor process DONE. Rank ', grid_proc_crs % rank, ' of ', grid_proc_crs % size
  end if

  server_success = .not. early_stop
end function default_grid_processor

function receive_message(context, dcb, client_id, state) result(receive_success)
  implicit none
  type(ioserver_context),            intent(inout) :: context   !< IO server context in which we are operating
  type(distributed_circular_buffer), intent(inout) :: dcb       !< Handle to the DCB used for transmission
  integer,                           intent(in)    :: client_id !< ID of the specific client we are receiving from
  type(server_receiver_state),       intent(inout) :: state     !< Current state of the receiver process
  logical :: receive_success

  logical :: success

  ! File management
  ! character(len=:), allocatable      :: filename
  type(local_server_stream), pointer :: stream_ptr

  ! Message reading
  integer(C_INT64_T)   :: capacity
  type(message_header) :: header
  type(message_cap)    :: end_cap

  ! Data extraction/processing
  type(data_record) :: record
  integer(C_INT64_T) :: num_data_int8
  ! integer(C_INT64_T), dimension(:), pointer, contiguous, save :: model_data => NULL()

  integer :: consumer_id

  ! data_heap = context % get_node_heap()
  capacity = dcb % get_capacity(client_id, CB_KIND_INTEGER_8)
  consumer_id = dcb % get_server_bound_server_id()

  receive_success = .false.

  if (.not. state % active_producers(client_id)) then
    receive_success = .true.
    return
  end if

  state % done_receiving = .false.

  success = dcb % peek_elems(client_id, header, message_header_size_byte(), CB_KIND_CHAR)

  if (.not. success) then
    print '(A)', 'ERROR: after peeking into DCB'
    return
  end if

  if (header % header_tag .ne. MSG_HEADER_TAG) then
    print '(A, I8, I8)', 'ERROR: Message header tag is wrong', header % header_tag, MSG_HEADER_TAG
    return
  end if

  ! Skip this one if we have lower tags to deal with
  if (header % message_tag - state % previous_lowest_tag > context % get_server_pipeline_depth()) then
    ! print *, 'SKIPPING'
    receive_success = .true.
    return
  end if

  ! Update lowest tag
  state % lowest_tag = min(state % lowest_tag, header % message_tag)

  ! Actually extract the header
  success = dcb % get_elems(client_id, header, message_header_size_byte(), CB_KIND_CHAR, .true.)

  ! Retrieve stream ptr
  call context % get_stream(header % stream_rank, stream_ptr)

  ! call print_message_header(header)

  if (header % content_size_int8 > capacity) then
    print *, 'ERROR: Message is larger than what we can deal with. That is problematic. (server)'
    print *, '   Message size:  ', header % content_size_int8
    print *, '   capacity     = ', capacity
    print *, '   client id    = ', client_id
    return
  end if

  !-------
  ! Data
  if (header % command == MSG_COMMAND_DATA) then
    ! print *, 'Got DATA message', consumer_id
    success = dcb % get_elems(client_id, record, data_record_size_byte(), CB_KIND_CHAR, .true.)
    if (.not. success) then
      print '(A)', 'ERROR: reading record'
      return
    end if

    if (record % cmeta_size > 0) then
      print '(A)', 'ERROR: Cannot handle compression metadata'
      return
    end if

    if (record % meta_size > 0) then
      print '(A)', 'ERROR: Cannot handle other metadata'
      return
    end if

    call state % allocate_command_buffer(record % command_size_int8)
    success = dcb % get_elems(client_id, state % command_buffer, record % command_size_int8, CB_KIND_INTEGER_8, .true.) .and. success ! Extract command from DCB
    success = stream_ptr % put_command(state % command_buffer(1:record % command_size_int8), header % message_tag)      .and. success ! Send command for later processing

    if (.not. success) then
      print '(A)', 'ERROR: Unable to enqueue the command accompanying a data packet. The queue is probably full, this could have created a deadlock.'
      return
    end if
 
    num_data_int8 = num_char_to_num_int8(record % data_size_byte)
    call state % allocate_model_data(num_data_int8)
    success = dcb % get_elems(client_id, state % model_data, num_data_int8, CB_KIND_INTEGER_8, .true.) .and. success ! Extract data from DCB
    success = stream_ptr % put_data(record, state % model_data) .and. success   ! Put data in its proper place within a global grid

    if (.not. success) then
      print *, 'ERROR: Could not put data into partial grid! (or maybe something else)'
      return
    end if

  !--------------------
  ! Execute a command
  else if (header % command == MSG_COMMAND_SERVER_CMD) then
    if (context % get_debug_level() >= 2) print '(A, I4)', 'Got a SERVER_CMD message! From model ', header % sender_global_rank
    call state % allocate_command_buffer(header % content_size_int8)
    success = dcb % get_elems(client_id, state % command_buffer, header % content_size_int8, CB_KIND_INTEGER_8, .true.)
    success = stream_ptr % put_command(state % command_buffer(1:header % content_size_int8), header % message_tag) .and. success

  !----------------
  ! Misc. message
  else if (header % command == MSG_COMMAND_DUMMY) then
    if (context % get_debug_level() >= 2) print '(A, I3)', 'DEBUG: Got a DUMMY message!', consumer_id

  !---------------------------------
  ! Stop receiving from this relay
  else if (header % command == MSG_COMMAND_RELAY_STOP) then
    if (context % get_debug_level() >= 1) print '(A, I3, I4)', 'DEBUG: Got a RELAY STOP message from relay ', consumer_id, header % relay_global_rank
    state % active_producers(client_id) = .false.
  
  !----------------------------
  ! Statistics from a MODEL PE
  else if (header % command == MSG_COMMAND_MODEL_STATS) then
    block
      type(cb_stats) :: stats
      if (context % get_debug_level() >= 2) print '(A, I5)', 'DEBUG: Got STATS from model ', header % sender_global_rank
      success = dcb % get_elems(client_id, stats, cb_stats_size_byte(), CB_KIND_CHAR, .true.)
      call state % add_stats(stats, client_id)
    end block

  !------------------------------------------------
  ! MODEL_STOP command, don't do anything for that
  else if (header % command == MSG_COMMAND_MODEL_STOP) then
    if (context % get_debug_level() >= 2) print '(A, I4)', 'DEBUG: Got a MODEL STOP message from ', header % sender_global_rank
    ! Do nothing for that

  !------------
  ! Big no-no
  else
    print *, 'ERROR: [server] Unhandled message type!', header % command
    call print_message_header(header)
    return
  end if

  if (.not. success) return

  success = dcb % get_elems(client_id, end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)

  if ((.not. success) .or. (end_cap % msg_length .ne. header % content_size_int8) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
    print *, 'Discrepancy between message length and end cap', header % content_size_int8, end_cap % msg_length, end_cap % cap_tag
    call print_message_header(header)
    ! call dcb % print(.true.)
    return
  end if

  receive_success = .true.

end function receive_message

subroutine reset_pass(state)
  implicit none
  class(server_receiver_state), intent(inout) :: state
  state % done_receiving      = .true.
  state % previous_lowest_tag = state % lowest_tag
  state % lowest_tag          = huge(state % lowest_tag)
end subroutine reset_pass

subroutine allocate_model_data(state, num_elements)
  implicit none
  class(server_receiver_state), intent(inout) :: state
  integer(C_INT64_T),           intent(in)    :: num_elements
  call allocate_buffer(state % model_data, num_elements)
end subroutine allocate_model_data

subroutine allocate_command_buffer(state, num_elements)
  implicit none
  class(server_receiver_state), intent(inout) :: state
  integer(C_INT64_T),           intent(in)    :: num_elements
  call allocate_buffer(state % command_buffer, num_elements)
end subroutine allocate_command_buffer

subroutine allocate_buffer(buffer, num_elements)
  implicit none
  integer(C_INT64_T), dimension(:), pointer, contiguous, intent(inout) :: buffer
  integer(C_INT64_T), intent(in) :: num_elements
  if (associated(buffer)) then
    if (size(buffer) >= num_elements) return
    deallocate(buffer)
  end if
  allocate(buffer(num_elements))
end subroutine allocate_buffer

subroutine set_num_producers(state, num_producers)
  implicit none
  class(server_receiver_state), intent(inout) :: state !< server_receiver_state instance
  integer, intent(in) :: num_producers  !< How many producers (relays) this state will manage

  if (allocated(state % stats_min)) deallocate(state % stats_min)
  if (allocated(state % stats_max)) deallocate(state % stats_max)
  if (allocated(state % stats_avg)) deallocate(state % stats_avg)
  if (allocated(state % stats_stdev)) deallocate(state % stats_stdev)
  if (allocated(state % num_stats)) deallocate(state % num_stats)
  if (allocated(state % active_producers)) deallocate(state % active_producers)

  state % num_producers = num_producers
  if (num_producers > 0) then
    allocate(state % stats_min(0 : num_producers - 1))
    allocate(state % stats_max(0 : num_producers - 1))
    allocate(state % stats_avg(0 : num_producers - 1))
    allocate(state % stats_stdev(0 : num_producers - 1))
    allocate(state % num_stats(0 : num_producers - 1))
    allocate(state % active_producers(0 : num_producers - 1))
    state % stats_min(:)        = maxed_cb_stats()
    state % num_stats(:)        = 0
    state % active_producers(:) = .true.
  end if
end subroutine set_num_producers

subroutine add_stats(this, stats, producer_id)
  implicit none
  class(server_receiver_state), intent(inout) :: this
  type(cb_stats),               intent(in)    :: stats
  integer,                      intent(in)    :: producer_id

  type(cb_stats_real) :: old_mean

  this % num_stats(producer_id) = this % num_stats(producer_id) + 1
  this % stats_min(producer_id) = cb_stats_min(this % stats_min(producer_id), stats)
  this % stats_max(producer_id) = cb_stats_max(this % stats_max(producer_id), stats)
  old_mean = this % stats_avg(producer_id)
  call this % stats_avg(producer_id) % mean_add_sample(stats, this % num_stats(producer_id))
  call this % stats_stdev(producer_id) % variance_add_sample(stats, old_mean, this % stats_avg(producer_id))

end subroutine add_stats

subroutine server_receive_state_final(state)
  implicit none
  type(server_receiver_state), intent(inout) :: state
  if (associated(state % model_data)) deallocate(state % model_data)
  if (associated(state % command_buffer)) deallocate(state % command_buffer)

  call state % set_num_producers(0)
end subroutine server_receive_state_final

end module run_server_node_module
