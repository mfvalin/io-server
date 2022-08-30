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

#include <serializer.hf>

module run_model_node_module
  use iso_c_binding

  use circular_buffer_module
  use ioserver_context_module
  use ioserver_message_module
  use jar_module
  use rpn_extra_module
  implicit none
  private

  public :: run_model_node, model_function_template, relay_function_template
  public :: default_model_bound_relay, default_server_bound_relay

  real(kind=8), parameter :: DCB_MESSAGE_RATIO = 0.2 !< What proportion of the DCB capacity should each transmission take (at most)

  abstract interface
    function model_function_template(context) result(model_success)
      import ioserver_context
      implicit none
      type(ioserver_context), intent(inout) :: context !< IO server context with which the model will operate
      logical :: model_success !< Whether the function terminated successfully
    end function model_function_template

    function relay_function_template(context) result(relay_success)
      import ioserver_context
      implicit none
      type(ioserver_context), intent(inout) :: context !< IO server context with which the relay will operate
      logical :: relay_success !< Whether the function terminated successfully
    end function relay_function_template
  end interface

  type, private :: relay_state

    type(circular_buffer), dimension(:), pointer :: model_message_buffers   !< List of CBs where messages are received from model PEs
    type(shmem_heap),      dimension(:), pointer :: model_heaps             !< List of heaps that belong to each model PE

    integer(C_INT64_T), dimension(:), allocatable :: message_content   !< Buffer for extracting model PE message content

    type(distributed_circular_buffer) :: server_bound_sender  !< DCB object used to transmit data to the server
    type(jar)                         :: server_bound_data    !< Jar object where data to be sent to the servers are accumulated
    integer                           :: server_bound_data_size_int8 !< Maximum size that can be transmitted to the server in one batch

    integer, dimension(:), allocatable :: latest_tags   !< Tags of the latest message transmitted by the relay for each model PE. -1 means the PE is done

    integer :: num_local_compute  =  0  !< How many model PEs there are on this node
    integer :: num_local_relays   =  0  !< How many server-bound relays there are on this node
    integer :: local_relay_id     = -1  !< ID of this relay among this node's relays

    integer(C_INT) :: lowest_tag         = huge(0_4)  !< After a pass, lowest message tag that was encountered in it
    integer(C_INT) :: highest_tag        = 0          !< After a pass, highest message tag that was encountered in it
    integer(C_INT) :: largest_tag_diff   = 0          !< Largest difference between message tags encountered during a single pass
    integer(C_INT) :: previous_lowest    = -1         !< During a pass, lowest message tag that was encountered in the previous pass
    integer(C_INT) :: latest_command_tag = 0          !< Latest (i.e. highest) command tag that was transmitted by this relay

    integer :: num_msg_in_pass = 0 !< After a pass, how many model PE messages were processed during it

    logical :: done_transmitting = .false. !< After a pass, whether we can exit the loop (because all model PEs are done)

    character(len=:), allocatable :: name
  contains
    procedure, pass :: init       => relay_state_init
    procedure, pass :: pre_pass   => relay_state_pre_pass
    procedure, pass :: post_pass  => relay_state_post_pass
    procedure, pass :: allocate_message_buffer
    procedure, pass :: flush_dcb_message_buffer
    final :: relay_state_finalize
  end type relay_state

contains

function run_model_node(params, custom_server_bound_relay_fn, custom_model_bound_relay_fn, custom_no_op_fn, model_function) result(success)
  implicit none
  type(ioserver_input_parameters), intent(in) :: params
  procedure(relay_function_template), pointer, intent(in), optional :: custom_server_bound_relay_fn
  procedure(relay_function_template), pointer, intent(in), optional :: custom_model_bound_relay_fn
  procedure(no_op_function_template), pointer, intent(in), optional :: custom_no_op_fn
  procedure(model_function_template), pointer, intent(in), optional :: model_function
  logical :: success

  type(ioserver_context) :: context
  procedure(model_function_template), pointer :: model_fn
  procedure(relay_function_template), pointer :: server_bound_relay_fn, model_bound_relay_fn
  procedure(no_op_function_template), pointer :: no_op_fn

  success = .false.

  ! Basic input check
  if (params % is_on_server) then 
    print '(A)', 'ERROR: Trying to initialize a model node, but setting "is_on_server = .true."'
    return
  end if

  !-----------------------------------
  ! Set up functions to call
  server_bound_relay_fn => default_server_bound_relay
  if (present(custom_server_bound_relay_fn)) then
    if (associated(custom_server_bound_relay_fn)) server_bound_relay_fn => custom_server_bound_relay_fn
  end if

  model_bound_relay_fn => default_model_bound_relay
  if (present(custom_model_bound_relay_fn)) then
    if (associated(custom_model_bound_relay_fn)) model_bound_relay_fn => custom_model_bound_relay_fn
  end if

  no_op_fn => default_no_op
  if (present(custom_no_op_fn)) then
    if (associated(custom_no_op_fn)) no_op_fn => custom_no_op_fn
  end if

  model_fn => default_model
  if (present(model_function)) then
    if (associated(model_function)) model_fn => model_function
  end if
  !-----------------------------------
  
  ! Init IO server context
  success = context % init(params)

  if (.not. success) then
    print '(A)', 'ERROR: could not initialize IO-server context for a model node!'
    return
  end if

  ! Do the work the model node is supposed to do, depending on this PE's role
  success = .false.
  if (context % is_no_op()) then
    success = no_op_fn(context)
  else if (context % is_model()) then
    success = model_fn(context)
  else if (context % is_relay()) then
    if (context % is_server_bound()) then
      success = server_bound_relay_fn(context)
    else if (context % is_model_bound()) then
      success = model_bound_relay_fn(context)
    else
      print '(A)', 'ERROR: Relay is neither server-bound nor model-bound'
      return
    end if
  else
    print '(A)', 'ERROR: Process on model node is neither a model nor a relay process'
    return
  end if

  if (.not. success) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Running process on model node'
    return
  end if

  call context % finalize()

end function run_model_node

function default_server_bound_relay(context) result(relay_success)
  use ISO_C_BINDING

  use circular_buffer_module
  use rpn_extra_module
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical :: relay_success

  type(relay_state) :: state
  logical :: success

  relay_success = .false.

  call state % init(context)

  ! Main loop (until all model PEs have sent their STOP signal)
  do while (.not. state % done_transmitting)
    success = check_and_process_model_messages(context, state)
    if (.not. success) exit
  end do

  ! Send the remaining data
  if (success) success = state % flush_dcb_message_buffer()

  if (.not. success) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Did not complete passes successfully'
    !TODO Send an error message to the server
    return
  end if

  if (context % get_debug_level() >= 1) print '(A, A, I3)', context % get_short_pe_name(), ' DEBUG: Largest diff b/w tags in one pass: ', state % largest_tag_diff

  relay_success = .true.

end function default_server_bound_relay

function check_and_process_model_messages(context, state) result(pass_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  type(relay_state),      intent(inout) :: state
  logical :: pass_success

  integer :: i_model
  logical :: success

  pass_success = .false.
  call state % pre_pass()

  do i_model = state % local_relay_id, state % num_local_compute - 1, state % num_local_relays
    success = check_and_process_single_message(context, state, i_model)
    if (.not. success) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR: Processing model PE message failed...'
      state % done_transmitting = .true.
      return
    end if
  end do ! Loop on each model PE for this relay

  pass_success = state % post_pass()
end function check_and_process_model_messages

function check_and_process_single_message(context, state, model_id) result(process_success)
  type(ioserver_context), intent(inout) :: context    !< IO server context
  type(relay_state),      intent(inout) :: state      !< Current state of the relay
  integer,                intent(in)    :: model_id   !< ID of the model PE we are checking now
  logical :: process_success !< Whether we were successful in processing this PE's message

  type(circular_buffer), pointer :: model_buffer ! Shortcut for using the model CB
  type(shmem_heap),      pointer :: model_heap   ! Shortcut for using the model heap

  type(message_header) :: header  ! header struct for both receiving and sending
  type(message_cap)    :: end_cap ! message cap struct for both receiving and sending
  type(data_record)    :: record  ! data record struct for both receiving and sending

  integer(C_INT64_T) :: content_size    ! Keep track of the original content size of the message (before the header is modified)
  integer(C_INT64_T) :: param_size_int8 ! Size of message content without some headers (size that must be extracted blindly)
  integer(C_INT64_T) :: num_data_int8   ! Size of data content in 64-bit units (for data messages)
  integer(C_INT64_T) :: total_message_size_int8  ! Total size of the message to be transmitted by the relay (including data)

  logical :: success  ! Result variable for any intermediate steps

  process_success = .false.

  !---------------------------------------------------------------------------
  ! Decide whether to process this model PE (if there is anything to process)

  ! Move on if this model buffer is done
  if (state % latest_tags(model_id) == -1) then
    process_success = .true.
    return
  end if

  state % done_transmitting = .false.   ! This model buffer is not finished yet, so keep the loop active

  model_buffer => state % model_message_buffers(model_id)
  model_heap   => state % model_heaps(model_id)

  state % lowest_tag = min(state % lowest_tag, state % latest_tags(model_id))  ! Update lowest tag

  ! If the buffer is empty, move on to the next
  if (model_buffer % get_num_elements(CB_KIND_INTEGER_8) == 0) then
    process_success = .true.
    return
  end if

  ! From this point on, we know there is something in the buffer
  success = model_buffer % peek(header, message_header_size_byte(), CB_KIND_CHAR)

  if (.not. success .or. header % header_tag .ne. MSG_HEADER_TAG) then
    print '(A, A, I8, I8)', context % get_short_pe_name(), ' ERROR: Message does not start with the message header tag', header % message_tag, MSG_HEADER_TAG
    return
  end if

  ! Skip this model process if we still need to deal with much lower message tags
  if (header % message_tag - state % previous_lowest > context % get_relay_pipeline_depth()) then
    process_success = .true.
    return
  end if

  ! Keep count of number of messages in every pass
  state % num_msg_in_pass = state % num_msg_in_pass + 1

  ! Extract header
  success = model_buffer % get(header, message_header_size_byte(), CB_KIND_CHAR, .false.)

  ! Update lowest/highest message tags
  if (header % message_tag >= 0) then
    state % lowest_tag  = min(state % lowest_tag, header % message_tag)
    state % highest_tag = max(state % highest_tag, header % message_tag)
    state % latest_tags(model_id) = header % message_tag
  end if

  content_size = header % content_size_int8

  ! call print_message_header(header)

  !------------------------------
  ! Extract/process message data

  ! -- MSG_COMMAND_DATA
  if (header % command == MSG_COMMAND_DATA) then
    param_size_int8 = header % content_size_int8 - data_record_size_int8()
    call state % allocate_message_buffer(param_size_int8)

    ! Extract record first, then the rest of metadata
    success = model_buffer % get(record, data_record_size_byte(), CB_KIND_CHAR, .false.)
    success = model_buffer % get(state % message_content, param_size_int8, CB_KIND_INTEGER_8, .false.) .and. success

    if (.not. success) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR Could not get record from data message'
      return
    end if

    num_data_int8 = num_char_to_num_int8(record % data_size_byte)       ! Get data size in the proper units

    header % content_size_int8  = header % content_size_int8 + num_data_int8   ! Update message header
    total_message_size_int8     = message_header_size_int8() + message_cap_size_int8() + header % content_size_int8

  ! -- MSG_COMMAND_MODEL_STOP
  else if (header % command == MSG_COMMAND_MODEL_STOP) then
    state % latest_tags(model_id) = -1  ! Indicate this model won't be active anymore

    if (context % get_debug_level() >= 2) then
      print '(A, A, I2, A, I4)',                &
          context % get_short_pe_name(), ' DEBUG: Model ', model_id, ' is finished. DCB client ', state % server_bound_sender % get_server_bound_client_id()
    end if

    total_message_size_int8 = message_header_size_int8() + message_cap_size_int8()

  ! -- MSG_COMMAND_SERVER_CMD, MSG_COMMAND_MODEL_STATS
  else if (header % command == MSG_COMMAND_SERVER_CMD .or.        &
           header % command == MSG_COMMAND_MODEL_STATS .or.       &
           header % command == MSG_COMMAND_USER) then
    param_size_int8 = header % content_size_int8

    call state % allocate_message_buffer(param_size_int8)

    success = model_buffer % get(state % message_content, param_size_int8, CB_KIND_INTEGER_8, .true.) ! Extract file name
    total_message_size_int8 = message_header_size_int8() + message_cap_size_int8() + param_size_int8

  ! -- We should not ever receive anything else
  else
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Unexpected message type'
    call print_message_header(header)
    return
  end if

  !----------------------------------
  ! Check the end cap at this point
  success = model_buffer % get(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)
  if ((.not. success) .or. (content_size .ne. end_cap % msg_length) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR We have a problem with message size (end cap does not match)'
    call print_message_header(header)
    print '(I10, 1X, I10, 1X, L2, I11)', end_cap % cap_tag, end_cap % msg_length, success, content_size
    return
  end if

  !----------------------------------------------------------------------------
  ! If the message is a server command that has already been sent, just skip it
  if (header % command == MSG_COMMAND_SERVER_CMD) then
    if (header % message_tag <= state % latest_command_tag) then
      process_success = .true.
      return
    end if
    state % latest_command_tag = header % message_tag ! Update latest command sent
  end if

  !------------------------------------
  ! If the DCB message buffer is too full to contain that new package, flush it now
  if (state % server_bound_data % get_top() + total_message_size_int8 > state % server_bound_data_size_int8) then
    success = state % flush_dcb_message_buffer()

    if (.not. success) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR sending message from relay to server!'
      return
    end if

    if (total_message_size_int8 > state % server_bound_data % get_size()) then
      print '(A, A, I9, I9)', context % get_short_pe_name(), ' ERROR: Too much data to fit in jar...', total_message_size_int8, state % server_bound_data % get_size()
      return
    end if
  end if

  !-----------------------------
  ! Copy message header

  header % relay_global_rank = context % get_global_rank()
  success = JAR_PUT_ITEM(state % server_bound_data, header)

  !----------------------------
  ! Copy message body
  if (header % command == MSG_COMMAND_DATA) then

    block
      type(C_PTR) :: c_data  ! C pointer into a heap, to the data to be transmitted
      integer(C_INT8_T), dimension(:), pointer :: f_data ! Fortran pointer to the same data

      c_data = model_heap % get_address_from_offset(record % heap_offset) ! Get proper pointer to data in shared memory
      if (.not. c_associated(c_data)) then
        print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Unable to retrieve the data from the heap'
        return
      end if

      call c_f_pointer(c_data, f_data, [record % data_size_byte])     ! Access data using a fortran pointer, for easy copy into the jar
      success = JAR_PUT_ITEM (state % server_bound_data, record)                                     .and. success  ! Data header
      success = JAR_PUT_ITEMS(state % server_bound_data, state % message_content(1:param_size_int8)) .and. success  ! All other metadata
      if (record % data_size_byte > 0) success = JAR_PUT_ITEMS(state % server_bound_data, f_data(:)) .and. success  ! The data

      if (.not. success) then
        print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Could not put heap data into the server-bound jar!'
        return
      end if

      ! Free the shared memory
      success = model_heap % free(c_data)
      if (.not. success) then
        print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Unable to free heap data'
        return
      end if
    end block

  else if (header % command == MSG_COMMAND_SERVER_CMD  .or.   &
           header % command == MSG_COMMAND_MODEL_STATS .or.   &
           header % command == MSG_COMMAND_USER) then

    success = JAR_PUT_ITEMS(state % server_bound_data, state % message_content(1:param_size_int8)) .and. success

  else if (header % command == MSG_COMMAND_MODEL_STOP) then
    ! No arguments to send

  else
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Unexpected message type! '
    call print_message_header(header)
    return

  end if

  !---------------------
  ! Put message end cap
  end_cap % msg_length = header % content_size_int8
  success = JAR_PUT_ITEM(state % server_bound_data, end_cap) .and. success

  process_success = success

end function check_and_process_single_message

subroutine relay_state_init(this, context)
  implicit none
  class(relay_state),     intent(inout) :: this
  type(ioserver_context), intent(in)    :: context

  type(comm_rank_size) :: local_relay_crs
  integer(C_INT64_T)   :: dcb_capacity_int8
  integer(JAR_ELEMENT) :: dummy_jar_element
  logical :: success

  ! Retrieve useful data structures
  call context % get_server_bound_cb_list(this % model_message_buffers)
  call context % get_heap_list(this % model_heaps)
  this % server_bound_sender = context % get_dcb()

  ! Get process info
  local_relay_crs = context % get_crs(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR)

  this % num_local_relays  = local_relay_crs % size
  this % local_relay_id    = local_relay_crs % rank
  this % num_local_compute = context % get_num_local_model()

  ! Determine size of buffer for sending data to the server
  dcb_capacity_int8 = this % server_bound_sender % get_capacity(CB_KIND_INTEGER_8)
  this % server_bound_data_size_int8 = int(dcb_capacity_int8 * DCB_MESSAGE_RATIO, kind = 4)

  if (storage_size(dummy_jar_element) / 8 .ne. 8) then
    print '(A)', 'ABOOOOOORT - Cannot deal with jar elements other than 64 bits in size'
    error stop 1
  end if

  ! Create buffer for outbound data
  success = this % server_bound_data % new(this % server_bound_data_size_int8)
  if (.not. success) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Could not create jar to contain DCB message...'
    error stop 1
  end if

  allocate(this % latest_tags(0:this % num_local_compute - 1))
  this % latest_tags(:) = 0

  this % name = context % get_short_pe_name()
end subroutine relay_state_init

subroutine relay_state_pre_pass(this)
  implicit none
  class(relay_state), intent(inout) :: this !< relay_state instance

  this % previous_lowest = this % lowest_tag
  this % lowest_tag      = huge(this % lowest_tag)
  this % highest_tag     = 0
  this % num_msg_in_pass = 0

  this % done_transmitting = .true.
end subroutine relay_state_pre_pass

function relay_state_post_pass(this) result(success)
  implicit none
  class(relay_state), intent(inout) :: this !< relay_state instance
  logical :: success

  success = .false.
  if (this % num_msg_in_pass > 0) then
    ! Keep track of the largest difference between tags
    this % largest_tag_diff = max(this % largest_tag_diff, this % highest_tag - this % lowest_tag)
    success = .true.
  else
    ! No one put anything to send. Send what's in the buffer, while we're just waiting for stuff...
    success = this % flush_dcb_message_buffer()
  end if
end function relay_state_post_pass

!> Make sure [buffer] has size at least [num_elem]
subroutine allocate_message_buffer(this, num_elem)
  implicit none
  class(relay_state), intent(inout) :: this     !< Relay state instance
  integer(C_INT64_T), intent(in)    :: num_elem !< How many 64-bit elements in the buffer

  if (num_elem <= 0) return

  if (allocated(this % message_content)) then
    if (size(this % message_content) >= num_elem) return
    deallocate(this % message_content)
  end if
  allocate(this % message_content(num_elem))
end subroutine allocate_message_buffer

!> If the message buffer is not empty, send its content to the server (through the DCB)
function flush_dcb_message_buffer(this) result(success)
  implicit none
  class(relay_state), intent(inout) :: this
  logical :: success !< Whether the DCB transmission was successful, if there was one, .true. otherwise

  integer(C_INT64_T) :: num_data
  integer(JAR_ELEMENT), dimension(:), pointer :: data_ptr

  success = .true.

  num_data = this % server_bound_data % get_top()
  data_ptr =>  this % server_bound_data % f_array()
  if (num_data > 0) then
    success = this % server_bound_sender % put_elems(data_ptr, num_data, CB_DATA_ELEMENT_KIND, .true.)
    call this % server_bound_data % reset()
    if (.not. success) print '(A, A)', this % name, ' ERROR: Flushing message buffer into DCB!'
  end if
end function flush_dcb_message_buffer

subroutine relay_state_finalize(this)
  implicit none
  type(relay_state), intent(inout) :: this
  integer :: jar_status

  if (allocated(this % message_content)) deallocate(this % message_content)
  if (allocated(this % latest_tags)) deallocate(this % latest_tags)
  jar_status = this % server_bound_data % free()
end subroutine relay_state_finalize

function default_model_bound_relay(context) result(relay_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: relay_success

  relay_success = .false.

  if (context % get_debug_level() >= 2) print '(A, A)', context % get_short_pe_name(), ' DEBUG: Doing its work'

  relay_success = .true.
end function default_model_bound_relay

function default_model(context) result(model_success)
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical :: model_success

  type(model_stream), pointer :: output_stream_1
  type(circular_buffer) :: data_buffer
  logical :: success

  model_success = .false.

  if (context % get_debug_level() >= 2) print *, 'Using default pseudo-model function. This does not do much.'

  call context % open_stream_model(output_stream_1)

  if (.not. associated(output_stream_1)) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Could not open a stream!!!'
    return
  end if

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid()) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: CB received from context is not valid!'
    return
  end if

  success = output_stream_1 % close()
  if (.not. success) then
    print '(A, A)', context % get_short_pe_name(), ' ERROR: Unable to close model file!!!!'
    return
  end if

  model_success = .true.
end function default_model

end module run_model_node_module
