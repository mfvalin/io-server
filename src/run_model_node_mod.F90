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
  use ioserver_context_module
  implicit none
  private

  public :: run_model_node, model_function_template, relay_function_template
  public :: default_model_bound_relay, default_server_bound_relay

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
    print *, 'ERROR: Trying to initialize a model node, but setting "is_on_server = .true."'
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
    print *, 'ERROR: could not initialize IO-server context for a model node!'
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
      print *, 'ERROR: Relay is neither server-bound nor model-bound'
      return
    end if
  else
    print *, 'ERROR: Process on model node is neither a model nor a relay process'
    return
  end if

  if (.not. success) then
    print *, 'ERROR: Running process on model node'
    return
  end if

  call context % finalize()

end function run_model_node

function default_server_bound_relay(context) result(relay_success)
  use ISO_C_BINDING

  use circular_buffer_module
  use ioserver_message_module
  use jar_module
  use rpn_extra_module
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical :: relay_success

  integer, parameter :: MAX_DCB_MESSAGE_SIZE_INT = 10000000

  type(circular_buffer), dimension(:), pointer :: cb_list
  type(heap),            dimension(:), pointer :: heap_list
  type(distributed_circular_buffer) :: data_buffer
  type(comm_rank_size)  :: node_crs, local_relay_crs
  integer :: client_id, num_clients
  integer :: i_compute
  integer :: num_local_compute, num_local_relays, local_relay_id
  integer :: dcb_message_buffer_size
  integer(C_INT64_T) :: dcb_capacity

  integer(C_INT64_T) :: total_message_size_int8, content_size
  integer(C_INT64_T) :: param_size_int8
  logical :: finished, success
  integer(C_INT64_T), dimension(:), allocatable :: cb_message
  integer(C_INT) :: lowest_tag, highest_tag, largest_tag_diff, previous_lowest
  integer :: num_msg_in_pass

  integer(C_INT64_T), dimension(:), pointer :: f_data
  type(C_PTR) :: c_data
  integer(C_INT64_T) :: num_data_int8

  type(data_record)    :: record
  type(message_header) :: header
  type(message_cap)    :: end_cap
  type(jar)            :: dcb_message_jar
  integer              :: jar_ok
  integer(JAR_ELEMENT) :: num_jar_elem
  integer              :: latest_command_tag
  integer, dimension(:), allocatable :: latest_tags ! Tags of the latest message transmitted by the relay for each model PE. -1 means the PE is done

  relay_success = .false.

  ! Retrieve useful data structures
  cb_list     => context % get_server_bound_cb_list()
  heap_list   => context % get_heap_list()
  data_buffer =  context % get_dcb()

  ! Get process info
  node_crs        = context % get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
  local_relay_crs = context % get_crs(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR)

  num_local_relays  = local_relay_crs % size
  local_relay_id    = local_relay_crs % rank
  num_local_compute = context % get_num_local_model()

  client_id               = data_buffer % get_server_bound_client_id()
  num_clients             = data_buffer % get_num_server_bound_clients()
  dcb_capacity            = data_buffer % get_capacity(CB_DATA_ELEMENT_KIND)
  dcb_message_buffer_size = min(int(dcb_capacity, kind=4) / 4, MAX_DCB_MESSAGE_SIZE_INT) - 10  ! Make sure there will be a bit of loose space in the server-side buffer

  ! Create buffer for outbound data
  jar_ok = dcb_message_jar % new(dcb_message_buffer_size)
  if (jar_ok .ne. 0) then
    print *, 'Could not create jar to contain DCB message...'
    error stop 1
  end if

  c_data = C_NULL_PTR
  nullify(f_data)
  total_message_size_int8 = 0
  content_size = 0

  allocate(latest_tags(0:num_local_compute - 1))
  latest_tags(:) = 0
  call dcb_message_jar % reset()

  latest_command_tag = 0
  param_size_int8 = -1
  largest_tag_diff = 0
  lowest_tag = 0
  finished = .false.

  ! Say hi to the consumer processes
  header % content_size_int8  = 0
  header % command            = MSG_COMMAND_DUMMY
  header % sender_global_rank = context % get_global_rank()
  header % relay_global_rank  = context % get_global_rank()
  end_cap % msg_length = header % content_size_int8

  success = data_buffer % put_elems(header, message_header_size_int8(), CB_KIND_INTEGER_8, .true.)
  success = data_buffer % put_elems(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.) .and. success ! Append size

  if (.not. success) then
    print *, 'ERROR: Failed saying HI to the consumer...'
    return
  end if

  ! Main loop (until all model PEs have sent their STOP signal)
  do while (.not. finished)

    ! Prepare pass over each model PE this relay is responsible for
    previous_lowest = lowest_tag
    lowest_tag = huge(lowest_tag)
    highest_tag = 0
    num_msg_in_pass = 0

    finished = .true.
    do i_compute = local_relay_id, num_local_compute - 1, num_local_relays

      !---------------------------------------------------------------------------
      ! Decide whether to process this model PE (if there is anything to process)

      if (latest_tags(i_compute) == -1) cycle               ! This model buffer is done, move on
      finished = .false.                                    ! This model buffer is not finished yet, so keep the loop active
      lowest_tag = min(lowest_tag, latest_tags(i_compute))  ! Update lowest tag
      if (cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_8) == 0) cycle ! The buffer is empty, move on to the next

      ! From this point on, we know there is something in the buffer
      success = cb_list(i_compute) % peek(header, message_header_size_byte(), CB_KIND_CHAR)

      if (.not. success .or. header % header_tag .ne. MSG_HEADER_TAG) then
        print '(A, I8, I8, A, I4)', 'ERROR: Message does not start with the message header tag', header % message_tag, MSG_HEADER_TAG, &
              ', relay id ', local_relay_id
        return
      end if

      ! Skip this model process if we still need to deal with much lower message tags
      if (header % message_tag - previous_lowest > context % get_relay_pipeline_depth()) cycle

      ! Keep count of number of messages in every pass
      num_msg_in_pass = num_msg_in_pass + 1

      ! Extract header
      success = cb_list(i_compute) % get(header, message_header_size_byte(), CB_KIND_CHAR, .false.)

      ! Update lowest/highest message tags
      lowest_tag  = min(lowest_tag, header % message_tag)
      highest_tag = max(highest_tag, header % message_tag)
      latest_tags(i_compute) = header % message_tag

      content_size = header % content_size_int8

      ! call print_message_header(header)

      !------------------------------
      ! Extract/process message data
      if (header % command == MSG_COMMAND_DATA) then
        success = cb_list(i_compute) % get(record, data_record_size_byte(), CB_KIND_CHAR, .false.) ! Extract record 
        ! TODO  Get compression metadata
        ! TODO  Get other metadata

        if (.not. success) then
          print *, 'ERROR Could not get record from data message'
          return
        end if

        num_data_int8 = num_char_to_num_int8(record % data_size_byte)                 ! Get data size in the proper units
        c_data = heap_list(i_compute) % get_address_from_offset(record % heap_offset) ! Get proper pointer to data in shared memory
        call c_f_pointer(c_data, f_data, [num_data_int8])                             ! Access it using a fortran pointer, for easy copy into the jar

        header % content_size_int8  = header % content_size_int8 + num_data_int8   ! Update message header
        total_message_size_int8     = message_header_size_int8() + message_cap_size_int8() + header % content_size_int8

      else if (header % command == MSG_COMMAND_MODEL_STOP) then
        latest_tags(i_compute) = -1  ! Indicate this model won't be active anymore

        if (context % debug_mode()) then
          print '(A, I3, A, I2, A, I4)', 'DEBUG: Model ', i_compute, ' is finished, relay ', local_relay_id, ' (local), DCB client ', client_id
        end if

        total_message_size_int8 = message_header_size_int8() + message_cap_size_int8()

      else if (header % command == MSG_COMMAND_CLOSE_FILE) then
        ! Do nothing, just send the header along
        total_message_size_int8 = message_header_size_int8() + message_cap_size_int8()

      else if (header % command == MSG_COMMAND_OPEN_FILE .or.             &
               header % command == MSG_COMMAND_SERVER_CMD .or.            &
               header % command == MSG_COMMAND_CREATE_STREAM) then
        param_size_int8 = header % content_size_int8

        if (.not. allocated(cb_message)) then
          allocate(cb_message(param_size_int8))
        else if (size(cb_message) < param_size_int8) then
          deallocate(cb_message)
          allocate(cb_message(param_size_int8))
        end if

        success = cb_list(i_compute) % get(cb_message, param_size_int8, CB_KIND_INTEGER_8, .true.) ! Extract file name
        total_message_size_int8 = message_header_size_int8() + message_cap_size_int8() + param_size_int8

      else
        print *, 'ERROR: [relay] Unknown message type'
        call print_message_header(header)
        return
      end if

      !----------------------------------
      ! Check the end cap at this point
      success = cb_list(i_compute) % get(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true.)
      if ((.not. success) .or. (content_size .ne. end_cap % msg_length) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
        print *, 'ERROR We have a problem with message size (end cap does not match)'
        call print_message_header(header)
        print *, end_cap % cap_tag, end_cap % msg_length, success, content_size
        success = .false.
        return
      end if

      !----------------------------------------------------------------------------
      ! If the message is a server command that has already been sent, just skip it
      if (header % command == MSG_COMMAND_SERVER_CMD) then
        if (header % message_tag > latest_command_tag) then
          latest_command_tag = header % message_tag
        else
          cycle
        end if
      end if

      !------------------------------------
      ! If the DCB message buffer is too full to contain that new package, flush it now
      if (dcb_message_jar % high() + total_message_size_int8 > dcb_message_buffer_size) then
        success = data_buffer % put_elems(dcb_message_jar % array(), dcb_message_jar % high(), CB_DATA_ELEMENT_KIND, .true.)
        call dcb_message_jar % reset()

        if (.not. success) then
          print *, 'ERROR sending message from relay to server!'
          return
        end if

        if (total_message_size_int8 > dcb_message_jar % usable()) then
          print *, 'ERROR: Too much data to fit in jar...', total_message_size_int8, dcb_message_jar % usable()
          return
        end if
      end if

      !-----------------------------
      ! Copy message header

      header % relay_global_rank = context % get_global_rank()
      ! print *, 'Message size: ', header % content_size_int8
      num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header)

      !----------------------------
      ! Copy message body
      if (header % command == MSG_COMMAND_DATA) then

        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, record)      ! Data header
        !TODO                                                     ! Compression metadata
        !TODO                                                     ! Other metadata
        num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, f_data(:))  ! The data
        
        success = heap_list(i_compute) % free(c_data)            ! Free the shared memory
        if (.not. success) then
          print*, 'ERROR: Unable to free heap data (from RELAY)'
          return
        end if

      else if (header % command == MSG_COMMAND_OPEN_FILE .or. header % command == MSG_COMMAND_SERVER_CMD .or. header % command == MSG_COMMAND_CREATE_STREAM) then

        num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, cb_message(1:param_size_int8))

      end if

      !---------------------
      ! Put message end cap
      end_cap % msg_length = header % content_size_int8
      num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, end_cap)

    end do ! Loop on each model PE for this relay

    if (num_msg_in_pass > 0) then
      ! Keep track of the largest difference between tags
      largest_tag_diff = max(largest_tag_diff, highest_tag - lowest_tag)
    else
      ! No one put anything to send. Send what's in the buffer, while we're just waiting for stuff...
      if (dcb_message_jar % high() > 0) then
        success = data_buffer % put_elems(dcb_message_jar % array(), dcb_message_jar % high(), CB_DATA_ELEMENT_KIND, .true.)
        call dcb_message_jar % reset()

        if (.not. success) then
          print *, 'ERROR sending message from relay to server!'
          return
        end if
      end if
    end if

  end do ! Loop until finished

  ! Send the remaining data
  if (dcb_message_jar % high() > 0) then
    ! print *, 'Sending remaining data: ', dcb_message_jar % high()
    success = data_buffer % put_elems(dcb_message_jar % array(), dcb_message_jar % high(), CB_DATA_ELEMENT_KIND, .true.)

    if (.not. success) then
      print *, 'ERROR sending remaining data!'
      return
    end if
  end if

  if (allocated(cb_message)) deallocate(cb_message)
  if (allocated(latest_tags)) deallocate(latest_tags)

  if (context % debug_mode()) print *, 'DEBUG: Largest diff b/w tags in one pass: ', largest_tag_diff

  if (local_relay_id == 0) then
    ! call heap_list(0) % dumpinfo()
    ! do i_compute = 0, num_local_compute - 1
    !   call cb_list(i_compute) % print_stats(client_id * 100 + i_compute, i_compute == 0)
    ! end do
  end if

  relay_success = .true.

end function default_server_bound_relay

function default_model_bound_relay(context) result(relay_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: relay_success

  relay_success = .false.

  if (context % debug_mode()) print *, 'Model-bound relay process'

  relay_success = .true.
end function default_model_bound_relay

function default_model(context) result(model_success)
  use ioserver_mpi

  use heap_module
  use ioserver_message_module
  use jar_module
  use rpn_extra_module, only: sleep_us
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical :: model_success

  type(model_stream), pointer :: output_stream_1
  character(len=1), dimension(22) :: stream_name
  character(len=32) :: tmp_name
  type(jar) :: command_jar
  integer :: jar_ok
  integer(JAR_ELEMENT) :: num_elem
  type(circular_buffer) :: data_buffer
  logical :: success

  model_success = .false.

  if (context % debug_mode()) print *, 'Using default pseudo-model function. This does not do much.'

  tmp_name = 'pseudo_model_results_1'
  call context % open_stream_model(trim(tmp_name), output_stream_1)
  stream_name(1:22) = transfer(tmp_name, stream_name)
  ! print *, 'tmp_name: ', tmp_name
  ! print *, 'stream_name: ', stream_name
  jar_ok = command_jar % new(100)
  num_elem = JAR_PUT_ITEMS(command_jar, stream_name)
  success = output_stream_1 % send_command(command_jar)
  if (.not. success .or. .not. output_stream_1 % is_open()) then
    print *, 'Unable to open model file 1 !!!!', success, output_stream_1 % is_open()
    return
  end if

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid(.false.)) then
    print *, 'ERROR: CB received from context is not valid!'
    return
  end if

  success = output_stream_1 % close()
  if (.not. success) then
    print *, 'Unable to close model file!!!!'
    return
  end if

  model_success = .true.
end function default_model

end module run_model_node_module
