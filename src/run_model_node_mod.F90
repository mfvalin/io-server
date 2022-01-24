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

  public :: run_model_node, ioserver_function_template

  abstract interface
    subroutine ioserver_function_template(context)
      import ioserver_context
      implicit none
      type(ioserver_context), intent(inout) :: context
    end subroutine ioserver_function_template
  end interface

contains

subroutine run_model_node(num_relay_per_node, use_debug_mode_in, custom_server_bound_relay_function, model_function)
  implicit none
  integer, intent(in) :: num_relay_per_node
  logical, optional, intent(in) :: use_debug_mode_in
  procedure(ioserver_function_template), pointer, intent(in), optional :: custom_server_bound_relay_function
  procedure(ioserver_function_template), pointer, intent(in), optional :: model_function

  type(ioserver_context) :: context
  logical :: success
  logical :: use_debug_mode

  procedure(ioserver_function_template), pointer :: server_bound_relay_fn, model_fn

  use_debug_mode = .false.
  if (present(use_debug_mode_in)) use_debug_mode = use_debug_mode_in

  server_bound_relay_fn => server_bound_relay_process
  if (present(custom_server_bound_relay_function)) server_bound_relay_fn => custom_server_bound_relay_function

  model_fn => pseudo_model_process
  if (present(model_function)) then
    if (associated(model_function)) model_fn => model_function
  end if
  
  success = context % init(.false., num_relay_per_node, 0, 0, 0, in_debug_mode = use_debug_mode)

  if (.not. success) then
    print *, 'ERROR: could not initialize IO-server context for a model node!'
    error stop 1
  end if

  if (context % is_model()) then
    call model_fn(context)
  else if (context % is_relay()) then
    if (context % is_server_bound()) then
      ! call server_bound_relay_process(context)
      call server_bound_relay_fn(context)
    else if (context % is_model_bound()) then
      call model_bound_relay_process(context)
    else
      print *, 'ERROR: Relay is neither server-bound nor model-bound'
    end if
  else
    print *, 'ERROR: Process on model node is neither a model nor a relay process'
  end if

  call context % finalize()

end subroutine run_model_node

subroutine server_bound_relay_process(context)
  use ISO_C_BINDING

  use circular_buffer_module
  use ioserver_message_module
  use jar_module
  implicit none

  type(ioserver_context), intent(inout) :: context

  integer, parameter :: MAX_DCB_MESSAGE_SIZE_INT = 50000

  type(circular_buffer), dimension(:), pointer :: cb_list
  type(heap),            dimension(:), pointer :: heap_list
  type(distributed_circular_buffer) :: data_buffer
  type(comm_rank_size)  :: node_crs, local_relay_crs
  integer :: client_id, num_clients
  integer :: i_compute
  integer :: num_local_compute, num_local_relays, local_relay_id
  integer :: dcb_message_buffer_size
  integer(C_INT64_T) :: dcb_capacity

  integer(C_INT64_T) :: total_message_size, content_size
  integer(C_INT64_T) :: filename_size
  logical :: finished, success, skip_message
  integer, dimension(:), allocatable :: cb_message
  integer(C_INT) :: message_header_tag

  integer, dimension(:), pointer :: f_data
  type(C_PTR) :: c_data
  integer(C_INT64_T) :: num_data_int

  type(model_record)   :: record
  type(message_header) :: header
  type(message_cap)    :: end_cap
  type(jar)            :: dcb_message_jar
  integer              :: jar_ok
  integer(JAR_ELEMENT) :: num_jar_elem
  integer(JAR_ELEMENT), dimension(:), pointer :: dcb_message
  logical, dimension(:), allocatable :: model_finished

  print *, 'Server-bound relay process'

  cb_list     => context % get_server_bound_cb_list()
  heap_list   => context % get_heap_list()
  data_buffer =  context % get_dcb()

  node_crs        = context % get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
  local_relay_crs = context % get_crs(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR)

  num_local_relays  = local_relay_crs % size
  local_relay_id    = local_relay_crs % rank
  num_local_compute = context % get_num_local_model()

  ! print '(A, I5)', 'DEBUG: RELAY, global ', context % get_global_rank()

  client_id               = data_buffer % get_server_bound_client_id()
  num_clients             = data_buffer % get_num_server_bound_clients()
  dcb_capacity            = data_buffer % get_capacity(CB_DATA_ELEMENT_KIND)
  dcb_message_buffer_size = min(int(dcb_capacity, kind=4) / 4, MAX_DCB_MESSAGE_SIZE_INT) - 10  ! Make sure there will be a bit of loose space in the server-side buffer

  jar_ok = dcb_message_jar % new(dcb_message_buffer_size)
  if (jar_ok .ne. 0) then
    print *, 'Could not create jar to contain DCB message...'
    error stop 1
  end if

  dcb_message => dcb_message_jar % raw_array()

  c_data = C_NULL_PTR
  nullify(f_data)
  total_message_size = 0
  content_size = 0

  ! Say hi to the consumer processes
  header % content_length     = 0
  header % command            = MSG_COMMAND_DUMMY
  header % sender_global_rank = context % get_global_rank()
  header % relay_global_rank  = context % get_global_rank()
  end_cap % msg_length = header % content_length

  success = data_buffer % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)
  success = data_buffer % put_elems(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success ! Append size

  if (.not. success) then
    print *, 'ERROR: Failed saying HI to the consumer...'
    error stop 1
  end if

  ! The main loop
  allocate(model_finished(0:num_local_compute - 1))
  model_finished(:) = .false.
  ! expected_message(:) = -1
  call dcb_message_jar % reset()

  finished = .false.
  do while (.not. finished)

    finished = .true.
    do i_compute = local_relay_id, num_local_compute - 1, num_local_relays
      skip_message = .false.

      ! print *, 'num elements: ', cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_4), i_compute

      if (model_finished(i_compute)) cycle  ! This model buffer is done, move on
      finished = .false.                    ! This model buffer is not finished yet, keep the loop active
      if (cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_4) == 0) cycle ! The buffer is empty, move on to the next

      ! From this point on, we know there is something in the buffer
      success = cb_list(i_compute) % peek(message_header_tag, 1_8, CB_KIND_INTEGER_4)

      if (message_header_tag .ne. MSG_HEADER_TAG) then
        print '(A, I8, I8, A, I4)', 'ERROR: Message does not start with the message header tag', message_header_tag, MSG_HEADER_TAG, &
              ', relay id ', local_relay_id
        error stop 1
      end if

      !---------------------------
      ! Get the header and check
      success = cb_list(i_compute) % get(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.) ! Extract header
      if (.not. success) then
        print *, 'ERROR when getting message header from CIO_OUT', i_compute
        error stop 1
      end if

      content_size = header % content_length

      ! call print_message_header(header)

      !------------------------------
      ! Extract/process message data
      if (header % command == MSG_COMMAND_DATA) then
        success = cb_list(i_compute) % get(record, model_record_size_int(), CB_KIND_INTEGER_4, .false.) ! Extract record 
        ! TODO  Get compression metadata
        ! TODO  Get other metadata

        if (.not. success) then
          print *, 'ERROR Could not get record from data message'
          error stop 1
        end if

        num_data_int = (record % data_size_byte + 3) / 4
        c_data = heap_list(i_compute) % get_address_from_offset(record % heap_offset)  ! Get proper pointer to data in shared memory
        call c_f_pointer(c_data, f_data, [num_data_int])                             ! Access it using a fortran pointer, for easy copy into the jar

        header % content_length = header % content_length + num_data_int             ! Update message header
        total_message_size = INT(message_header_size_int() + message_cap_size_int(), kind=4) + header % content_length

      else if (header % command == MSG_COMMAND_MODEL_STOP) then
        model_finished(i_compute) = .true.  ! Indicate this model won't be active anymore

        print '(A, I3, A, I2, A, I4)', 'DEBUG: Model ', i_compute, ' is finished, relay ', local_relay_id, ' (local), DCB client ', client_id

        total_message_size = INT(message_header_size_int() + message_cap_size_int(), kind=4)

      else if (header % command == MSG_COMMAND_CLOSE_FILE) then
        ! Do nothing, just send the header along
        total_message_size = INT(message_header_size_int() + message_cap_size_int(), kind=4)

      else if (header % command == MSG_COMMAND_OPEN_FILE) then
        filename_size = num_char_to_num_int(int(header % content_length, 4))

        if (.not. allocated(cb_message)) then
          allocate(cb_message(filename_size))
        else if (size(cb_message) < filename_size) then
          deallocate(cb_message)
          allocate(cb_message(filename_size))
        end if

        success = cb_list(i_compute) % get(cb_message, filename_size, CB_KIND_INTEGER_4, .true.) ! Extract file name
        total_message_size = INT(message_header_size_int() + message_cap_size_int() + filename_size, kind=4)

      else
        print *, 'ERROR: Unknown message type'
        call print_message_header(header)
        error stop 1
      end if

      !----------------------------------
      ! Check the end cap at this point
      success = cb_list(i_compute) % get(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.)
      if ((.not. success) .or. (content_size .ne. end_cap % msg_length) .or. (end_cap % cap_tag .ne. MSG_CAP_TAG)) then
        print *, 'ERROR We have a problem with message size (end cap does not match)'
        call print_message_header(header)
        print *, end_cap % cap_tag, end_cap % msg_length, success, content_size
        error stop 1
      end if

      !------------------------------------
      ! If the DCB message buffer is too full to contain that new package, flush it now
      if (dcb_message_jar % high() + total_message_size > dcb_message_buffer_size) then
        print *, 'Sending data ', dcb_message_jar % high()
        success = data_buffer % put_elems(dcb_message, dcb_message_jar % high(), CB_DATA_ELEMENT_KIND, .true.)
        call dcb_message_jar % reset()

        if (.not. success) then
          print *, 'ERROR sending message from relay to server!'
          error stop 1
        end if
      end if

      !-----------------------------
      ! Copy message header

      header % relay_global_rank = context % get_global_rank()
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
          error stop 1
        end if

      else if (header % command == MSG_COMMAND_OPEN_FILE) then

        num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, cb_message(1:filename_size))

      end if

      !---------------------
      ! Put message end cap
      end_cap % msg_length = header % content_length
      num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, end_cap)

    end do
  end do

  ! Send the remaining data
  print *, 'Sending remaining data: ', dcb_message_jar % high()
  success = data_buffer % put_elems(dcb_message, dcb_message_jar % high(), CB_DATA_ELEMENT_KIND, .true.)

  if (.not. success) then
    print *, 'ERROR sending remaining data!'
    error stop 1
  end if

  if (allocated(cb_message)) deallocate(cb_message)

  if (local_relay_id == 0) then
    ! call heap_list(0) % dumpinfo()
    ! do i_compute = 0, num_local_compute - 1
    !   call cb_list(i_compute) % print_stats(client_id * 100 + i_compute, i_compute == 0)
    ! end do
  end if

end subroutine server_bound_relay_process

subroutine model_bound_relay_process(context)
  implicit none
  type(ioserver_context), intent(inout) :: context

  if (context % has_debug_mode()) print *, 'Model-bound relay process'
end subroutine model_bound_relay_process

subroutine pseudo_model_process(context)
  use mpi_f08

  use heap_module
  use ioserver_message_module
  use rpn_extra_module, only: sleep_us
  implicit none

  type(ioserver_context), intent(inout) :: context

  type(model_stream)    :: output_stream_1
  type(circular_buffer) :: data_buffer
  logical :: success

  print *, 'Using default pseudo-model function. This does not do much.'

  output_stream_1 = context % open_stream_model('pseudo_model_results_1')
  if (.not. output_stream_1 % is_open()) then
    print *, 'Unable to open model file 1 !!!!'
    error stop 1
  end if

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid()) then
    print *, 'ERROR: CB received from context is not valid!'
    error stop 1
  end if

  success = output_stream_1 % close()
  if (.not. success) then
    print *, 'Unable to close model file!!!!'
    error stop 1
  end if
end subroutine pseudo_model_process

end module run_model_node_module
