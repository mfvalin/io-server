module ioserver_data_check_module
  implicit none
  integer, parameter :: CB_MESSAGE_SIZE_INT       = 500                      ! Size of each data batch put in a CB
  integer, parameter :: CB_TOTAL_DATA_TO_SEND_INT = 2000                     ! How much total data to send (for each CB)
contains
  function compute_data_point(compute_rank, tag, index) result(data_point)
    implicit none
    integer, intent(in) :: compute_rank
    integer, intent(in) :: tag
    integer, intent(in) :: index
    integer :: data_point

    data_point = mod(compute_rank, 1000) * 1000000 + mod(tag, 1000) * 1000 + mod(index, 800) + tag / 1000
  end function compute_data_point
end module ioserver_data_check_module

#include <serializer.hf>

module run_model_node_module
  use ioserver_context_module
  implicit none
  private

  public :: run_model_node

contains

subroutine run_model_node(num_relay_per_node, use_debug_mode_in)
  implicit none
  integer, intent(in) :: num_relay_per_node
  logical, optional, intent(in) :: use_debug_mode_in

  type(ioserver_context) :: context
  logical :: success
  logical :: use_debug_mode
  
  use_debug_mode = .false.
  if (present(use_debug_mode_in)) use_debug_mode = use_debug_mode_in

  success = context % init(.false., num_relay_per_node, 0, 0, 0, in_debug_mode = use_debug_mode)

  if (.not. success) then
    print *, 'ERROR: could not initialize IO-server context for a model node!'
    error stop 1
  end if

  if (context % is_model()) then
    call pseudo_model_process(context)
  else if (context % is_relay()) then
    if (context % is_server_bound()) then
      call server_bound_relay_process(context)
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
  use ioserver_data_check_module
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

  integer :: total_message_size, content_size
  integer(C_INT64_T) :: filename_size
  logical :: finished, success, skip_message
  integer, dimension(:), allocatable :: cb_message
  integer(C_INT) :: message_header_tag

  integer, dimension(:), pointer :: f_data
  type(C_PTR) :: c_data
  integer :: num_data_int
  integer :: h_status

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
  dcb_message_buffer_size = min(int(dcb_capacity, kind=4) / 4, MAX_DCB_MESSAGE_SIZE_INT) - 10  ! Make sure we have a bit of loose space

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
        c_data = context % ptr_translate_from(record % data, MODEL_COLOR, i_compute) ! Get proper pointer to data in shared memory
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
        filename_size = num_char_to_num_int(header % content_length)

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

        
        h_status = heap_list(i_compute) % free(c_data)            ! Free the shared memory
        if (h_status .ne. 0) then
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
  use ioserver_data_check_module
  use ioserver_message_module
  use rpn_extra_module, only: sleep_us
  implicit none

  type(ioserver_context), intent(inout) :: context

  type(heap)            :: node_heap
  type(model_stream)    :: output_stream_1, output_stream_2
  type(circular_buffer) :: data_buffer
  type(comm_rank_size)  :: model_crs, node_crs, global_compute_crs

  type(subgrid_t)  :: local_grid
  type(grid_t)     :: input_grid, output_grid
  type(block_meta) :: data_array_info_1, data_array_info_2
  integer(kind=4), dimension(:), pointer :: data_array_1, data_array_2

  integer :: global_rank
  integer :: global_compute_id

  logical :: success

  node_heap = context % get_local_heap()

  output_stream_1 = context % open_stream_model('pseudo_model_results_1')
  if (.not. output_stream_1 % is_open()) then
    print *, 'Unable to open model file 1 !!!!'
    error stop 1
  end if

  output_stream_2 = context % open_stream_model('pseudo_model_results_2')
  if (.not. output_stream_2 % is_open()) then
    print *, 'Unable to open model file 2 !!!!'
    error stop 1
  end if

  model_crs = context % get_crs(MODEL_COLOR)

  node_crs           = context % get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  global_compute_crs = context % get_crs(MODEL_COLOR)
  global_compute_id  = global_compute_crs % rank

  global_rank = context % get_global_rank()

  ! print '(A, I5)', 'DEBUG: MODEL, global ', global_rank

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid()) then
    print *, 'ERROR: CB received from context is not valid!'
    error stop 1
  end if

  ! Init area info
  local_grid % offset(1) = global_compute_id * CB_MESSAGE_SIZE_INT + 1
  local_grid % size(1)   = CB_MESSAGE_SIZE_INT

  input_grid % id = 1
  input_grid % size(1) = CB_MESSAGE_SIZE_INT * model_crs % size
  input_grid % size(2) = 1
  input_grid % elem_size = 4

  output_grid % id = 1

  ! print *, 'Created local grid', local_grid % i0, local_grid % i0 + local_grid % ni

  ! call sleep_us(5000)
  block
    integer :: i, j, current_tag
    current_tag = 1
    do i = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT
      !------------------------
      ! First stream
    
      current_tag = current_tag + 2

      ! Get memory and put data in it. Try repeatedly if it failed.
      data_array_info_1 = node_heap % allocate(data_array_1, [CB_MESSAGE_SIZE_INT])
      do while (.not. associated(data_array_1))
        call sleep_us(10)
        data_array_info_1 = node_heap % allocate(data_array_1, [CB_MESSAGE_SIZE_INT])
      end do

      ! Using i + 2 b/c tag is incremented when opening a file
      do j = 1, CB_MESSAGE_SIZE_INT
        data_array_1(j) = compute_data_point(global_rank, current_tag, j)
      end do

      ! Write the data to a file (i.e. send it to the server to do that for us)
      success = output_stream_1 % write(data_array_info_1, local_grid, input_grid, output_grid)

      if (.not. success) then
        print *, 'ERROR: Write into model stream failed'
        error stop 1
      end if

      !------------------------
      ! Second stream
      data_array_info_2 = node_heap % allocate(data_array_2, [CB_MESSAGE_SIZE_INT])
      do while (.not. associated(data_array_2))
        call sleep_us(10)
        data_array_info_2 = node_heap % allocate(data_array_2, [CB_MESSAGE_SIZE_INT])
      end do

      ! Using i + 2 b/c tag is incremented when opening a file
      do j = 1, CB_MESSAGE_SIZE_INT
        data_array_2(j) = compute_data_point(global_rank, current_tag + 1, j)
      end do

      success = output_stream_2 % write(data_array_info_2, local_grid, input_grid, output_grid)
      if (.not. success) then
        print *, 'ERROR while trying to do a WRITE'
        error stop 1
      end if

      call sleep_us(50)
    end do
  end block

  success = output_stream_1 % close()
  success = output_stream_2 % close() .and. success
  if (.not. success) then
    print *, 'Unable to close model file!!!!'
    error stop 1
  end if
end subroutine pseudo_model_process

end module run_model_node_module
