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

subroutine run_model_node(num_relay_per_node, do_expensive_checks_in)
  implicit none
  integer, intent(in) :: num_relay_per_node
  logical, optional, intent(in) :: do_expensive_checks_in

  type(ioserver_context) :: context
  logical :: success
  logical :: do_expensive_checks
  
  do_expensive_checks = .false.
  if (present(do_expensive_checks_in)) do_expensive_checks = do_expensive_checks_in

  success = context % init(.false., num_relay_per_node, 0, 0, in_debug_mode = do_expensive_checks)

  if (.not. success) then
    print *, 'ERROR: could not initialize IO-server context for a model node!'
    error stop 1
  end if

  if (context % is_model()) then
    call pseudo_model_process(context)
  else if (context % is_relay()) then
    if (context % is_server_bound()) then
      call server_bound_relay_process(context, do_expensive_checks)
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

subroutine server_bound_relay_process(context, do_expensive_checks)
  use ISO_C_BINDING

  use circular_buffer_module
  use ioserver_data_check_module
  use ioserver_message_module
  use jar_module
  implicit none

  type(ioserver_context), intent(inout) :: context
  logical,                intent(in)    :: do_expensive_checks

  integer, parameter :: MAX_DCB_MESSAGE_SIZE_INT = 50000

  type(circular_buffer), dimension(:), pointer :: cb_list
  type(heap),            dimension(:), pointer :: heap_list
  type(distributed_circular_buffer) :: data_buffer
  type(comm_rank_size)  :: node_crs, local_relay_crs, all_crs
  integer :: client_id, local_relay_id
  integer :: i_compute
  integer :: num_local_compute, num_local_relays
  integer :: dcb_message_buffer_size
  integer(C_INT64_T) :: dcb_capacity

  integer :: i_data_check
  integer :: total_message_size, model_message_size, end_cap, content_size
  logical :: finished, success, skip_message
  integer, dimension(:), allocatable :: cb_message
  integer, dimension(:), allocatable :: expected_message

  integer, dimension(:), pointer :: f_data
  type(C_PTR) :: c_data
  integer :: num_data
  integer :: h_status

  type(model_record)   :: record
  type(message_header) :: header
  type(jar)            :: dcb_message_jar
  integer              :: jar_ok, num_jar_elem
  integer, dimension(:), pointer :: dcb_message
  logical, dimension(:), allocatable :: model_finished

  print *, 'Server-bound relay process'

  cb_list     => context % get_server_bound_cb_list()
  heap_list   => context % get_heap_list()
  data_buffer =  context % get_dcb()

  node_crs        = context % get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
  local_relay_crs = context % get_crs(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR)

  local_relay_id    = local_relay_crs % rank
  num_local_relays  = local_relay_crs % size
  num_local_compute = context % get_num_local_model()

  ! print *, 'RELAY, local relay id: ', local_relay_id
  all_crs = context % get_crs(NO_COLOR)
  print '(A, I5)', 'RELAY, global ', all_crs % rank

  client_id = data_buffer % get_server_bound_client_id()
  dcb_capacity            = data_buffer % get_capacity(CB_KIND_INTEGER_4)
  dcb_message_buffer_size = min(int(dcb_capacity, kind=4) / 4, MAX_DCB_MESSAGE_SIZE_INT)

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
  header % length  = int(message_header_size_int(), kind=4)
  header % command = MSG_COMMAND_DUMMY
  success = data_buffer % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)
  success = data_buffer % put_elems(header % length, 1_8, CB_KIND_INTEGER_4, .true.) .and. success ! Append size

  if (.not. success) then
    print *, 'ERROR saying HI to the consumer...'
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
    do i_compute = 0, num_local_compute - 1, num_local_relays
      skip_message = .false.

      ! print *, 'num elements: ', cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_4), i_compute

      if (model_finished(i_compute)) cycle  ! This model buffer is done, move on
      finished = .false.                    ! This model buffer is not finished yet, keep the loop active
      if (cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_4) == 0) cycle ! The buffer is empty, move on to the next

      ! From this point on, we know there is something in the buffer
      success = cb_list(i_compute) % peek(model_message_size, 1_8, CB_KIND_INTEGER_4)

      if (model_message_size < message_header_size_int()) then
        print *, 'AAAHHHHhhh message size is smaller than a message header!', model_message_size, message_header_size_int()
      end if

      success = cb_list(i_compute) % get(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.) ! Header
      if (.not. success) then
        print *, 'ERROR when getting message header from CIO_OUT', i_compute
        error stop 1
      end if

      ! call message_header_print(header)

      if (header % command == MSG_COMMAND_DATA) then
        success = cb_list(i_compute) % get(record, model_record_size_int(), CB_KIND_INTEGER_4, .false.) ! Record 
        success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.) ! End cap

        if (.not. success) then
          print *, 'ERROR Could not get record from data message'
          error stop 1
        end if

        if (record % record_length .ne. end_cap .or. (record % record_length .ne. header % length)) then
          print *, 'We have a problem with message size (end cap does not match)'
          error stop 1
        end if

        ! TODO take compression meta and other meta stuff into account

        c_data = context % ptr_translate_from(record % data, MODEL_COLOR, i_compute)
        num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size into account!!!!
        call c_f_pointer(c_data, f_data, [num_data])
        
        ! print *, 'f_data: ', f_data, num_data
        ! print '(A12, I4, I5, I3, I4, I4, I4, I4, I4, I4, I4, I3, I3, I4, I4)', 'Record info: ', &
        !   record % record_length, record % tag, record % stream, &
        !   record % ni, record % nj, record % gnignj, &
        !   record % gin, record % gout, record % i0, record % j0, &
        !   record % nk, record % nvar, record % csize, record % msize

        if (do_expensive_checks) then

          if (.not. allocated(expected_message)) then
            allocate(expected_message(num_data))
          else if (size(expected_message) < num_data) then
            deallocate(expected_message)
            allocate(expected_message(num_data))
          end if

          do i_data_check = 1, num_data
            expected_message(i_data_check) = compute_data_point(header % sender_global_rank, record % tag, i_data_check)
          end do
          ! if (i_compute == 0) then
          !   print *, 'Got ', f_data(1:3), transfer(record % data, tmp)
          ! end if
          if (.not. all(expected_message(1:num_data) == f_data(:))) then
            print *, 'Expected: ', expected_message(1:num_data / 100 + 3)
            print *, 'Received: ', f_data(:num_data / 100 + 3)
            print *, 'i_compute, tag: ', i_compute, record % tag
            error stop 1
          end if
        end if

        record % record_length = record % record_length + num_data
        header % length = record % record_length
        total_message_size = INT(message_header_size_int(), kind=4) + record % record_length + 1

      else if (header % command == MSG_COMMAND_MODEL_STOP) then
        model_finished(i_compute) = .true.
        print *, 'Model is finished ', i_compute

      else if (header % command == MSG_COMMAND_CLOSE_FILE) then
        ! Do nothing, just send the header along
      else
        content_size = header % length
        if (header % command == MSG_COMMAND_OPEN_FILE) content_size = num_char_to_num_int(header % length)

        total_message_size = INT(message_header_size_int(), kind=4) + content_size + 1
      end if

      ! If the DCB message buffer is too full to contain that new package, flush it now
      if (dcb_message_jar % high() + total_message_size > dcb_message_buffer_size) then
        print *, 'Sending data ', dcb_message_jar % high()
        success = data_buffer % put_elems(dcb_message, INT(dcb_message_jar % high(), C_SIZE_T), CB_KIND_INTEGER_4, .true.)
        call dcb_message_jar % reset()

        if (.not. success) then
          print *, 'ERROR sending message from relay to server!'
          error stop 1
        end if
      end if

      ! Copy the message header
      ! call message_header_print(header)
      num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header)

      if (header % command == MSG_COMMAND_DATA) then
        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, record)                 ! Data header
        !TODO compression and other meta
        num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, f_data(:))             ! The data
        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, record % record_length) ! End cap

        f_data(2) = -1
        h_status = heap_list(i_compute) % free(c_data)
        if (h_status .ne. 0) then
          print*, 'ERROR: Unable to free heap data (from RELAY)'
          error stop 1
        end if

      else if (header % command == MSG_COMMAND_MODEL_STOP) then
        success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.)
        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header % length) ! Sending it along, gotta put the end cap

      else if (header % command == MSG_COMMAND_CLOSE_FILE) then
        success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.)
        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header % length) ! Sending it along, gotta put the end cap

      else

        if (.not. allocated(cb_message)) then
          allocate(cb_message(content_size))
        else if (size(cb_message) < content_size) then
          deallocate(cb_message)
          allocate(cb_message(content_size))
        end if

        success = cb_list(i_compute) % get(cb_message, int(content_size, kind=8), CB_KIND_INTEGER_4, .true.)
        success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.) ! End cap
        num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, cb_message(1:content_size))
        ! print *, '(cmd d)  Jar high = ', dcb_message_jar % high()
        num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header % length)
        ! print *, '(cmdlen) Jar high = ', dcb_message_jar % high()
      end if
    end do
  end do

  ! Send the remaining data
  print *, 'Sending remaining data: ', dcb_message_jar % high()
  success = data_buffer % put_elems(dcb_message, INT(dcb_message_jar % high(), C_SIZE_T), CB_KIND_INTEGER_4, .true.)

  if (.not. success) then
    print *, 'ERROR sending remaining data!'
    error stop 1
  end if

  if (allocated(cb_message)) deallocate(cb_message)

  if (local_relay_id == 0) then
    call heap_list(0) % dumpinfo()
    do i_compute = 0, num_local_compute - 1
      call cb_list(i_compute) % print_stats(client_id * 100 + i_compute, i_compute == 0)
    end do
  end if

end subroutine server_bound_relay_process

subroutine model_bound_relay_process(context)
  implicit none
  type(ioserver_context), intent(inout) :: context

  print *, 'Model-bound relay process'
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
  type(model_stream)    :: output_file
  type(circular_buffer) :: data_buffer
  type(comm_rank_size)  :: model_crs, node_crs, local_compute_crs, all_crs

  type(subgrid)    :: local_grid
  type(grid)       :: input_grid, output_grid
  type(block_meta) :: data_array_info
  integer(kind=4), dimension(:), pointer :: data_array

  integer :: global_rank
  integer :: local_compute_id

  logical :: success

  node_heap = context % get_local_heap()

  output_file = context % open_file_model('model_write_results_')
  if (.not. output_file % is_open()) then
    print *, 'Unable to open model file!!!!'
    error stop 1
  end if

  model_crs = context % get_crs(MODEL_COLOR)

  node_crs          = context % get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  local_compute_crs = context % get_crs(NODE_COLOR + MODEL_COLOR)
  local_compute_id  = local_compute_crs % rank

  all_crs = context % get_crs(NO_COLOR)
  global_rank = all_crs % rank

  ! print *, 'MODEL, local compute id: ', local_compute_id
  print '(A, I5)', 'MODEL, global ', global_rank

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid()) then
    print *, 'ERROR: CB received from context is not valid!'
    error stop 1
  end if

  ! Init area info
  local_grid % i0 = local_compute_id * CB_MESSAGE_SIZE_INT + 1
  local_grid % ni = CB_MESSAGE_SIZE_INT
  local_grid % j0 = 1 ! cause ARRAYS START AT 1
  local_grid % nj = 1
  local_grid % nk = 1
  local_grid % nv = 1

  input_grid % id = 1
  input_grid % size_i = CB_MESSAGE_SIZE_INT * model_crs % size
  input_grid % size_j = 1

  output_grid % id = 1

  ! call sleep_us(5000)
  block
    integer :: i, j
    do i = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT
      ! Get memory and put data in it. Try repeatedly if it failed.
      data_array_info = node_heap % allocate(data_array, [CB_MESSAGE_SIZE_INT])
      do while (.not. associated(data_array))
        call sleep_us(10)
        data_array_info = node_heap % allocate(data_array, [CB_MESSAGE_SIZE_INT])
      end do

      ! Using i + 2 b/c tag is incremented when opening a file
      do j = 1, CB_MESSAGE_SIZE_INT
        data_array(j) = compute_data_point(global_rank, i + 1, j)
      end do

      ! Write the data to a file (i.e. send it to the server to do that for us)
      success = output_file % write(data_array_info, local_grid, input_grid, output_grid)

      if (.not. success) then
        print *, 'ERROR while trying to do a WRITE'
        error stop 1
      end if

      call sleep_us(50)
    end do
  end block

  success = output_file % close() ! TODO implement that function so that it can send the stop signal
  if (.not. success) then
    print *, 'Unable to close model file!!!!'
    error stop 1
  end if
end subroutine pseudo_model_process

end module run_model_node_module
