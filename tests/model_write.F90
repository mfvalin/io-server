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

module model_write_parameters
  use iso_c_binding
  use ioserver_mpi
  implicit none

  integer, parameter :: CB_MESSAGE_SIZE_INT       = 16384                     !< Size of each data batch put in a CB. Must be divisible by 16 for the tests
  integer, parameter :: CB_TOTAL_DATA_TO_SEND_INT = CB_MESSAGE_SIZE_INT * 16  !< How much total data to send (for each CB)

  integer, parameter :: block_width  = CB_MESSAGE_SIZE_INT / 4
  integer, parameter :: block_height = 4

  integer, save :: num_compute_x
  integer, save :: num_compute_y

  integer(C_INT64_T), parameter :: dim_x = 10
  integer(C_INT64_T), parameter :: dim_y = 37
  integer(C_INT64_T), parameter :: dim_z = 40
  integer(C_INT64_T), parameter :: num_vars = 3
  integer(C_INT64_T), parameter :: num_time_steps = 2

  character(len=*), parameter :: filename1 = 'pseudo_model_1'
  character(len=*), parameter :: filename2 = 'pseudo_model_2'

contains

  function compute_int4_data_point(compute_rank, tag, index) result(data_point)
    implicit none
    integer, intent(in) :: compute_rank
    integer, intent(in) :: tag
    integer, intent(in) :: index
    integer :: data_point

    data_point = mod(compute_rank, 1000) * 1000000 + mod(tag, 1000) * 1000 + mod(index, 800) + tag / 1000
  end function compute_int4_data_point

  function compute_real8_data_point(compute_id, tag, i, j, k, l, m) result(data_point)
    implicit none
    integer, intent(in) :: compute_id
    integer, intent(in) :: tag
    integer, intent(in) :: i, j
    integer, intent(in), optional :: k, l, m

    real(kind=8) :: data_point

    data_point = tag / 100 + mod(compute_id, 100) * 100 + mod(i, 1000) * 100000 + mod(j, 100) * 10000000
    if (present(k)) data_point = data_point + mod(k, 10) * 100000000
    if (present(l)) data_point = data_point + mod(l, 10) * 1000000000_8
    if (present(m)) data_point = data_point + mod(m, 10) * 10000000000_8
  end function compute_real8_data_point

  function am_server_node(node_rank, node_size, single_node, num_nodes)
    implicit none

    integer, intent(out) :: node_rank, node_size
    logical, intent(out) :: single_node
    integer, intent(out) :: num_nodes
    logical :: am_server_node

    integer :: node_comm, first_rank_comm
    integer :: global_rank, node_root_global_rank
    integer :: global_size
    integer :: ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm, ierr)
    call MPI_Comm_rank(node_comm, node_rank, ierr)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm, ierr)

    call MPI_Comm_size(MPI_COMM_WORLD, global_size, ierr)
    call MPI_Comm_size(node_comm, node_size, ierr)

    am_server_node = (node_root_global_rank == 0)
    single_node = (global_size == node_size)

    if (node_rank == 0) then
      call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, first_rank_comm, ierr)
      call MPI_Comm_size(first_rank_comm, num_nodes, ierr)
    else
      call MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, first_rank_comm, ierr)
      num_nodes = -1
    end if
  end function am_server_node

  function pseudo_model_process(context) result(model_success)
    use circular_buffer_module
    use ioserver_context_module
    use ioserver_message_module
    use jar_module
    use process_command_module
    use rpn_extra_module, only: sleep_us
    use shmem_heap_module
    implicit none

#include <rmn/serializer.hf>

    type(ioserver_context), intent(inout) :: context
    logical :: model_success

    type(shmem_heap)      :: node_heap
    type(circular_buffer) :: model_cb
    type(model_stream), pointer :: output_stream_1, output_stream_2
    type(circular_buffer) :: data_buffer
    type(comm_rank_size)  :: model_crs, node_crs

    type(grid_bounds_t) :: local_grid, big_local_grid
    type(grid_bounds_t) :: global_grid, big_global_grid
    type(block_meta)    :: data_array_info_1, data_array_info_2, big_array_info
    integer(kind=4), dimension(:,:), contiguous, pointer :: data_array_4
    integer(kind=8), dimension(:,:), contiguous, pointer :: data_array_8
    real(kind=8), dimension(:,:,:,:,:), contiguous, pointer :: big_array

    type(command_header) :: c_header

    integer :: global_model_id
    integer :: compute_width, compute_height

    type(jar) :: command_jar

    logical :: success

    model_success = .false.

    node_heap = context % get_local_heap()
    model_cb  = context % get_server_bound_cb()
    success = command_jar % new(100)

    ! Open and check stream 1
    call context % open_stream_model(output_stream_1)
    if (.not. success .or. .not. associated(output_stream_1)) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR: Could not open model stream'
      return
    end if 

    ! Send "open file" command into stream 1
    c_header % command_type = COMMAND_TYPE_OPEN_FILE
    c_header % size_bytes   = len_trim(filename1)
    success = JAR_PUT_ITEM(command_jar, c_header)        .and. success
    success = JAR_PUT_ITEM(command_jar, trim(filename1)) .and. success
    success = output_stream_1 % send_command(command_jar)
    if (.not. success) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR: Unable to send command to model stream 1 !!!!'
      return
    end if

    ! Open and check stream 2
    call context % open_stream_model(output_stream_2)
    if (.not. associated(output_stream_2)) then
      print '(A, A)', context % get_short_pe_name(), ' ERROR: Could not open second model stream'
      return
    end if

    ! Send "open file" command into stream 2
    call command_jar % reset()
    c_header % command_type = COMMAND_TYPE_OPEN_FILE
    c_header % size_bytes   = len_trim(filename2)
    success = JAR_PUT_ITEM(command_jar, c_header)        .and. success
    success = JAR_PUT_ITEM(command_jar, trim(filename2)) .and. success
    success = output_stream_2 % send_command(command_jar) .and. success
    if (.not. success .or. .not. output_stream_2 % is_open()) then
      print *, 'Unable to open model file 2 !!!!'
      return
    end if

    model_crs = context % get_crs(MODEL_COLOR)

    node_crs         = context % get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
    global_model_id  = model_crs % rank

    data_buffer = context % get_server_bound_cb()
    if (.not. data_buffer % is_valid()) then
      print *, 'ERROR: CB received from context is not valid!'
      return
    end if

    if (context % get_debug_level() >= 1 .and. model_crs % rank == 0) then
      print '(A, I8, A)', 'INFO: We are using ', model_crs % size, ' pseudo-model processes'
    end if

    ! Init area info
    if (mod(model_crs % size, 4) == 0) then
      compute_width  = model_crs % size / 4
      compute_height = 4
    else if (mod(model_crs % size, 2) == 0) then
      compute_width  = model_crs % size / 2
      compute_height = 2
    else
      compute_width  = model_crs % size
      compute_height = 1
    end if

    ! For checking later
    num_compute_x = compute_width
    num_compute_y = compute_height

    if (model_crs % rank == 0) then
      block
        type(message_header) :: header
        type(message_cap)    :: end_cap
        header % command = MSG_COMMAND_USER
        header % sender_global_rank = context % get_global_rank()
        header % content_size_int8 = 2
        end_cap % msg_length = header % content_size_int8

        success = model_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = 0)
        success = model_cb % put(num_compute_x, 1_8, CB_KIND_INTEGER_4, .false., timeout_ms = 0)          .and. success
        success = model_cb % put(num_compute_y, 1_8, CB_KIND_INTEGER_4, .false., timeout_ms = 0)          .and. success
        success = model_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = 0)  .and. success

        if (.not. success) then
          print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Unable to send user command...'
          return
        end if
      end block
    end if

    block
      integer :: min_x, min_y
      min_x = mod(global_model_id, compute_width) * block_width + 1
      min_y = (global_model_id / compute_width) * block_height + 1
      call local_grid % set_min(min_x, min_y)
      call local_grid % set_size(block_width, block_height)

      call global_grid % set_min(1, 1)    ! Not actually necessary, the default value is already 1
      call global_grid % set_size(block_width * compute_width, block_height * compute_height)

      min_x = mod(global_model_id, compute_width) * dim_x + 1
      min_y = (global_model_id / compute_width) * dim_y + 1
      call big_local_grid % set_min(min_x, min_y)
      call big_local_grid % set_size(dim_x, dim_y, dim_z, num_vars, num_time_steps)

      call big_global_grid % set_size(dim_x * compute_width, dim_y * compute_height, dim_z, num_vars, num_time_steps)

      if (model_crs % rank == 0) then
        print '(A, I7, A, I3)', 'Blocks:  ', block_width, 'x', block_height
        print '(A, I7, A, I3)', 'Compute: ', compute_width, 'x', compute_height
        print '(A, I3, A)', 'Using grids for ', model_crs % size, ' PEs'
        call local_grid % print()
        call global_grid % print()
        call big_local_grid % print()
        call big_global_grid % print()
      end if
    end block

    ! call sleep_us(5000)
    block
      type(model_grid) :: m_grid
      integer :: i_msg, i_data, j_data, current_tag
      integer(C_INT64_T), dimension(2) :: array_dims
      integer(C_INT64_T), dimension(5) :: big_array_dims
      integer :: i, j, k, l, m
      integer :: data_size_divisor

      data_size_divisor = 1
      if (model_crs % size >= 200) data_size_divisor = 4
      array_dims = [block_width, block_height]
      big_array_dims = [dim_x, dim_y, dim_z, num_vars, num_time_steps]
      current_tag = 0
      do i_msg = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT / data_size_divisor
        !------------------------
        ! First stream
      
        current_tag = current_tag + 3

        ! Get memory and put data in it. Try repeatedly if it failed.
        data_array_info_1 = node_heap % allocate(data_array_4, array_dims)
        do while (.not. associated(data_array_4))
          call sleep_us(10)
          data_array_info_1 = node_heap % allocate(data_array_4, array_dims)
        end do

        ! Put data in the array
        do i_data = 1, CB_MESSAGE_SIZE_INT/4
          do j_data = 1, 4
            data_array_4(i_data,j_data) = compute_int4_data_point(global_model_id, current_tag, (j_data-1) * CB_MESSAGE_SIZE_INT / 4 + i_data)
          end do
        end do

        ! Command header
        call command_jar % reset()
        c_header % command_type = COMMAND_TYPE_WRITE_DATA
        c_header % size_bytes   = (storage_size(m_grid) + 7) / 8
        success = JAR_PUT_ITEM(command_jar, c_header)

        ! Grid metadata (for model processing)
        m_grid % dims      = global_grid % get_size_val()
        m_grid % elem_size = data_array_info_1 % get_kind()
        success = JAR_PUT_ITEM(command_jar, m_grid) .and. success

        ! Write the data to a file (i.e. send it to the server to do that for us)
        success = output_stream_1 % send_data(data_array_info_1, local_grid, global_grid, command = command_jar) .and. success

        if (.not. success) then
          print *, 'ERROR: Send data into model stream failed'
          return
        end if

        !------------------------
        ! Second stream
        data_array_info_2 = node_heap % allocate(data_array_8, array_dims)
        do while (.not. associated(data_array_8))
          call sleep_us(10)
          data_array_info_2 = node_heap % allocate(data_array_8, array_dims)
        end do

        big_array_info = node_heap % allocate(big_array, big_array_dims)
        do while (.not. associated(big_array))
          call sleep_us(10)
          big_array_info = node_heap % allocate(big_array, big_array_dims)
        end do

        ! Put data in the arrays
        do i_data = 1, CB_MESSAGE_SIZE_INT / 4
          do j_data = 1, 4
            data_array_8(i_data,j_data) = compute_int4_data_point(global_model_id, current_tag + 1, (j_data-1) * CB_MESSAGE_SIZE_INT / 4 + i_data)
          end do
        end do

        do m = 1, num_time_steps
          do l = 1, num_vars
            do k = 1, dim_z
              do j = 1, dim_y
                do i = 1, dim_x
                  big_array(i, j, k, l, m) = compute_real8_data_point(global_model_id, current_tag + 2, i, j, k, l, m)
                end do
              end do
            end do
          end do
        end do

        ! Command header
        call command_jar % reset()
        c_header % command_type = COMMAND_TYPE_WRITE_DATA
        c_header % size_bytes   = (storage_size(m_grid) + 7) / 8
        success = JAR_PUT_ITEM(command_jar, c_header)

        ! Grid metadata (for model processing)
        m_grid % dims      = global_grid % get_size_val()
        m_grid % elem_size = data_array_info_2 % get_kind()
        success = JAR_PUT_ITEM(command_jar, m_grid) .and. success

        success = output_stream_2 % send_data(data_array_info_2, local_grid, global_grid, command = command_jar) .and. success

        ! Command header
        call command_jar % reset()
        c_header % command_type = COMMAND_TYPE_WRITE_DATA
        c_header % size_bytes   = (storage_size(m_grid) + 7) / 8
        success = JAR_PUT_ITEM(command_jar, c_header) .and. success

        ! Grid metadata (for model processing)
        m_grid % dims      = big_global_grid % get_size_val()
        m_grid % elem_size = big_array_info % get_kind()
        success = JAR_PUT_ITEM(command_jar, m_grid) .and. success

        success = output_stream_2 % send_data(big_array_info, big_local_grid, big_global_grid, command = command_jar) .and. success
        if (.not. success) then
          print *, 'ERROR while trying to do a SEND DATA'
          return
        end if

        call sleep_us(5000)
      end do
    end block

    ! Close files
    call command_jar % reset()
    c_header % command_type = COMMAND_TYPE_CLOSE_FILE
    c_header % size_bytes = 0
    success = JAR_PUT_ITEM(command_jar, c_header)
    success = output_stream_1 % send_command(command_jar) .and. success
    success = output_stream_2 % send_command(command_jar) .and. success

    ! Close streams
    success = output_stream_1 % close() .and. success
    success = output_stream_2 % close() .and. success
    if (.not. success) then
      print *, 'Unable to close model file!!!!'
      return
    end if

    model_success = .true.
  end function pseudo_model_process

  !> Verify that one of the files written during the test contains the proper values, with
  !> the correct structure
  subroutine check_result()
    implicit none

    integer(C_INT64_T), dimension(2) :: dims1
    integer(C_INT64_T), dimension(5) :: dims2

    integer(C_INT8_T), dimension(:,:),       allocatable, target :: read1 ! Array used to read from file (must be bytes)
    integer(C_INT8_T), dimension(:,:,:,:,:), allocatable, target :: read2 ! Array used to read from file (must be bytes)

    integer(C_INT64_T), dimension(:,:),       pointer :: array1 ! Array used to compare values (must be the value type)
    real(kind=8),       dimension(:,:,:,:,:), pointer :: array2 ! Array used to compare values (must be the value type)

    type(C_PTR) :: tmp_ptr ! To convert between array types and shapes

    integer :: file_unit
    integer :: model_id, tag
    integer :: i_msg
    integer :: i_comp, j_comp
    integer :: start_i, end_i, start_j, end_j
    integer :: i, j, k, l, m
    real(kind=8) :: expected, val
    integer :: data_size_divisor

    print '(A)', 'Checking written file'

    dims1(1) = block_width * num_compute_x
    dims1(2) = block_height * num_compute_y

    dims2(1) = dim_x * num_compute_x
    dims2(2) = dim_y * num_compute_y
    dims2(3) = dim_z
    dims2(4) = num_vars
    dims2(5) = num_time_steps

    ! Allocate reading space
    allocate(read1(dims1(1) * 8, dims1(2)))
    allocate(read2(dims2(1) * 8, dims2(2), dims2(3), dims2(4), dims2(5)))

    ! Set up pointer for right type access
    tmp_ptr = c_loc(read1)
    call c_f_pointer(tmp_ptr, array1, dims1)

    tmp_ptr = c_loc(read2)
    call c_f_pointer(tmp_ptr, array2, dims2)

    open(newunit = file_unit, file = trim(filename2), status = 'old', form = 'unformatted', action = 'read')

    data_size_divisor = 1
    if (num_compute_x * num_compute_y >= 200) data_size_divisor = 4
    print *, 'num_model_pes = ', num_compute_x * num_compute_y, num_compute_x, num_compute_y
    tag = 2
    do i_msg = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT / data_size_divisor ! Loop through all pairs of records in this file
      read(unit=file_unit) read1 ! First one won't be checked
      read(unit=file_unit) read2 ! This is the one we are interested in
      ! print *, array1(:,1)

      tag = tag + 3 ! Tags skip three at each iteration. One for the first file (not even considered in this check), and another for the array being ignored
      model_id = -1

      ! print *, "Checking tag ", tag

      do j_comp = 1, num_compute_y                  ! Number of model PEs in the vertical direction
        start_j = (j_comp - 1) * int(dim_y,4) + 1   ! Starting index within the global array (not just this PE's block)
        end_j = start_j + int(dim_y,4) - 1
        do i_comp = 1, num_compute_x                ! Number of model PEs in the horizontal direction
          model_id = model_id + 1
          start_i = (i_comp - 1) * int(dim_x,4) + 1 ! Starting index within the global array (not just this PE's block)
          end_i   = start_i + int(dim_x,4) - 1
          do m = 1, num_time_steps
            do l = 1, num_vars
              do k = 1, dim_z
                do j = 1, dim_y
                  do i = 1, dim_x
                    val = array2(i + start_i - 1, j + start_j - 1, k, l, m)
                    expected = compute_real8_data_point(model_id, tag, i, j, k, l, m)
                    if (abs(val - expected) .gt. 0.0) then
                      print '(A, E25.14, A, E25.14)', 'AAAAhhhhh!! Got ', val, ', but should be ', expected
                      print *, 'Model ID ', model_id
                      print *, 'Index    ', i, j, k, l, m
                      print *, 'Block ID ', i_comp, j_comp
                      print '(A, 5(I5))', 'Dims     ', dims2
                      error stop 1
                    end if
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do

    print *, 'All good'

  end subroutine check_result

  subroutine receive_num_pes(context, command)
    use ioserver_context_module
    use jar_module
    implicit none
    type(ioserver_context), intent(inout) :: context
    type(jar),              intent(inout) :: command
#include <rmn/serializer.hf>

    logical :: success

    success = JAR_GET_ITEM(command, num_compute_x)
    success = JAR_GET_ITEM(command, num_compute_y)

    print '(A, 1X, A, 2I6)', context % get_short_pe_name(), 'Got num PEs: ', num_compute_x, num_compute_y
  end subroutine receive_num_pes

end module model_write_parameters

program pseudomodelandserver
  use ISO_C_BINDING
  use ioserver_mpi

  use ioserver_context_module
  use ioserver_run_module
  use model_write_parameters
  implicit none

  integer :: debug_level
  character(len=128) :: arg

  logical :: server_node, single_node
  integer :: node_rank, node_size, global_size, global_rank, num_nodes

  integer :: num_server_processes     ! excluding no_op processes
  integer :: num_receiver_processes
  integer :: num_stream_processors
  integer :: num_channels
  integer :: num_relay_per_node, num_noop

  type(ioserver_input_parameters) :: params
  procedure(model_function_template), pointer :: model_fn_ptr
  procedure(user_command_template),   pointer :: user_command_fn
  logical :: success
  integer :: ierr

  success = .false.

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, global_size, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)

  if (command_argument_count() < 5) then
    if (global_rank == 0) then
      print *, 'ERROR: Need more arguments when launching this program:'
      print *, '1. Whether to activate debug mode (0 or 1)'
      print *, '2. How many "stream processor" server processes you want'
      print *, '3. How many receiver server processes you want'
      print *, '4. How many channel processes you want'
      print *, '5. How many relay processes there should be on each model node'
    end if
    error stop 1
  end if

  call GET_COMMAND_ARGUMENT(1, arg)
  read(arg, *) debug_level

  call GET_COMMAND_ARGUMENT(2, arg)
  read(arg, *) num_stream_processors

  call GET_COMMAND_ARGUMENT(3, arg)
  read(arg,*) num_receiver_processes

  call GET_COMMAND_ARGUMENT(4, arg)
  read(arg, *) num_channels

  call GET_COMMAND_ARGUMENT(5, arg)
  read(arg,*) num_relay_per_node

  server_node          = am_server_node(node_rank, node_size, single_node, num_nodes)
  num_server_processes = num_receiver_processes + num_channels + num_stream_processors
  num_noop             = 0

  model_fn_ptr => pseudo_model_process
  user_command_fn => receive_num_pes

  if (server_node) then
    if (.not. single_node .or. node_rank < num_server_processes) params % is_on_server = .true.
  end if

  params % num_relay_per_node      = num_relay_per_node
  params % num_server_bound_server = num_receiver_processes
  params % num_stream_processors   = num_stream_processors
  params % num_channels            = num_channels
  params % debug_level             = debug_level

  params % dcb_server_bound_size_mb = 500.0

  if (params % is_on_server) then
    success = ioserver_run_server_node(params, custom_user_command_fn = user_command_fn)
  else
    success = ioserver_run_model_node(params, model_function = model_fn_ptr)
  end if

  if (.not. success) then
    print '(A)', 'ERROR while trying to run model_write'
    error stop 1
  end if

  call MPI_Finalize(ierr)

  if (server_node .and. node_rank == 0) then
    call check_result()
  end if

end program
