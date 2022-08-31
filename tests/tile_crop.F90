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
!> \author V. Magnoux, Recherche en Prevision Numerique
!> \date 2020-2022

module tile_crop_module
  use iso_c_binding
  use mpi

  use ioserver_context_module
  implicit none
  save

  integer :: num_pe_x       = 0
  integer :: num_pe_y       = 0
  integer :: num_active_pes = 0

  integer, parameter :: halo_size = 3
  ! integer, parameter :: global_num_x = 20002
  ! integer, parameter :: global_num_y = 19997
  ! integer, parameter :: global_num_x = 202
  ! integer, parameter :: global_num_y = 197
  integer, parameter :: global_num_x = 5
  integer, parameter :: global_num_y = 5

  character(len=*), parameter :: filename = 'tile_crop_test.out'

contains
  function compute_int1(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    integer(C_INT8_T) :: val
    val = (mod(k - 1, 2) * 8 + mod(j - 1, 8)) * 8 + mod(i - 1, 8)
    ! print *, 'ijk  = ', i, j, k, ', val = ', val
  end function compute_int1

  function compute_int2(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    integer(C_INT16_T) :: val
    val = (mod(k - 1, 8) * 64 + mod(j - 1, 64)) * 64 + mod(i - 1, 64)
  end function compute_int2

  function compute_int4(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    integer(C_INT32_T) :: val
    val = (mod(k - 1, 128) * 4096 + mod(j - 1, 4096)) * 4096 + mod(i - 1, 4096)
  end function compute_int4

  function compute_int8(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    integer(C_INT64_T) :: val
    val = compute_int4(i, j, k) * 256 * 256 * 256 * 256
  end function

  function compute_real4(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    real(C_FLOAT) :: val
    integer(C_INT32_T) :: tmp
    tmp = compute_int4(i, j, k)
    val = transfer(tmp, val) * 0.6
  end function compute_real4

  function compute_real8(i, j, k) result(val)
    integer, intent(in) :: i, j, k
    real(C_DOUBLE) :: val
    integer(C_INT64_T) :: tmp
    tmp = compute_int8(i, j, k)
    val = transfer(tmp, val) * 0.28
  end function compute_real8

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
    use shmem_heap_module
    use circular_buffer_module
    use ioserver_message_module
    use jar_module
    use process_command_module
    ! use rpn_extra_module, only: sleep_us
    implicit none

#include <serializer.hf>

    type(ioserver_context), intent(inout) :: context
    logical :: model_success

    type(shmem_heap)      :: node_heap
    type(comm_rank_size)  :: model_crs
    type(model_stream), pointer :: output_stream
    type(circular_buffer) :: model_cb

    type(jar)            :: command_jar
    type(command_header) :: c_header

    logical :: success


    model_success = .false.

    success = command_jar % new(100)

    node_heap = context % get_local_heap()
    model_cb = context % get_server_bound_cb()

    model_crs = context % get_crs(MODEL_COLOR)

    if (model_crs % size < 9) then
      print '(A, 1X, A, I5, A)', context % get_short_pe_name(),           &
            'ERROR: There need to be at least 9 model PEs for this test to work. There are only ', model_crs % size, ' currently'
      return
    end if

    num_pe_x = int(floor(sqrt(real(model_crs % size))))
    num_pe_y = model_crs % size / num_pe_x
    num_active_pes = num_pe_x * num_pe_y

    if (model_crs % rank == 0) then
      print '(A, 1X, A, I5, A, I5, A)', context % get_short_pe_name(), 'Will use ', num_active_pes, ' of ', model_crs % size, ' PEs for this test'

      block
        type(message_header) :: header
        type(message_cap)    :: end_cap
        header % command = MSG_COMMAND_USER
        header % sender_global_rank = context % get_global_rank()
        header % content_size_int8 = 2
        end_cap % msg_length = header % content_size_int8

        success = model_cb % put(header, message_header_size_byte(), CB_KIND_CHAR, .false., timeout_ms = 0)
        success = model_cb % put(num_pe_x, 1_8, CB_KIND_INTEGER_4, .false., timeout_ms = 0)                .and. success
        success = model_cb % put(num_pe_y, 1_8, CB_KIND_INTEGER_4, .false., timeout_ms = 0)                .and. success
        success = model_cb % put(end_cap, message_cap_size_byte(), CB_KIND_CHAR, .true., timeout_ms = 0) .and. success

        if (.not. success) then
          print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Unable to send user command...'
          return
        end if
      end block
    end if

    ! Open and check stream 1
    call context % open_stream_model(output_stream)
    if (.not. success .or. .not. associated(output_stream)) then
      print '(A, 1X, A)', context % get_short_pe_name(), 'ERROR: Could not open model stream'
      return
    end if 

    block
      c_header % command_type = COMMAND_TYPE_OPEN_FILE
      c_header % size_bytes   = len_trim(filename)
      success = JAR_PUT_ITEM (command_jar, c_header)        .and. success
      success = JAR_PUT_STRING(command_jar, trim(filename)) .and. success
      success = output_stream % send_command(command_jar) .and. success
      if (.not. success) then
        print '(A, A)', context % get_short_pe_name(), ' ERROR: Unable to send open file command to model stream !!!!'
        return
      end if
    end block

    block
      type(grid_bounds_t) :: global_grid, reduced_global_grid
      type(grid_bounds_t) :: local_grid
      
      integer :: tile_id_x, tile_id_y
      integer :: base_num_x, base_num_y
      type(grid_index_t) :: real_tile_size
      integer(C_INT64_T) :: real_num_x, real_num_y

      integer :: i, j
      
      type(block_meta_f08) :: data_info
      integer(C_INT32_T), dimension(:,:), pointer, contiguous :: data_array_tmp, data_array
      
      type(model_grid)     :: m_grid

      tile_id_x = mod(model_crs % rank, num_pe_x)  ! Tile index starting at 0
      tile_id_y = model_crs % rank / num_pe_x      ! Tile index starting at 0

      base_num_x = int(ceiling(real(global_num_x) / num_pe_x))
      base_num_y = int(ceiling(real(global_num_y) / num_pe_y))

      call global_grid % set_min(1, 1)
      call global_grid % set_size(global_num_x, global_num_y)
      call local_grid % set_min(tile_id_x * base_num_x + 1, tile_id_y * base_num_y + 1)
      call local_grid % set_size(base_num_x, base_num_y)
      ! local_grid = local_grid % intersection(global_grid)   ! Don't do this, we should just let the io-server deal with it
      reduced_global_grid = global_grid

      ! call local_grid % print()
      ! call halo_grid % print()
      if (model_crs % rank == 0) call global_grid % print()

      data_info = node_heap % allocate(data_array, local_grid % get_min_val(), local_grid % get_max_val())

      if (.not. associated(data_array)) then
        print '(A, 1x, A)', context % get_short_pe_name(), 'ERROR: Could not allocate from shmem heap!'
        return
      end if

      ! print *, shape(data_array), lbound(data_array), ubound(data_array)
      do j = lbound(data_array, dim = 2), ubound(data_array, dim = 2)
        do i = lbound(data_array, dim = 1), ubound(data_array, dim = 1)
          data_array(i, j) = compute_int4(i, j, 1)
          ! print '(A, I2, A, I2, A, I2, A, I2, A, I10)', 'data(', i, ', ', j, ') - global(', i, ', ', j, ') = ', data_array(i, j)
        end do
      end do

      ! Write command header
      call command_jar % reset()
      c_header % command_type = COMMAND_TYPE_WRITE_DATA
      c_header % size_bytes   = (storage_size(m_grid) + 7) / 8
      success = JAR_PUT_ITEM(command_jar, c_header)

      ! Grid metadata (for model processing)
      m_grid % dims      = reduced_global_grid % get_size_val()
      m_grid % elem_size = data_info % get_kind()
      ! print *, 'ELEM_SIZE = ', m_grid % elem_size
      success = JAR_PUT_ITEM(command_jar, m_grid) .and. success

      success = output_stream % send_data(data_info, local_grid, reduced_global_grid, command = command_jar) .and. success

        ! data_array_info_1 = node_heap % allocate(data_array_4, array_dims)

    end block


    success = output_stream % close()

    if (.not. success) then
      print '(A, 1X, A)', 'ERROR: Unable to close stream'
      return
    end if

    model_success = .true.
  end function pseudo_model_process

  subroutine receive_num_pes(context, command)
    use jar_module
    implicit none
    type(ioserver_context), intent(inout) :: context
    type(jar),              intent(inout) :: command
#include <serializer.hf>

    logical :: success

    success = JAR_GET_ITEM(command, num_pe_x)
    success = JAR_GET_ITEM(command, num_pe_y)

    print '(A, 1X, A, 2I6)', context % get_short_pe_name(), 'Got num PEs: ', num_pe_x, num_pe_y
  end subroutine receive_num_pes

  function check_result() result(success)
    implicit none
    logical :: success

    integer :: file_unit
    integer :: i, j
    integer :: num_errors
    integer(C_INT8_T), dimension(global_num_x * 4, global_num_y), target :: read4
    integer(C_INT32_T), dimension(:,:), pointer :: data_i4
    integer(C_INT32_T), dimension(global_num_x, global_num_y) :: expected_i4
    type(C_PTR) :: tmp_ptr

    success = .false.
    num_errors = 0

    tmp_ptr = c_loc(read4)
    call c_f_pointer(tmp_ptr, data_i4, [global_num_x, global_num_y])

    open(newunit = file_unit, file = trim(filename), status = 'old', form = 'unformatted', action = 'read')
    read(unit=file_unit) read4

    do j = 1, global_num_y
      do i = 1, global_num_x
        expected_i4(i, j) = compute_int4(i, j, 1)
        if (data_i4(i, j) .ne. expected_i4(i, j)) then
          num_errors = num_errors + 1
          print *, 'ERROR in file at ', i, j
        end if
      end do
    end do

    ! print *, 'Expected'
    ! print '(3(3(I10), /))', expected_i4
    ! print *, 'Got'
    ! print '(3(3(I10), /))', data_i4

    if (num_errors == 0) success = .true.

  end function check_result
end module tile_crop_module

program tile_crop
  use ioserver_run_module
  use tile_crop_module
  implicit none

  integer :: debug_level
  character(len=128) :: arg

  integer :: ierr
  integer :: global_size, global_rank
  integer :: node_size, node_rank

  logical :: server_node
  logical :: single_node
  integer :: num_nodes

  integer :: num_server_processes     ! excluding no_op processes
  integer :: num_receiver_processes
  integer :: num_stream_processors
  integer :: num_channels
  integer :: num_relay_per_node
  integer :: num_noop

  logical :: success
  type(ioserver_input_parameters) :: params
  procedure(model_function_template), pointer :: model_fn_ptr
  procedure(user_command_template),   pointer :: user_command_fn

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
  ! nullify(model_fn_ptr)

  if (server_node) then
    if (.not. single_node .or. node_rank < num_server_processes) params % is_on_server = .true.
  end if

  params % num_relay_per_node      = num_relay_per_node
  params % num_server_bound_server = num_receiver_processes
  params % num_stream_processors   = num_stream_processors
  params % num_channels            = num_channels
  params % debug_level             = debug_level

  params % model_heap_size_mb      = 200.0

  params % dcb_server_bound_size_mb = 1200.0

  if (node_rank == 0) call params % print()
  if (single_node .and. node_rank == num_server_processes) call params % print()

  if (params % is_on_server) then
    success = ioserver_run_server_node(params, custom_user_command_fn = user_command_fn)
  else
    success = ioserver_run_model_node(params, model_function = model_fn_ptr)
  end if

  if (.not. success) then
    print '(A)', 'ERROR while trying to run tile_crop'
    error stop 1
  end if

  call MPI_Finalize(ierr)

  if (server_node .and. node_rank == 0) then
    success = check_result()
    if (.not. success) then
      print '(A)', 'ERROR: Content of file not as expected'
      error stop 1
    end if
    print '(A)', 'Yayy success'
  end if
end program tile_crop
