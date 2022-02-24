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
  use ioserver_mpi_f08
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

    type(MPI_Comm) :: node_comm, first_rank_comm
    integer :: global_rank, node_root_global_rank
    integer :: global_size

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm)
    call MPI_Comm_rank(node_comm, node_rank)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm)

    call MPI_Comm_size(MPI_COMM_WORLD, global_size)
    call MPI_Comm_size(node_comm, node_size)

    am_server_node = (node_root_global_rank == 0)
    single_node = (global_size == node_size)

    if (node_rank == 0) then
      call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, first_rank_comm)
      call MPI_Comm_size(first_rank_comm, num_nodes)
    else
      call MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, first_rank_comm)
      num_nodes = -1
    end if
  end function am_server_node

  function pseudo_model_process(context) result(model_success)
    use heap_module
    use ioserver_context_module
    use ioserver_message_module
    use rpn_extra_module, only: sleep_us
    implicit none

    type(ioserver_context), intent(inout) :: context
    logical :: model_success

    type(heap)            :: node_heap
    type(model_stream)    :: output_stream_1, output_stream_2
    type(circular_buffer) :: data_buffer
    type(comm_rank_size)  :: model_crs, node_crs

    type(subgrid_t)      :: local_grid, big_local_grid
    type(grid_t)         :: input_grid_4, input_grid_8, output_grid, big_grid
    type(block_meta_f08) :: data_array_info_1, data_array_info_2, big_array_info
    integer(kind=4), dimension(:,:), pointer :: data_array_4
    integer(kind=8), dimension(:,:), pointer :: data_array_8
    real(kind=8), dimension(:,:,:,:,:), pointer :: big_array

    integer :: global_model_id
    integer :: compute_width, compute_height

    logical :: success

    model_success = .false.

    node_heap = context % get_local_heap()

    output_stream_1 = context % open_stream_model(filename1)
    if (.not. output_stream_1 % is_open()) then
      print *, 'Unable to open model file 1 !!!!'
      return
    end if

    output_stream_2 = context % open_stream_model(filename2)
    if (.not. output_stream_2 % is_open()) then
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

    if (context % debug_mode()) print '(A, I8, A)', 'INFO: We are using ', model_crs % size, ' pseudo-model processes'

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

    local_grid % offset(1) = mod(global_model_id, compute_width) * block_width + 1
    local_grid % offset(2) = (global_model_id / compute_width) * block_height + 1
    local_grid % size(1)   = block_width
    local_grid % size(2)   = block_height

    input_grid_4 % id = 1
    input_grid_4 % size(1) = block_width * compute_width
    input_grid_4 % size(2) = block_height * compute_height
    input_grid_4 % elem_size = 4

    input_grid_8 % id = 1
    input_grid_8 % size(1) = block_width * compute_width
    input_grid_8 % size(2) = block_height * compute_height
    input_grid_8 % elem_size = 8

    output_grid % id = 1

    big_local_grid % offset(1) = mod(global_model_id, compute_width) * dim_x + 1
    big_local_grid % offset(2) = (global_model_id / compute_width) * dim_y + 1
    big_local_grid % offset(3) = 1
    big_local_grid % offset(4) = 1
    big_local_grid % offset(5) = 1

    big_local_grid % size(1) = dim_x
    big_local_grid % size(2) = dim_y
    big_local_grid % size(3) = dim_z
    big_local_grid % size(4) = num_vars
    big_local_grid % size(5) = num_time_steps

    big_grid % id = 2
    big_grid % size(1) = dim_x * compute_width
    big_grid % size(2) = dim_y * compute_height
    big_grid % size(3) = dim_z
    big_grid % size(4) = num_vars
    big_grid % size(5) = num_time_steps
    big_grid % elem_size = 8

    ! print *, 'Created local grid', local_grid % i0, local_grid % i0 + local_grid % ni

    ! call sleep_us(5000)
    block
      integer :: i_msg, i_data, j_data, current_tag
      integer(C_INT64_T), dimension(2) :: array_dims
      integer(C_INT64_T), dimension(5) :: big_array_dims
      integer :: i, j, k, l, m
      array_dims = [block_width, block_height]
      big_array_dims = [dim_x, dim_y, dim_z, num_vars, num_time_steps]
      current_tag = 0
      do i_msg = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT
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

        ! Write the data to a file (i.e. send it to the server to do that for us)
        success = output_stream_1 % write(data_array_info_1, local_grid, input_grid_4, output_grid)

        if (.not. success) then
          print *, 'ERROR: Write into model stream failed'
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

        success = output_stream_2 % write(data_array_info_2, local_grid, input_grid_8, output_grid)
        success = output_stream_2 % write(big_array_info, big_local_grid, big_grid, output_grid) .and. success
        if (.not. success) then
          print *, 'ERROR while trying to do a WRITE'
          return
        end if

        call sleep_us(5000)
      end do
    end block

    success = output_stream_1 % close()
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

    open(newunit = file_unit, file = trim(filename2)//'.out', status = 'old', form = 'unformatted', action = 'read')

    tag = 2
    do i_msg = 1, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT ! Loop through all pairs of records in this file
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

end module model_write_parameters

program pseudomodelandserver
  use ISO_C_BINDING
  use ioserver_mpi_f08

  use ioserver_context_module
  use ioserver_run_module
  use model_write_parameters
  implicit none

  integer :: debug_mode_flag
  character(len=128) :: arg

  logical :: server_node, single_node
  integer :: node_rank, node_size, global_size, global_rank, num_nodes

  integer :: num_server_processes     ! excluding no_op processes
  integer :: num_receiver_processes
  integer :: num_grid_processors
  integer :: num_channels
  integer :: num_relay_per_node, num_noop

  type(ioserver_input_parameters) :: params
  type(ioserver_context) :: context
  procedure(model_function_template), pointer :: model_fn_ptr
  logical :: success

  success = .false.

  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD, global_size)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)

  if (command_argument_count() < 5) then
    if (global_rank == 0) then
      print *, 'ERROR: Need more arguments when launching this program:'
      print *, '1. Whether to activate debug mode (0 or 1)'
      print *, '2. How many "grid processor" server processes you want'
      print *, '3. How many receiver server processes you want'
      print *, '4. How many channel processes you want'
      print *, '5. How many relay processes there should be on each model node'
    end if
    error stop 1
  end if

  call GET_COMMAND_ARGUMENT(1, arg)
  read(arg, *) debug_mode_flag

  call GET_COMMAND_ARGUMENT(2, arg)
  read(arg, *) num_grid_processors

  call GET_COMMAND_ARGUMENT(3, arg)
  read(arg,*) num_receiver_processes

  call GET_COMMAND_ARGUMENT(4, arg)
  read(arg, *) num_channels

  call GET_COMMAND_ARGUMENT(5, arg)
  read(arg,*) num_relay_per_node

  server_node          = am_server_node(node_rank, node_size, single_node, num_nodes)
  num_server_processes = num_receiver_processes + num_channels + num_grid_processors
  num_noop             = 0

  model_fn_ptr => pseudo_model_process
  ! nullify(model_fn_ptr)

  if (server_node) then
    if (.not. single_node .or. node_rank < num_server_processes) params % is_on_server = .true.
  end if

  if (debug_mode_flag == 1) params % debug_mode = .true.

  params % num_relay_per_node      = num_relay_per_node
  params % num_server_bound_server = num_receiver_processes
  params % num_grid_processors     = num_grid_processors
  params % num_channels            = num_channels

  params % dcb_server_bound_size_mb = 500.0

  if (params % is_on_server) then
    success = ioserver_run_server_node(params, context_out = context)
  else
    success = ioserver_run_model_node(params, model_function = model_fn_ptr)
  end if

  if (.not. success) then
    print *, 'ERROR while trying to run model_write'
    error stop 1
  end if

  call MPI_Finalize()

  if (server_node .and. node_rank == 0) then
    block
      integer :: num_model_pes

      num_model_pes = context % get_num_total_model()

      ! Init area info
      if (mod(num_model_pes, 4) == 0) then
        num_compute_x = num_model_pes / 4
        num_compute_y = 4
      else if (mod(num_model_pes, 2) == 0) then
        num_compute_x = num_model_pes / 2
        num_compute_y = 2
      else
        num_compute_x = num_model_pes
        num_compute_y = 1
      end if

      call check_result()
    end block
  end if

end program
