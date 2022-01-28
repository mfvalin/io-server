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
  use mpi_f08
  implicit none

  integer, parameter :: CB_MESSAGE_SIZE_INT       = 16384                     !< Size of each data batch put in a CB. Must be divisible by 16 for the tests
  integer, parameter :: CB_TOTAL_DATA_TO_SEND_INT = CB_MESSAGE_SIZE_INT * 16  !< How much total data to send (for each CB)

  integer, save :: num_compute_x
  integer, save :: num_compute_y
  integer, save :: num_elem_x
  integer, save :: num_elem_y

  integer(C_INT64_T), parameter :: dim_x = 10
  integer(C_INT64_T), parameter :: dim_y = 37
  integer(C_INT64_T), parameter :: dim_z = 40
  integer(C_INT64_T), parameter :: num_vars = 3
  integer(C_INT64_T), parameter :: num_time_steps = 5

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

  function am_server_node(node_rank, node_size, single_node)
    implicit none

    integer, intent(out) :: node_rank, node_size
    logical, intent(out) :: single_node
    logical :: am_server_node

    type(MPI_Comm) :: node_comm
    integer :: global_rank, node_root_global_rank
    integer :: global_size

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm)
    call MPI_Comm_rank(node_comm, node_rank)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.

    call MPI_Comm_size(MPI_COMM_WORLD, global_size)
    call MPI_Comm_size(node_comm, node_size)

    single_node = .false.
    if (global_size == node_size) single_node = .true.
  end function am_server_node

  subroutine pseudo_model_process(context)
    use heap_module
    use ioserver_context_module
    use ioserver_message_module
    use rpn_extra_module, only: sleep_us
    implicit none

    type(ioserver_context), intent(inout) :: context

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
    integer :: compute_width, compute_height, block_width, block_height

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

    node_crs         = context % get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
    global_model_id  = model_crs % rank

    data_buffer = context % get_server_bound_cb()
    if (.not. data_buffer % is_valid()) then
      print *, 'ERROR: CB received from context is not valid!'
      error stop 1
    end if

    print '(A, I8, A)', 'INFO: We are using ', model_crs % size, ' pseudo-model processes'

    block_width  = CB_MESSAGE_SIZE_INT / 4
    block_height = 4

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
    num_elem_x = block_width
    num_elem_y = block_height

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
      current_tag = 1
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
          error stop 1
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
          error stop 1
        end if

        call sleep_us(500)
      end do
    end block

    success = output_stream_1 % close()
    success = output_stream_2 % close() .and. success
    if (.not. success) then
      print *, 'Unable to close model file!!!!'
      error stop 1
    end if
  end subroutine pseudo_model_process
end module model_write_parameters

program pseudomodelandserver
  use ISO_C_BINDING
  use mpi_f08

  use ioserver_run_module
  use model_write_parameters
  implicit none

  integer :: status, input
  integer :: nio_node, num_channels

  integer :: nserv !, noops
  character(len=128) :: arg

  logical :: server_node, single_node
  integer :: node_rank, node_size

  integer :: num_relay_per_node, num_noop

  logical :: debug_mode
  logical :: CHECK_CB_MESSAGES
  logical :: CHECK_DCB_MESSAGES

  procedure(ioserver_function_template), pointer :: model_fn_ptr

  call mpi_init(status)

  debug_mode = .false.
  ! debug_mode = .true. ! activate debug mode

  ! Arguments
  ! 1. Check messages or not
  ! 2. Number of active server processes
  ! 3. Number of channel processes
  ! 4. Number of relay processes per node

  ! Arg 1
  CHECK_CB_MESSAGES = .false.
  CHECK_DCB_MESSAGES = .false.
  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg, *) input
  if (input > 0) then
    CHECK_CB_MESSAGES = .true.
    CHECK_DCB_MESSAGES = .true.
  end if

  ! Arg 2
  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)
  read(arg,*) nserv
  nserv = nserv

  ! Arg 3
  arg = '1'
  if (COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg, *) num_channels
  nserv = nserv + num_channels

  ! Arg 4
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 4) call GET_COMMAND_ARGUMENT(4, arg)
  read(arg,*) nio_node                    ! number of relay processes per node

  server_node = am_server_node(node_rank, node_size, single_node)

  num_relay_per_node = nio_node
  num_noop = 0
  if (.not. single_node) then
    num_noop = node_size - nserv
  end if

  model_fn_ptr => pseudo_model_process
  ! nullify(model_fn_ptr)

  ! if(rank >= nserv) then
  ! if(mod(rank, NUM_NODES) .ne. 0) then
  if (.not. server_node) then
    call ioserver_run_model_node(num_relay_per_node, use_debug_mode_in = CHECK_CB_MESSAGES)
  else
    if (node_rank < nserv) then
      call ioserver_run_server_node(nserv - num_channels, num_channels, num_noop, use_debug_mode_in = CHECK_DCB_MESSAGES)
    else 
      if (single_node) then
        call ioserver_run_model_node(num_relay_per_node, use_debug_mode_in = CHECK_CB_MESSAGES, model_function = model_fn_ptr)
      else
        call ioserver_run_server_node(nserv - num_channels, num_channels, num_noop, use_debug_mode_in = CHECK_DCB_MESSAGES)
      end if
    end if

  endif
  call mpi_finalize(status)

end program
