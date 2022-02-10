
module custom_functions_module
  use mpi_f08
  use ioserver_context_module
  implicit none

contains

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

  function custom_functions_model(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success

    type(comm_rank_size) :: active_crs

    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom model doing nothing ', active_crs % rank

    success = .true.
  end function custom_functions_model

  function custom_server_bound_relay(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(comm_rank_size) :: active_crs
    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom server-bound relay doing nothing ', active_crs % rank
    success = .true.
  end function custom_server_bound_relay

  function custom_model_bound_relay(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(comm_rank_size) :: active_crs
    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom model-bound relay doing nothing ', active_crs % rank
    success = .true.
  end function custom_model_bound_relay

  function custom_grid_processor(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(comm_rank_size) :: active_crs
    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom grid processor doing nothing ', active_crs % rank
    success = .true.
  end function custom_grid_processor

  function custom_channel(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success

    type(comm_rank_size) :: active_crs
    type(distributed_circular_buffer) :: dcb

    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom channel doing almost nothing ', active_crs % rank
    dcb = context % get_dcb()
    success = dcb % start_listening()


    success = .true.
  end function custom_channel

  function custom_server_bound_server(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(comm_rank_size) :: active_crs
    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom server-bound server doing nothing ', active_crs % rank
    success = .true.
  end function custom_server_bound_server

  function custom_model_bound_server(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(comm_rank_size) :: active_crs
    active_crs = context % get_crs(NO_COLOR)
    call MPI_Barrier(active_crs % comm)
    print *, 'Custom model-bound server doing nothing ', active_crs % rank
    success = .true.
  end function custom_model_bound_server

end module custom_functions_module

program custom_functions
  use custom_functions_module
  use ioserver_run_module
  implicit none

  logical :: server_node, single_node, success
  integer :: node_rank, node_size, num_server_processes, global_size

  type(ioserver_input_parameters) :: params
  procedure(model_function_template),  pointer :: model_fn_ptr
  procedure(relay_function_template),  pointer :: sb_relay_fn, mb_relay_fn
  procedure(server_function_template), pointer :: grid_processor_fn, channel_fn, sb_server_fn, mb_server_fn

  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD, global_size)

  if (global_size < 9) then
    print *, 'ERROR: Need at least 10 processes to run this test! Only have ', global_size
    error stop 1
  end if

  params % debug_mode = .true.

  server_node = am_server_node(node_rank, node_size, single_node)

  params % num_channels = 2
  params % num_server_bound_server = 2
  params % num_relay_per_node = 2
  params % num_grid_processors = 1

  num_server_processes = params % num_channels + params % num_server_bound_server + params % num_grid_processors

  if (server_node) then
    ! Add 1 NO-OP process on the server
    if (.not. single_node .or. node_rank < num_server_processes + 1) params % is_on_server = .true.
  end if

  model_fn_ptr => custom_functions_model
  sb_relay_fn  => custom_server_bound_relay
  mb_relay_fn  => custom_model_bound_relay

  grid_processor_fn => custom_grid_processor
  sb_server_fn      => custom_server_bound_server
  mb_server_fn      => custom_model_bound_server
  channel_fn        => custom_channel
  ! nullify(channel_fn)

  if (params % is_on_server) then
    success = ioserver_run_server_node(params,        &
        custom_grid_processor_fn = grid_processor_fn, &
        custom_server_bound_fn = sb_server_fn,        &
        custom_model_bound_fn = mb_server_fn,         &
        custom_channel_fn = channel_fn)
  else
    success = ioserver_run_model_node(params,         &
        model_function = model_fn_ptr,                &
        custom_server_bound_relay_fn = sb_relay_fn,   &
        custom_model_bound_relay_fn = mb_relay_fn)
  end if

  if (.not. success) then
    print *, 'ERROR while trying to run the IO server with all custom functions'
    error stop 1
  end if

  call MPI_Finalize()

end program custom_functions
