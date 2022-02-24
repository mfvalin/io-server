
module init_comms_module
  use ioserver_mpi_f08
  use ioserver_context_module
  implicit none

  type, private :: all_crs
    type(comm_rank_size) :: global_crs, active_crs, node_crs, io_crs
    type(comm_rank_size) :: model_relay_crs, model_crs, relay_crs, model_relay_node_crs, model_node_crs, relay_node_crs, sb_relay_node_crs
    type(comm_rank_size) :: server_crs, server_work_crs, server_dcb_crs, sb_server_crs, mb_server_crs, grid_processor_crs
  contains
    procedure, pass :: get_all
    procedure, pass :: check_all

    procedure, nopass :: check_single_non_null
    procedure, nopass :: check_single_barrier
  end type all_crs

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

  subroutine get_all(this, context)
    implicit none
    class(all_crs),         intent(inout) :: this
    type(ioserver_context), intent(in)    :: context

    this % global_crs            = context % get_crs(NO_COLOR)
    this % active_crs            = context % get_crs(SERVER_COLOR + MODEL_COLOR + RELAY_COLOR)
    this % node_crs              = context % get_crs(NODE_COLOR)
    this % io_crs                = context % get_crs(RELAY_COLOR + SERVER_COLOR)
    this % model_relay_crs       = context % get_crs(MODEL_COLOR + RELAY_COLOR)
    this % model_crs             = context % get_crs(MODEL_COLOR)
    this % relay_crs             = context % get_crs(RELAY_COLOR)
    this % model_relay_node_crs  = context % get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
    this % model_node_crs        = context % get_crs(MODEL_COLOR + NODE_COLOR)
    this % relay_node_crs        = context % get_crs(RELAY_COLOR + NODE_COLOR)
    this % sb_relay_node_crs     = context % get_crs(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR)

    this % server_crs            = context % get_crs(SERVER_COLOR)
    this % server_work_crs       = context % get_crs(SERVER_COLOR - CHANNEL_COLOR)
    this % server_dcb_crs        = context % get_crs(CHANNEL_COLOR + SERVER_BOUND_COLOR + MODEL_BOUND_COLOR)
    this % sb_server_crs         = context % get_crs(SERVER_COLOR + SERVER_BOUND_COLOR)
    this % mb_server_crs         = context % get_crs(SERVER_COLOR + MODEL_BOUND_COLOR)
    this % grid_processor_crs    = context % get_crs(SERVER_COLOR + GRID_PROCESSOR_COLOR)
  end subroutine get_all

  function check_all(this, context,                         &
    active,                                                 &
    node, io, model_relay, model, relay, model_relay_node,  &
    model_node, relay_node, sb_relay_node,                  &
    server, server_work, server_dcb, sb_server, mb_server,  &
    grid_processor) result(success)
    implicit none
    class(all_crs),         intent(inout) :: this
    type(ioserver_context), intent(in)    :: context
    logical, intent(in) :: active, node, io, model_relay, model, relay
    logical, intent(in) :: model_relay_node, model_node, relay_node
    logical, intent(in) :: sb_relay_node
    logical, intent(in) :: server, server_work, server_dcb
    logical, intent(in) :: sb_server, mb_server, grid_processor
    logical :: success

    call this % get_all(context)

    success =                                                                 &
        check_single_non_null(context, this % global_crs, .true., 'global') .and.       &
        check_single_non_null(context, this % active_crs, active, 'active') .and.       &
        check_single_non_null(context, this % node_crs, node, 'node') .and.       &
        check_single_non_null(context, this % io_crs, io, 'io') .and.       &
        check_single_non_null(context, this % model_relay_crs, model_relay, 'model+relay') .and.       &
        check_single_non_null(context, this % model_crs, model, 'model') .and.       &
        check_single_non_null(context, this % relay_crs, relay, 'relay') .and.       &
        check_single_non_null(context, this % model_relay_node_crs, model_relay_node, 'model+relay on node') .and.       &
        check_single_non_null(context, this % model_node_crs, model_node, 'model on node') .and.       &
        check_single_non_null(context, this % relay_node_crs, relay_node, 'relay on node') .and.       &
        check_single_non_null(context, this % sb_relay_node_crs, sb_relay_node, 'server-bound relay on node') .and.       &
        check_single_non_null(context, this % server_crs, server, 'server') .and.       &
        check_single_non_null(context, this % server_work_crs, server_work, 'server workers') .and.       &
        check_single_non_null(context, this % server_dcb_crs, server_dcb, 'server DCB') .and.       &
        check_single_non_null(context, this % sb_server_crs, sb_server, 'server-bound server') .and.       &
        check_single_non_null(context, this % mb_server_crs, mb_server, 'model-bound server') .and.       &
        check_single_non_null(context, this % grid_processor_crs, grid_processor, 'grid processor') 

    if (.not. success) return

    call check_single_barrier(context, this % global_crs, .true., 'global')
    call check_single_barrier(context, this % active_crs, active, 'active')
    call check_single_barrier(context, this % node_crs, node, 'node')
    call check_single_barrier(context, this % io_crs, io, 'io')
    call check_single_barrier(context, this % model_relay_crs, model_relay, 'model+relay')
    call check_single_barrier(context, this % model_crs, model, 'model')
    call check_single_barrier(context, this % relay_crs, relay, 'relay')
    call check_single_barrier(context, this % model_relay_node_crs, model_relay_node, 'model+relay on node')
    call check_single_barrier(context, this % model_node_crs, model_node, 'model on node')
    call check_single_barrier(context, this % relay_node_crs, relay_node, 'relay on node')
    call check_single_barrier(context, this % sb_relay_node_crs, sb_relay_node, 'server-bound relay on node')
    call check_single_barrier(context, this % server_crs, server, 'server')
    call check_single_barrier(context, this % server_work_crs, server_work, 'server workers')
    call check_single_barrier(context, this % server_dcb_crs, server_dcb, 'server DCB')
    call check_single_barrier(context, this % sb_server_crs, sb_server, 'server-bound server')
    call check_single_barrier(context, this % mb_server_crs, mb_server, 'model-bound server')
    call check_single_barrier(context, this % grid_processor_crs, grid_processor, 'grid processor') 

    call check_single_barrier(context, this % active_crs, active, 'active') ! Once more, to help synchronize output

  end function check_all

  function check_single_non_null(context, crs, do_check, name) result(success)
    implicit none
    type(ioserver_context), intent(in)    :: context
    type(comm_rank_size),   intent(inout) :: crs
    logical,                intent(in)    :: do_check
    character(len=*),       intent(in)    :: name
    logical :: success

    success = crs % is_null()
    if (do_check) success = .not. success

    if (.not. success) then
      if (.not. do_check) then
        print '(A, A, A)', context % get_detailed_pe_name(), ' has access to a comm it should not! ', name
      else
        print '(A, A, A)', context % get_detailed_pe_name(), ' does not have access to a comm it should! ', name
      end if
    end if
  end function check_single_non_null

  subroutine check_single_barrier(context, crs, do_check, name)
    implicit none
    type(ioserver_context), intent(in) :: context
    type(comm_rank_size),   intent(in) :: crs
    logical,                intent(in) :: do_check
    character(len=*),       intent(in) :: name

    ! print *, 'Check ', name, do_check

    if (do_check) then
      call MPI_Barrier(crs % comm)
      if (crs % rank == 0) then
        print '(A, A, A, A)', 'Barrier by ', context % get_detailed_pe_name(), ' for ', name
      end if
    end if
  end subroutine check_single_barrier

  function custom_functions_model(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom model doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .true.,           &
        model_relay       = .true.,           &
        model_relay_node  = .true.,           &
        model_node        = .true.,           &
        io                = .false.,          &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .false.,          &
        server_work       = .false.,          &
        server_dcb        = .false.,          &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'MODEL PE HAS FAILED'
  end function custom_functions_model

  function custom_server_bound_relay(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom server-bound relay doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .true.,           &
        model_relay_node  = .true.,           &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .true.,           &
        relay_node        = .true.,           &
        sb_relay_node     = .true.,           &
        server            = .false.,          &
        server_work       = .false.,          &
        server_dcb        = .false.,          &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'Server-bound relay has failed!'

  end function custom_server_bound_relay

  function custom_model_bound_relay(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom model-bound relay doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .true.,           &
        model_relay_node  = .true.,           &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .true.,           &
        relay_node        = .true.,           &
        sb_relay_node     = .false.,          &
        server            = .false.,          &
        server_work       = .false.,          &
        server_dcb        = .false.,          &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'Model-bound relay has failed!'

  end function custom_model_bound_relay

  function custom_grid_processor(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom grid processor doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .false.,          &
        model_relay_node  = .false.,          &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .true.,           &
        server_work       = .true.,           &
        server_dcb        = .false.,          &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .true.)

    if (.not. success) print *, 'Grid processor PE has failed!'

  end function custom_grid_processor

  function custom_channel(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success

    type(all_crs) :: crs
    type(distributed_circular_buffer) :: dcb

    print *, 'Custom channel doing almost nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .false.,          &
        model_relay_node  = .false.,          &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .true.,           &
        server_work       = .false.,          &
        server_dcb        = .true.,           &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'Channel PE has failed!'

    dcb = context % get_dcb()
    success = dcb % start_listening()
  end function custom_channel

  function custom_server_bound_server(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom server-bound server doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .false.,          &
        model_relay_node  = .false.,          &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .true.,           &
        server_work       = .true.,           &
        server_dcb        = .true.,           &
        sb_server         = .true.,           &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'Server-bound server PE has failed!'

  end function custom_server_bound_server

  function custom_model_bound_server(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom model-bound server doing nothing '
    success = crs % check_all(context,        &
        active            = .true.,           &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .false.,          &
        model_relay_node  = .false.,          &
        model_node        = .false.,          &
        io                = .true.,           &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .true.,           &
        server_work       = .true.,           &
        server_dcb        = .true.,           &
        sb_server         = .false.,          &
        mb_server         = .true.,           &
        grid_processor    = .false.)

    if (.not. success) print *, 'Model-bound server PE has failed!'

  end function custom_model_bound_server

  function custom_server_no_op(context) result(success)
    implicit none
    type(ioserver_context), intent(inout) :: context
    logical :: success
    type(all_crs) :: crs

    print *, 'Custom no-op server doing nothing '
    success = crs % check_all(context,        &
        active            = .false.,          &
        node              = .true.,           &
        model             = .false.,          &
        model_relay       = .false.,          &
        model_relay_node  = .false.,          &
        model_node        = .false.,          &
        io                = .false.,          &
        relay             = .false.,          &
        relay_node        = .false.,          &
        sb_relay_node     = .false.,          &
        server            = .false.,          &
        server_work       = .false.,          &
        server_dcb        = .false.,          &
        sb_server         = .false.,          &
        mb_server         = .false.,          &
        grid_processor    = .false.)

    if (.not. success) print *, 'No-op server PE has failed!'

    call context % no_op()
  end function custom_server_no_op

end module init_comms_module

program init_comms
  use init_comms_module
  use ioserver_run_module
  implicit none

  logical :: server_node, single_node, success
  integer :: node_rank, node_size, num_server_processes, global_size

  type(ioserver_input_parameters) :: params
  procedure(model_function_template),  pointer :: model_fn_ptr
  procedure(relay_function_template),  pointer :: sb_relay_fn, mb_relay_fn
  procedure(server_function_template), pointer :: grid_processor_fn, channel_fn, sb_server_fn, mb_server_fn
  procedure(no_op_function_template),  pointer :: server_no_op_fn

  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD, global_size)

  if (global_size < 11) then
    print *, 'ERROR: Need at least 11 processes to run this test! Only have ', global_size
    error stop 1
  end if

  params % debug_mode = .true.

  server_node = am_server_node(node_rank, node_size, single_node)

  params % num_channels = 2
  params % num_server_bound_server = 2
  params % num_model_bound_server = 1
  params % num_relay_per_node = 2
  params % num_grid_processors = 1

  num_server_processes = params % num_channels + params % num_server_bound_server + params % num_grid_processors + params % num_model_bound_server

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
  server_no_op_fn   => custom_server_no_op

  ! nullify(channel_fn)

  if (params % is_on_server) then
    success = ioserver_run_server_node(params,        &
        custom_grid_processor_fn = grid_processor_fn, &
        custom_server_bound_fn = sb_server_fn,        &
        custom_model_bound_fn = mb_server_fn,         &
        custom_channel_fn = channel_fn,               &
        custom_no_op_fn = server_no_op_fn)
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

end program init_comms
