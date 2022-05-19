!  Copyright (C) 2022  Recherche en Prevision Numerique
! 
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
! 
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022
!
module ioserver_context_module
  !> Module description
  use ISO_C_BINDING
  use ioserver_mpi

  use circular_buffer_module
  use distributed_circular_buffer_module
  use shmem_heap_module
  use ioserver_constants_module
  use ioserver_message_module
  use model_stream_module
  use rpn_extra_module
  use server_stream_module
  use shmem_arena_module
  implicit none

  private
  save

  ! Publish some types and constants that are useful
  public :: shmem_heap, circular_buffer, distributed_circular_buffer, comm_rank_size, model_stream, local_server_stream
  public :: no_op_function_template, default_no_op
  public :: MODEL_COLOR, SERVER_COLOR, RELAY_COLOR, SERVER_BOUND_COLOR, MODEL_BOUND_COLOR, NODE_COLOR, GRID_PROCESSOR_COLOR, NO_COLOR, CHANNEL_COLOR

  integer, parameter         :: MAX_PES_PER_NODE  = 128 !< How many PEs per node we can manage

  !> Struct to hold information for PEs on a node (_shared memory addresses are valid for the corresponding PE only_)
  type, bind(C) :: pe_info 
    type(C_PTR)         :: arena_ptr              !< Local address of shared memory arena
    integer(C_INTPTR_T) :: heap_offset            !< Offset of heap inside shared memory arena
    integer(C_INTPTR_T) :: model_bound_cb_offset  !< Offset of model-bound CB inside shared memory arena
    integer(C_INTPTR_T) :: server_bound_cb_offset !< Offset of server-bound CB inside shared memory arena
    integer(C_INT)      :: color                  !< PE type (compute/relay/server/...)
    integer(C_INT)      :: rank                   !< rank on node + same color
    integer(C_INT), dimension(4) :: pad      !< pad to a size of 64 bytes
  end type

  type, bind(C) :: control_shared_memory
    integer(C_INT) :: version      = 10000                 ! version 1.0.0
    integer(C_INT) :: time_to_quit = 0
    ! NO-OP PEs do not use anything beyond this point
    integer(C_INT), dimension(10) :: pad                   !< To align next item on a 64 byte boundary
    ! pe MUST be the last element of this structure
    type(pe_info), dimension(0:MAX_PES_PER_NODE-1) :: pe   !< Node PE information origin 0, indexed by rank
  end type control_shared_memory

  type, public :: ioserver_input_parameters
    !> @{ \name Process counts and types
    integer :: num_relay_per_node       = 0 !< How many relay processes there are on each model node (server- and model-bound)
    integer :: num_grid_processors      = 0 !< How many server processes are used to process grids and write files
    integer :: num_server_bound_server  = 0 !< How many server-bound server processes there are
    integer :: num_model_bound_server   = 0 !< How many model-bound server processes there are
    integer :: num_channels             = 0 !< How many communication channels (processes) there are on the server

    logical :: is_on_server = .false.   !< Whether the context will belong to a server process
    !> @}

    !> @{ \name Size of shared memory spaces
    real :: model_heap_size_mb       = 50.0 !< Size in MB of shared memory heap for each model process
    real :: server_bound_cb_size_mb  = 5.0  !< Size in MB of buffer for server-bound data for each model process
    real :: model_bound_cb_size_mb   = 2.0  !< Size in MB of buffer for model-bound data for each model process

    real :: server_heap_size_mb      = 10000.0  !< Size in MB of the server heap where grids will be assembled
    real :: dcb_server_bound_size_mb = 50.0     !< Size in MB of each server-bound CB within the DCB
    real :: dcb_model_bound_size_mb  = 2.0      !< Size in MB of each model-bound CB within the DCB
    !> @}

    !> @{ \name Stream control
    !> How many streams can be open at the same time.
    !> _This will be the same for every PE, so the common value is determined by the root server PE._
    integer :: max_num_concurrent_streams = 32
    !> @}

    !> @{ \name Pipeline control
    !> Maximum difference of tags that a relay can transmit before starting to wait for the slower model PEs on the node.
    !> Ideally, should be lower than MAX_ASSEMBLY_LINES
    integer :: relay_pipeline_depth = 5

    !> Maximum difference of tags that a server can process (between the various relays it receives from), before starting to
    !> wait for the slower relays
    !> Ideally, should be lower than MAX_ASSEMBLY_LINES. _Must be at least as large as #relay_pipeline_depth_
    integer :: server_pipeline_depth = 10
    !> @}

    !> Determines the level of debugging statements within the code
    !> 0: no debug statements. 1: synchronize model commands to the server, print node-wide debug info. 2: Also print PE-specific debug info
    !> _Can be different for each PE, but some info is printed only by specific PEs, so be careful with that_
    integer :: debug_level = 0
  end type ioserver_input_parameters

  !> Context that allows to interact with the IO-server library (initialization + the rest of the API)
  type, public :: ioserver_context
    private
    integer :: color = NO_COLOR !< Color of this PE. Describes its role (server vs relay vs model, server-bound vs relay-bound, etc.)

    type(ioserver_input_parameters) :: params !< Input parameters for initializing the context

    ! ----------------------------------------------------------------
    !> @{ \name Shared memory areas (raw C pointers and area sizes)

    ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
    type(C_PTR)       :: ctrl_shmem_c      = C_NULL_PTR       !< Address of main (control) shared memory area on this node
    integer(C_SIZE_T) :: ctrl_shmem_size = 0 !< Size of control shared memory

    ! information for model compute and IO relay PEs on a given SMP node
    ! shared memory used for heaps and circular buffers on a given SMP node
    ! (one heap and 2 circular buffers per compute PE)
    ! this memory is used for communications between IO relay and model compute PEs
    type(C_PTR)       :: model_shmem          = C_NULL_PTR    !< Address of relay+model PEs shared memory area
    integer(C_SIZE_T) :: model_shmem_size     = 0             !< Size of relay/model shared memory area

    type(C_PTR)       :: server_heap_shmem      = C_NULL_PTR  !< Address of io server PEs shared memory area
    integer(C_SIZE_T) :: server_heap_shmem_size = 0           !< Size of server shared memory area (we want a lot on the server for grid assembly)

    type(C_PTR)       :: server_file_shmem      = C_NULL_PTR  !< Address of server shared memory area for holding open file data
    integer(C_SIZE_T) :: server_file_shmem_size = 0           !< Size of server shared memory area for holding open file data (computed at initialization)

    type(C_PTR) :: local_arena_ptr                            !< Pointer to start of shared memory arena
    !> @}
    
    ! -----------------
    !> @{ \name Communicators
    integer :: global_comm = MPI_COMM_WORLD  !< MPI "WORLD" for this set of PEs
    integer :: global_rank = -1              !< rank in global_comm
    integer :: global_size =  0              !< population of global_comm

    integer :: node_comm   = MPI_COMM_NULL   !< PEs on this node. If server and model are on the same physical node, they will be split into two virtual nodes
    integer :: node_rank   = -1              !< rank in node_comm
    integer :: node_size   =  0              !< population of node_comm

    integer :: active_comm = MPI_COMM_NULL   !< All "active" PEs (non NO-OP) (subset of global_comm)
    integer :: active_rank = -1              !< Rank on active_comm
    integer :: active_size =  0              !< Size of active_comm (number of active PEs)

    integer :: io_comm                     = MPI_COMM_NULL  !< all IO PEs     (relay + server) (subset of active_comm)
    integer :: io_dcb_comm                 = MPI_COMM_NULL  !< all IO PEs that participate in the DCB (relay + server, without grid processors) (subset of active_comm)

    integer :: model_relay_comm            = MPI_COMM_NULL  !< model and relay PEs (all nodes) (subset of active_comm)
    integer :: model_comm                  = MPI_COMM_NULL  !< model PEs           (all nodes) (subset of model_relay_comm)
    integer :: relay_comm                  = MPI_COMM_NULL  !< relay PEs           (all nodes) (subset of model_relay_comm)

    integer :: server_comm                 = MPI_COMM_NULL  !< IO server PEs (subset of active_comm)
    integer :: server_work_comm            = MPI_COMM_NULL  !< Server PEs that process data (subset of server_comm, excludes channel PEs)
    integer :: server_dcb_comm             = MPI_COMM_NULL  !< Server PEs that participate in the DCB (subset of server_comm, excludes grid processors)
    integer :: server_bound_server_comm    = MPI_COMM_NULL  !< Server PEs that process server-bound relay data (subset of server_dcb_comm)
    integer :: model_bound_server_comm     = MPI_COMM_NULL  !< Server PEs that process model-bound relay data (subset of server_dcb_comm)
    integer :: grid_processor_server_comm  = MPI_COMM_NULL  !< Server PEs that process assembled grid data (subset of server_work_comm)

    integer :: model_relay_smp_comm        = MPI_COMM_NULL  !< model+relays on current node
    integer :: model_smp_comm              = MPI_COMM_NULL  !< model PEs on current SMP node
    integer :: relay_smp_comm              = MPI_COMM_NULL  !< relay PEs on current SMP node
    integer :: server_bound_relay_smp_comm = MPI_COMM_NULL  !< server-bound relay PEs on current SMP node, subset of relay_smp_comm
    !> @}

    !----------------------------------------------------
    !> @{ \name Local instances of objects located in shared memory
    type(shmem_arena)      :: arena                 !< Node memory arena
    type(shmem_heap)       :: local_heap            !< Local heap for this process (located in memory arena)
    type(circular_buffer)  :: local_cio_in          !< Model-bound circular buffer for this process (located in memory arena)
    type(circular_buffer)  :: local_server_bound_cb !< Server-bound circular buffer for this process (located in memory arena)
    type(distributed_circular_buffer) :: local_dcb  !< Distributed circular buffer for communication b/w relay and server processes
    type(shmem_heap)       :: node_heap             !< On server node only. Heap that everyone on the node can use

    type(circular_buffer),     dimension(:), pointer :: model_bound_cbs      => NULL() !< The CB objects belonging to model PEs (model-bound)
    type(circular_buffer),     dimension(:), pointer :: server_bound_cbs     => NULL() !< The CB objects belonging to model PEs (server-bound)
    type(shmem_heap),          dimension(:), pointer :: local_heaps          => NULL() !< Shared memory heaps belonging to model PEs
    type(local_server_stream), dimension(:), pointer :: local_server_streams => NULL() !< Local stream instances to access the shared ones (on server only)
    !> @}

    !---------------------------------------------------
    !> @{ \name Direct (Fortran) pointers to actual shared memory
    type(control_shared_memory),              pointer :: shmem                 => NULL() !< Will point to start of control shared memory
    type(shared_server_stream), dimension(:), pointer :: common_server_streams => NULL() !< Stream files used to assemble grids and write them (in shared memory)
    !> @}

    !-------------------
    !> @{ \name Stuff
    integer :: model_relay_smp_rank = -1    !< rank in model_relay_smp_comm
    integer :: model_relay_smp_size =  0    !< population of model_relay_smp_comm
    integer :: server_comm_rank     = -1    !< rank in server_comm
    integer :: num_local_model_proc = -1    !< Number of model processes on this SMP node

    integer :: max_smp_pe     =  0 !< Highest ID/rank of PEs on this node
    integer :: max_relay_rank = -1 !< Highest rank of relay PEs on this node
    integer :: max_model_rank = -1 !< Highest rank of model PEs on this node
    integer, dimension(:), pointer :: node_relay_ranks  => NULL() !< ranks of relay PEs on this node
    integer, dimension(:), pointer :: node_model_ranks  => NULL() !< ranks of model PEs on this node

    integer :: num_server_stream_owners = -1 !< How many server processes can own a stream (can be lower than number of server-bound processes)
    integer :: node_id = -1

    type(model_stream), dimension(:), pointer :: local_model_streams => NULL() !< List of opened streams, for easy access
    !> @}

    !----------------------
    !> @{ \name Some stats
    integer :: num_nodes                = 0
    integer :: num_model_nodes          = 0
    integer :: num_server_bound_relays  = 0
    integer :: num_model_bound_relays   = 0
    integer :: num_model_pes            = 0
    integer :: num_server_noop          = 0
    !> @}

    ! ------------------
    !> @{ \name Miscellaneous
    type(ioserver_messenger), pointer :: messenger => NULL() !< Will be shared among open model files
    !> @}

  contains
    private

    !> @{ \name Initialization
    procedure, pass, public :: init                     => ioctx_init
    procedure, pass         :: init_communicators       => ioctx_init_communicators
    procedure, pass         :: init_shared_mem          => ioctx_init_shared_mem
    procedure, pass         :: build_relay_model_index  => ioctx_build_relay_model_index
    procedure, pass         :: fetch_node_shmem_structs => ioctx_fetch_node_shmem_structs
    procedure, pass         :: allocate_from_arena      => ioctx_allocate_from_arena
    procedure, pass         :: create_local_heap        => ioctx_create_local_heap
    procedure, pass         :: create_local_cb          => ioctx_create_local_cb
    !> @}

    !> @{ \name Process type query
    procedure, pass, public :: is_relay
    procedure, pass, public :: is_server
    procedure, pass, public :: is_model
    procedure, pass, public :: is_server_bound
    procedure, pass, public :: is_model_bound
    procedure, pass, public :: is_channel
    procedure, pass, public :: is_grid_processor
    procedure, pass, public :: is_no_op
    !> @}

    !> @{ \name Finalization
    procedure, pass         :: set_time_to_quit
    procedure, pass         :: finalize_model
    procedure, pass         :: finalize_relay
    procedure, pass         :: finalize_server
    procedure, pass, public :: finalize => ioserver_context_finalize_manually
    final                   :: ioserver_context_finalize
    !> @}

    !> @{ \name Getters
    procedure, pass         :: is_initialized
    procedure, pass, public :: is_time_to_quit
    procedure, pass, public :: get_num_local_model
    procedure, pass, public :: get_num_total_model

    procedure, pass, public :: get_global_rank
    procedure, pass, public :: get_crs => IOserver_get_crs
    procedure, pass, public :: get_local_heap => IOserver_get_local_heap
    procedure, pass, public :: get_node_heap => IOserver_get_node_heap
    procedure, pass, public :: get_server_bound_cb => IOserver_get_server_bound_cb
    procedure, pass, public :: get_model_bound_cb => IOserver_get_model_bound_cb
    procedure, pass, public :: get_dcb => IOserver_get_dcb
    procedure, pass, public :: get_messenger => IOserver_get_messenger
    procedure, pass, public :: get_stream => IOserver_get_stream
    procedure, pass, public :: get_relay_pipeline_depth
    procedure, pass, public :: get_server_pipeline_depth
    procedure, pass, public :: get_max_num_streams

    procedure, pass, public :: get_server_bound_cb_list
    procedure, pass, public :: get_heap_list
    !> @}

    !> @{ \name Process function management
    procedure, pass, public :: no_op => IOserver_noop
    !> @}

    !> @{ \name File management
    procedure, pass, public :: open_stream_model
    ! procedure, pass, public :: open_stream_server
    ! procedure, pass, public :: close_stream_server
    !> @}
    
    !> @{ \name Debugging
    procedure, pass, public :: set_debug_level  !< Debug level setter. \sa ioserver_input_parameters::debug_level
    procedure, pass, public :: get_debug_level  !< Debug level getter. \sa ioserver_input_parameters::debug_level
    procedure, pass, public :: get_detailed_pe_name

    procedure, pass :: print_io_colors
    procedure, pass :: print_shared_mem_sizes
    !> @}
  end type ioserver_context

  abstract interface
    function no_op_function_template(context) result(no_op_success)
      import :: ioserver_context
      implicit none
      type(ioserver_context), intent(inout) :: context !< IO server context with which the model will operate
      logical :: no_op_success !< Whether the function terminated successfully
    end function no_op_function_template
  end interface

  interface 
    module function ioctx_init(context, params) result(success)
      implicit none
      class(ioserver_context),         intent(inout) :: context !< The context we are initialising ("this")
      type(ioserver_input_parameters), intent(in)    :: params  !< All details about how it should be initialized
      logical :: success
    end function ioctx_init

    module function ioctx_init_communicators(context) result(success)
      implicit none
      class(ioserver_context), intent(inout) :: context
      logical :: success
    end function ioctx_init_communicators

    module function ioctx_init_shared_mem(context) result(success)
      implicit none
      class(ioserver_context), intent(inout) :: context
      logical :: success
    end function ioctx_init_shared_mem

    module subroutine ioctx_build_relay_model_index(context)
      implicit none
      class(ioserver_context), intent(inout) :: context
    end subroutine ioctx_build_relay_model_index

    module function ioctx_fetch_node_shmem_structs(context) result(success)
      implicit none
      class(ioserver_context), intent(inout) :: context
      logical :: success
    end function

    module function ioctx_allocate_from_arena(context, num_bytes, name, id) result(ptr)
      implicit none
      class(ioserver_context), intent(inout) :: context
      integer(C_SIZE_T),       intent(in)    :: num_bytes !< Size of the block (in bytes)
      character(len=4),        intent(in)    :: name      !< Short name for the block
      integer,                 intent(in)    :: id        !< ID of the process that created the block
      type(C_PTR) :: ptr
    end function ioctx_allocate_from_arena

    module function ioctx_create_local_heap(context, num_bytes) result(success)
      implicit none
      class(ioserver_context), intent(inout) :: context
      integer(C_SIZE_T),       intent(in)    :: num_bytes !< Desired size of the heap (in bytes)
      logical :: success
    end function ioctx_create_local_heap

    module function ioctx_create_local_cb(context, num_bytes, is_output) result(success)
      implicit none
      class(ioserver_context), intent(inout) :: context
      integer(C_SIZE_T),       intent(in)    :: num_bytes !< Desired size of the circular buffer (in bytes)
      logical,                 intent(in)    :: is_output !< Whether the CB will be used for output (server-bound) or input (model-bound)
      logical :: success
    end function ioctx_create_local_cb
  end interface

contains

function default_no_op(context) result(no_op_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: no_op_success
  no_op_success = .false.
  call context % no_op()
  no_op_success = .true.
end function default_no_op

!> Whether this context belongs to a relay process (server- or model-bound)
function is_relay(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_relay
  is_relay = is_color_relay(context % color)
end function is_relay

!> Whether this context belongs to a server process
function is_server(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_server
  is_server = is_color_server(context % color)
end function is_server

!> Whether this context belongs to a model process
function is_model(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_model
  is_model = is_color_model(context % color)
end function is_model

!> Whether this context belongs to a process that handles communications going towards the server
function is_server_bound(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_server_bound
  is_server_bound = iand(context % color, SERVER_BOUND_COLOR) == SERVER_BOUND_COLOR
end function is_server_bound

!> Whether this context belongs to a process that handles communications going towards the model
function is_model_bound(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_model_bound
  is_model_bound = iand(context % color, MODEL_BOUND_COLOR) == MODEL_BOUND_COLOR
end function is_model_bound

!> Whether this context belongs to a process that serves as a MPI communication channel (on server only)
function is_channel(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_channel
  is_channel = iand(context % color, CHANNEL_COLOR) == CHANNEL_COLOR
end function is_channel

!> Whether this context belongs to a process that processes completed grids on the server
function is_grid_processor(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_grid_processor
  is_grid_processor = iand(context % color, GRID_PROCESSOR_COLOR) == GRID_PROCESSOR_COLOR
end function is_grid_processor

!> Whether this context belongs to a NO-OP process
function is_no_op(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_no_op
  is_no_op = iand(context % color, NO_OP_COLOR) == NO_OP_COLOR
end function is_no_op

!> Whether debug mode is enabled for this context
function get_debug_level(context) result(debug_level)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: debug_level
  debug_level = context % params % debug_level
end function get_debug_level

function get_detailed_pe_name(context) result(detailed_name)
  implicit none
  class(ioserver_context), intent(in) :: context
  character(len=:), allocatable :: detailed_name
  character(len=20)  :: short_name
  character(len=256) :: temp_name

  if (context % is_server()) then
    if (context % is_server_bound()) then
      short_name = 'Server/server-bound'
    else if (context % is_model_bound()) then
      short_name = 'Server/model-bound'
    else if (context % is_channel()) then
      short_name = 'Server/channel'
    else if (context % is_grid_processor()) then
      short_name = 'Server/grid worker'
    else
      short_name = 'Server/[unknown]'
    end if
  else if (context % is_relay()) then
    if (context % is_server_bound()) then
      short_name = 'Relay/server-bound'
    else if (context % is_model_bound()) then
      short_name = 'Relay/model-bound'
    else
      short_name = 'Relay/[unknown]'
    end if
  else if (context % is_model()) then
    short_name = 'Model'
  else if (context % is_no_op()) then
    short_name = 'No-op'
  else
    short_name = '[unknown]'
  end if

  write(temp_name, '(A, A, I6, A, I6, A, I5, A, I5, A, I6)')         &
      short_name, ' | active rank ', context % active_rank, '/', context % active_size, ' | node ', context % node_id, '/', context % num_nodes, &
      ' | global rank ', context % get_global_rank()

  detailed_name = trim(temp_name)
end function get_detailed_pe_name

!> Open a stream where the model can write data
subroutine open_stream_model(context, new_stream)
  implicit none
  class(ioserver_context),     intent(inout) :: context     !< io-server context instance
  type(model_stream), pointer, intent(out)   :: new_stream  !< A pointer to a newly-opened stream. NULL() if there was an error

  integer :: i_stream
  type(model_stream), pointer :: tmp_stream

  nullify(new_stream)

  if (.not. context % is_initialized()) then
    print *, 'ERROR: Cannot open file, context is *not* initialized.'
    return
  end if

  do i_stream = 1, context % params % max_num_concurrent_streams
    tmp_stream => context % local_model_streams(i_stream)
    if (.not. tmp_stream % is_open()) then
      if (tmp_stream % open()) then
        new_stream => tmp_stream
      else
        print '(A, I3)', 'ERROR: Unable to open new model stream with rank ', i_stream
      end if
      return
    end if
  end do

  print *, 'ERROR: No space left in the list of (model) streams to open a new one'
end subroutine open_stream_model

!> Set time to quit flag in control area
subroutine set_time_to_quit(context)
  implicit none
  class(ioserver_context), intent(inout) :: context
  context % shmem % time_to_quit = 1
  if (context % get_debug_level() >= 2) print '(A, A)', 'DEBUG: time to quit ', context % get_detailed_pe_name()
end subroutine set_time_to_quit

!> Check whether the "time-to-quit" flag has been set on this node
function is_time_to_quit(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_time_to_quit
  is_time_to_quit = (context % shmem % time_to_quit == 1)
end function is_time_to_quit

subroutine print_io_colors(context)
    implicit none
    class(ioserver_context), intent(in) :: context
    write(6,'(A,(15I5))')' DEBUG: colors =', context % shmem % pe(0:context % max_smp_pe) % color
end subroutine print_io_colors

!> Get number of model processes on this node
function get_num_local_model(context) result(num_model)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: num_model
  num_model = context % num_local_model_proc
end function get_num_local_model

!> Get total number of model PEs that connect to the IO server
function get_num_total_model(context) result(num_model)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: num_model
  num_model = context % num_model_pes
end function get_num_total_model

!> Get rank of this process on the global communicator
function get_global_rank(context) result(global_rank)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: global_rank
  global_rank = context % global_rank
end function get_global_rank

!> Get the comm_rank_size object corresponding to the given input color
function IOserver_get_crs(context, color) result(crs)
  implicit none
  class(ioserver_context), intent(in)        :: context
  integer,                 intent(IN), value :: color
  type(comm_rank_size) :: crs

  integer :: ierr

  crs = comm_rank_size(MPI_COMM_NULL, -1, 0)
  
  select case(color)
    case(NO_COLOR)                                ! Everyone that did context init (i.e. global_comm)
      crs % comm = context % global_comm

    case(SERVER_COLOR + MODEL_COLOR + RELAY_COLOR)! all non NO-OP PEs               (subset of global_comm)
      crs % comm = context % active_comm

    case(NODE_COLOR)                              ! all PEs on this SMP node        (subset of global_comm)
      crs % comm = context % node_comm

    case(SERVER_COLOR - CHANNEL_COLOR)            ! server PEs that can do work     (subset of server_comm, itself a subset of active_comm. excludes channel)
      crs % comm = context % server_work_comm

    case(MODEL_COLOR + RELAY_COLOR)               ! compute and relay PEs           (subset of active_comm)
      crs % comm = context % model_relay_comm

    case(MODEL_COLOR)                             ! all model compute PEs           (subset of active_comm, model_relay_comm)
      crs % comm = context % model_comm

    case(RELAY_COLOR)                             ! all IO relay PEs                (subset of active_comm, model_relay_comm)
      crs % comm = context % relay_comm

    case(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)  ! compute and relay PEs on SMP node (subset of node_comm, model_comm, iorelay_comm)
      crs % comm = context % model_relay_smp_comm

    case(MODEL_COLOR + NODE_COLOR)                ! compute PEs on SMP node         (subset of  node_comm, model_comm)
      crs % comm = context % model_smp_comm

    case(RELAY_COLOR + NODE_COLOR)                ! relay PEs on SMP node           (subset of  node_comm, iorelay_comm)
      crs % comm = context % relay_smp_comm
    
    case(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR) ! server-bound relay PEs on SMP node (subset of relay_smp_comm)
      crs % comm = context % server_bound_relay_smp_comm

    case(RELAY_COLOR + SERVER_COLOR)              ! relay and server PEs            (subset of active_comm)
      crs % comm = context % io_comm

    case(SERVER_COLOR)                            ! Active server PEs
      crs % comm = context % server_comm

    case(GRID_PROCESSOR_COLOR)
      crs % comm = context % grid_processor_server_comm

    case(GRID_PROCESSOR_COLOR + SERVER_COLOR)    ! Same as just "GRID_PROCESSOR_COLOR"
      crs % comm = context % grid_processor_server_comm

    case(SERVER_COLOR + SERVER_BOUND_COLOR)
      crs % comm = context % server_bound_server_comm

    case(SERVER_COLOR + MODEL_BOUND_COLOR)
      crs % comm = context % model_bound_server_comm

    case(CHANNEL_COLOR + SERVER_BOUND_COLOR + MODEL_BOUND_COLOR)
      crs % comm = context % server_dcb_comm

    case default
      crs % comm = MPI_COMM_NULL
  end select

  if(crs % comm .ne. MPI_COMM_NULL) then
    call MPI_Comm_rank(crs % comm, crs % rank, ierr)
    call MPI_Comm_size(crs % comm, crs % size, ierr)
  endif
end function IOserver_get_crs

!> Get the local shmem_heap that belongs to this PE
function IOserver_get_local_heap(context) result(h)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(shmem_heap) :: h
  h = context % local_heap
end function IOserver_get_local_heap

!> Get the local shmem_heap that belongs to this node (server only?)
function IOserver_get_node_heap(context) result(h)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(shmem_heap) :: h
  h = context % node_heap
end function IOserver_get_node_heap

!> Get the local accessor to the model-bound CB that belongs to this process
function IOserver_get_model_bound_cb(context) result(cio)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer) :: cio
  cio = context % local_cio_in
end function IOserver_get_model_bound_cb

!> Get the local accessor to the server-bound CB that belongs to this process
function IOserver_get_server_bound_cb(context) result(cio)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer) :: cio
  cio = context % local_server_bound_cb
end function IOserver_get_server_bound_cb

!> Get the DCB created when initializing this context
function IOserver_get_dcb(context) result(dcb)
  implicit none
  class(ioserver_context), intent(in) :: context
  type(distributed_circular_buffer) :: dcb
  dcb = context % local_dcb
end function IOserver_get_dcb

!> Get the messenger object created for this context
function IOserver_get_messenger(context) result(messenger)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(ioserver_messenger), pointer :: messenger
  messenger => context % messenger
end function IOserver_get_messenger

!> Get a local accessor to a specific stream
subroutine IOserver_get_stream(context, stream_rank, stream)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_rank !< Rank of the stream we want to access
  type(local_server_stream), pointer, intent(out) :: stream
  stream => context % local_server_streams(stream_rank)
end subroutine IOserver_get_stream

!> Get the value of relay_pipeline_depth
function get_relay_pipeline_depth(context) result(depth)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: depth
  depth = context % params % relay_pipeline_depth
end function get_relay_pipeline_depth

!> Get the value of server_pipeline_depth
function get_server_pipeline_depth(context) result(depth)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: depth
  depth = context % params % server_pipeline_depth
end function get_server_pipeline_depth

!> Get the max number of streams that can be open concurrently
function get_max_num_streams(context) result(num_streams)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer :: num_streams
  num_streams = context % params % max_num_concurrent_streams
end function get_max_num_streams

!> Get the list of local accessors to the server-bound CBs created on this node
subroutine get_server_bound_cb_list(context, cb_list)
  implicit none
  class(ioserver_context),                      intent(in)  :: context !< The ioserver_context instance
  type(circular_buffer), dimension(:), pointer, intent(out) :: cb_list !< Pointer to the local list of server-bound CBs
  cb_list => context % server_bound_cbs
end subroutine get_server_bound_cb_list

!> Get the list of local accessors to the heaps created on this node
subroutine get_heap_list(context, heap_list)
  implicit none
  class(ioserver_context), intent(in)  :: context   !< The ioserver_context instance
  type(shmem_heap), dimension(:), pointer, intent(out) :: heap_list !< Pointer to the local list of model heaps
  heap_list => context % local_heaps
end subroutine get_heap_list

!> Set the debug flag for this context
subroutine set_debug_level(context, level)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer, intent(in) :: level
  context % params % debug_level = min(max(level, 0), 2)
  if (context % get_debug_level() > 0) print '(A, 1X, I2)', 'INFO: debug level =', context % get_debug_level()
end subroutine set_debug_level

!> NO OP loop to park processes with minimal CPU consumption
subroutine IOserver_noop(context) 
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer :: sleep_dummy

  interface
    function sleep(nsec) result(left) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: nsec
      integer(C_INT) :: left
    end function sleep
  end interface

  do while (.not. context % is_time_to_quit())    ! sleep loop until quit flag appears
    sleep_dummy = sleep(1)
  enddo
end subroutine IOserver_noop

!> Check whether this context has been successfully initialized
function is_initialized(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_initialized
  is_initialized = context % color .ne. NO_COLOR
end function is_initialized

!> Finalize this context (on model PEs)
subroutine finalize_model(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(message_header) :: header
  type(message_cap)    :: end_cap
  type(cb_stats), pointer :: stats
  logical :: success

  success = .false.
  if (this % is_model()) then
    ! Send statistics
    stats => this % local_server_bound_cb % get_stats()
    call this % messenger % bump_tag()
    header % content_size_int8  = cb_stats_size_int8()
    header % command            = MSG_COMMAND_MODEL_STATS
    header % message_tag        = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank
    end_cap % msg_length        = header % content_size_int8
    success = this % local_server_bound_cb % put(header, message_header_size_int8(), CB_KIND_INTEGER_8, .false.)
    success = this % local_server_bound_cb % put(stats, cb_stats_size_int8(), CB_KIND_INTEGER_8, .false.)     .and. success
    success = this % local_server_bound_cb % put(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.) .and. success

    ! Send a signal towards the server to indicate that this PE will no longer send anything
    call this % messenger % bump_tag()
    header % content_size_int8  = 0
    header % command            = MSG_COMMAND_MODEL_STOP
    header % message_tag        = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank
    end_cap % msg_length        = header % content_size_int8
    success = this % local_server_bound_cb % put(header, message_header_size_int8(), CB_KIND_INTEGER_8, .false.) .and. success
    success = this % local_server_bound_cb % put(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.)    .and. success
  else
    print '(A)', 'WARNING: Should NOT be calling "finish_model"'
  end if
end subroutine finalize_model

!> Finalize this context (on relay PEs)
subroutine finalize_relay(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(message_header) :: header
  type(message_cap)    :: end_cap
  logical :: success

  if (this % is_relay() .and. this % is_server_bound()) then
    ! Send a stop signal to the server
    if (this % get_debug_level() >= 1) print '(A, I3, A)', 'DEBUG: Relay ', this % local_dcb % get_server_bound_client_id() , ' sending STOP signal'
    header % content_size_int8  = 0
    header % command            = MSG_COMMAND_RELAY_STOP
    header % sender_global_rank = this % global_rank
    header % relay_global_rank  = this % global_rank
    end_cap % msg_length        = header % content_size_int8

    success = this % local_dcb % put_elems(header, message_header_size_int8(), CB_KIND_INTEGER_8, .false.)
    success = this % local_dcb % put_elems(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.) .and. success

    if (.not. success) then
      if (this % get_debug_level() >= 1) print '(A)', 'WARNING: Relay could not send a stop signal!!!'
      call print_message_header(header)
    end if
  end if
end subroutine finalize_relay

!> Finalize this context (on server PEs)
subroutine finalize_server(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(local_server_stream), pointer :: stream
  integer :: i, ierr

  ! Print data/stats from grid processors
  if (this % is_grid_processor()) then
    block
      type(comm_rank_size) :: grid_crs
      grid_crs = this % get_crs(GRID_PROCESSOR_COLOR)

      ! Grid processor barrier
      call MPI_Barrier(grid_crs % comm, ierr)

      if (grid_crs % rank == 0) then
        print '(A, /, A)',      &
            '------------------------------------------------------------------',     &
            '  Command buffers for stream processors'
        call this % local_server_streams(1) % print_command_stats(1, .true.)
      end if

      ! Grid processor barrier
      call MPI_Barrier(grid_crs % comm, ierr)

      do i = 2, this % params % max_num_concurrent_streams
        stream => this % local_server_streams(i)
        if (stream % is_owner()) call stream % print_command_stats(i, .false.)
      end do

      do i = 1, this % params % max_num_concurrent_streams
        stream => this % local_server_streams(i)
        if (stream % is_open()) then
          if (stream % is_owner()) then
            print '(A, I4, A, A)', 'WARNING: Heeeeeyyyy forgot to close stream #', stream % get_id(), ', owned by myself ', this % get_detailed_pe_name()
          end if
        end if
      end do

    end block
  end if

  if (associated(this % local_server_streams)) then
    ! Close all owned streams
    deallocate(this % local_server_streams)
    nullify(this % local_server_streams)
  end if

  ! Sync non-channel processes here
  if (.not. this % is_channel()) call MPI_Barrier(this % server_work_comm, ierr)
end subroutine finalize_server

!> Finalize function that can be called explicitly
subroutine ioserver_context_finalize_manually(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  if (this % is_initialized()) then
    call this % set_time_to_quit()

    if (this % is_server()) then
      call this % finalize_server()
    else if (this % is_model()) then
      call this % finalize_model()
    else if (this % is_relay()) then
      call this % finalize_relay()
    end if

    if (associated(this % messenger)) deallocate(this % messenger)

    if (associated(this % node_relay_ranks)) deallocate(this % node_relay_ranks)
    if (associated(this % node_model_ranks)) deallocate(this % node_model_ranks)

    if (associated(this % model_bound_cbs)) deallocate(this % model_bound_cbs)
    if (associated(this % server_bound_cbs)) deallocate(this % server_bound_cbs)
    if (associated(this % local_heaps)) deallocate(this % local_heaps)

    if (associated(this % local_model_streams)) deallocate(this % local_model_streams)

    call this % local_dcb % delete() ! This will block if not everyone calls it

    this % color = NO_COLOR
  end if
end subroutine ioserver_context_finalize_manually

subroutine ioserver_context_finalize(context)
  implicit none
  type(ioserver_context), intent(inout) :: context
  call context % finalize()
end subroutine ioserver_context_finalize

subroutine print_shared_mem_sizes(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  if (context % is_server()) then
    print '(A,F8.1,A)', '(Server node) Shared memory heap: ', context % params % server_heap_size_mb, ' MB'
    if (.not. context % is_grid_processor()) then
      if (context % local_dcb % is_valid()) then
        print '(A,F8.1,A)', '(Server node) Server-bound DCB:   ', context % params % dcb_server_bound_size_mb * context % local_dcb % get_num_server_bound_clients(), ' MB (total)'
        print '(A,F8.1,A)', '(Server node) Model-bound DCB:    ', context % params % dcb_model_bound_size_mb, ' MB (per client)'
      end if
    end if
  else
    print '(A,F8.1,A)', '(Model node)  Shared memory heap: ', context % params % model_heap_size_mb, ' MB'
    print '(A,F8.1,A)', '(Model node)  Server-bound CB:    ', context % params % server_bound_cb_size_mb, ' MB'
    print '(A,F8.1,A)', '(Model node)  Model-bound CB:     ', context % params % model_bound_cb_size_mb, ' MB'
    if (context % is_relay()) then
      print '(A,F8.1,A)', '(Relay)       Server-bound DCB:   ', &
        real(context % local_dcb % get_capacity_local(CB_KIND_CHAR), kind=4) / MBYTE, ' MB'
    end if
  end if
end subroutine print_shared_mem_sizes

end module ioserver_context_module
