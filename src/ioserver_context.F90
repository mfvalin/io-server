!  functions for C and FORTRAN programming
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
  use mpi_f08

  use circular_buffer_module
  use distributed_circular_buffer_module
  use heap_module
  use ioserver_constants
  use ioserver_message_module
  use model_stream_module
  use server_stream_module
  use shmem_arena_mod
  implicit none

  private
  save

  ! Publish some types and constants that are useful
  public :: heap, circular_buffer, distributed_circular_buffer, comm_rank_size, model_stream, local_server_stream
  public :: MODEL_COLOR, SERVER_COLOR, RELAY_COLOR, SERVER_BOUND_COLOR, MODEL_BOUND_COLOR, NODE_COLOR, NO_COLOR

  integer, parameter, public :: MAX_NUM_SERVER_STREAMS = 128 !< How many streams we can open within a single context
  integer, parameter         :: MAX_PES_PER_NODE       = 128 !< How many PEs per node we can manage

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

    !> @{ \name Pipeline control
    !> Maximum difference of tags that a relay can transmit before starting to wait for the slower model PEs on the node.
    !> Ideally, should be lower than MAX_ASSEMBLY_LINES
    integer :: relay_pipeline_depth = 5

    !> Maximum difference of tags that a server can process (between the various relays it receives from), before starting to
    !> wait for the slower relays
    !> Ideally, should be lower than MAX_ASSEMBLY_LINES. _Must be at least as large as #relay_pipeline_depth_
    integer :: server_pipeline_depth = 10
    !> @}

    logical :: debug_mode = .false.
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
    type(MPI_Comm) :: global_comm = MPI_COMM_WORLD  !< MPI "WORLD" for this set of PEs
    integer        :: global_rank = -1              !< rank in global_comm
    integer        :: global_size =  0              !< population of global_comm

    type(MPI_Comm) :: smp_comm    = MPI_COMM_NULL   !< PEs on this SMP node        (any kind ) (subset of global_comm)
    integer        :: smp_rank    = -1              !< rank in smp_comm
    integer        :: smp_size    =  0              !< population of smp_comm

    type(MPI_Comm) :: active_comm = MPI_COMM_NULL   !< All "active" PEs (non NO-OP) (subset of global_comm)
    integer        :: active_rank = -1              !< Rank on active_comm
    integer        :: active_size =  0              !< Size of active_comm (number of active PEs)

    type(MPI_Comm) :: io_comm                     = MPI_COMM_NULL  !< all IO PEs     (relay + server) (subset of active_comm)

    type(MPI_Comm) :: model_relay_comm            = MPI_COMM_NULL  !< model and relay PEs (all nodes) (subset of active_comm)
    type(MPI_Comm) :: model_comm                  = MPI_COMM_NULL  !< model PEs           (all nodes) (subset of model_relay_comm)
    type(MPI_Comm) :: relay_comm                  = MPI_COMM_NULL  !< relay PEs           (all nodes) (subset of model_relay_comm)

    type(MPI_Comm) :: server_comm                 = MPI_COMM_NULL  !< IO server PEs (subset of active_comm)
    type(MPI_Comm) :: server_work_comm            = MPI_COMM_NULL  !< Server PEs that process data (subset of server_comm)
    type(MPI_Comm) :: server_bound_server_comm    = MPI_COMM_NULL  !< Server PEs that process server-bound relay data (subset of server_work_comm)
    type(MPI_Comm) :: model_bound_server_comm     = MPI_COMM_NULL  !< Server PEs that process model-bound relay data (subset of server_work_comm)

    type(MPI_Comm) :: model_relay_smp_comm        = MPI_COMM_NULL  !< model+relays on current node
    type(MPI_Comm) :: model_smp_comm              = MPI_COMM_NULL  !< model PEs on current SMP node
    type(MPI_Comm) :: relay_smp_comm              = MPI_COMM_NULL  !< relay PEs on current SMP node
    type(MPI_Comm) :: server_bound_relay_smp_comm = MPI_COMM_NULL  !< server-bound relay PEs on current SMP node, subset of relay_smp_comm
    !> @}

    !----------------------------------------------------
    !> @{ \name Local instances of objects located in shared memory
    type(shmem_arena)      :: arena                 !< Node memory arena
    type(heap)             :: local_heap            !< Local heap for this process (located in memory arena)
    type(circular_buffer)  :: local_cio_in          !< Model-bound circular buffer for this process (located in memory arena)
    type(circular_buffer)  :: local_server_bound_cb !< Server-bound circular buffer for this process (located in memory arena)
    type(distributed_circular_buffer) :: local_dcb  !< Distributed circular buffer for communication b/w relay and server processes
    type(heap)             :: node_heap             !< On server node only. Heap that everyone on the node can use

    type(circular_buffer),     dimension(:), pointer :: model_bound_cbs      => NULL() !< The CB objects belonging to model PEs (model-bound)
    type(circular_buffer),     dimension(:), pointer :: server_bound_cbs     => NULL() !< The CB objects belonging to model PEs (server-bound)
    type(heap),                dimension(:), pointer :: local_heaps          => NULL() !< Shared memory heaps belonging to model PEs
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
    integer, dimension(:), pointer :: iocolors => NULL()     !< Color table for io server and relay processes
    type(ioserver_messenger), pointer :: messenger => NULL() !< Will be shared among open model files
    !> @}

  contains
    private

    !> @{ \name Initialization
    procedure, pass, public :: init => ioserver_context_init
    procedure, pass         :: init_communicators
    procedure, pass         :: init_shared_mem
    procedure, pass         :: build_relay_model_index
    procedure, pass         :: build_print_model_index
    procedure, pass         :: fetch_node_shmem_structs
    procedure, pass         :: is_initialized
    procedure, pass         :: allocate_from_arena
    procedure, pass         :: create_local_heap
    procedure, pass         :: create_local_cb
    procedure, nopass       :: check_cb_jar_elem
    !> @}

    !> @{ \name Process type query
    procedure, pass, public :: is_relay
    procedure, pass, public :: is_server
    procedure, pass, public :: is_model
    procedure, pass, public :: is_server_bound
    procedure, pass, public :: is_model_bound
    procedure, pass, public :: is_channel
    procedure, pass, public :: is_grid_processor
    !> @}

    !> @{ \name Finalization
    procedure, pass         :: set_time_to_quit
    procedure, pass         :: is_time_to_quit
    procedure, pass         :: finalize_model
    procedure, pass         :: finalize_relay
    procedure, pass         :: finalize_server
    procedure, pass, public :: finalize => ioserver_context_finalize_manually
    final                   :: ioserver_context_finalize
    !> @}

    !> @{ \name Getters
    procedure, pass, public :: get_num_local_model

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

    procedure, pass, public :: get_server_bound_cb_list
    procedure, pass, public :: get_heap_list
    !> @}

    !> @{ \name Process function management
    procedure, pass         :: no_op => IOserver_noop
    procedure, pass, public :: set_debug => IOserver_set_debug
    !> @}

    !> @{ \name File management
    procedure, pass, public :: open_stream_model
    procedure, pass, public :: open_stream_server
    procedure, pass, public :: close_stream_server
    !> @}
    
    !> @{ \name Debugging
    procedure, pass, public :: debug_mode => ioserver_context_debug_mode

    procedure, pass :: print_io_colors
    procedure, pass :: print_shared_mem_sizes
    !> @}
  end type ioserver_context

contains

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

!> Whether debug mode is enabled for this context
function ioserver_context_debug_mode(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: ioserver_context_debug_mode
  ioserver_context_debug_mode = context % params % debug_mode
end function ioserver_context_debug_mode

!> Open a stream where the model can write data
!> \return A stream object that is already open and can be written to
function open_stream_model(context, filename) result(new_file)
  implicit none
  class(ioserver_context), intent(inout) :: context
  character(len=*),        intent(in)    :: filename !< Base name of the file to open
  type(model_stream) :: new_file

  logical :: success

  if (.not. context % is_initialized()) then
    print *, 'ERROR: Cannot open file, context is *not* initialized.'
    error stop 1
  end if

  if (context % debug_mode()) print *, 'DEBUG: (Model) Opening file with name ', filename

  new_file = model_stream(context % global_rank, &
                          context % local_heap, &
                          context % local_server_bound_cb, &
                          context % debug_mode(), &
                          context % messenger)
  success = new_file % open(filename)

  if (.not. success) print *, 'ERROR: Unable to open file, for some reason'
end function open_stream_model

!> Open a stream on the server (if this process is the owner, will actually open the shared instance)
!> \return A pointer to the local stream instance able to access the underlying shared memory stream
!> \sa server_stream_module::local_server_stream, server_stream_module::shared_server_stream
function open_stream_server(context, filename, stream_id) result(new_stream)
  implicit none
  class(ioserver_context), intent(inout) :: context
  character(len=*),        intent(in)    :: filename  !< Base name of the file to open
  integer,                 intent(in)    :: stream_id !< ID of the stream to open (they should all be already initialized, we just need to open them)
  type(local_server_stream), pointer :: new_stream

  type(local_server_stream), pointer :: tmp_stream
  logical :: success

  nullify(new_stream)
  tmp_stream => context % local_server_streams(stream_id)

  success = tmp_stream % open(filename)
  
  if (.not. success) then
    print *, 'ERROR: Unable to properly open stream file ', filename, stream_id
    error stop 1
  end if

  new_stream => tmp_stream
end function open_stream_server

!> Close the specified server stream
!> \return Whether the operation was successful
function close_stream_server(context, stream_id, force_close) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_id   !< ID of the stream to close
  logical,                 intent(in)    :: force_close !< Whether to close the stream even if some grids within it are incomplete
  logical :: success
  success = context % local_server_streams(stream_id) % close(force_close)
end function close_stream_server

!> Fetch the shared memory structs created by other model PEs on this node and initialize the
!> corresponding access to them on this PE. There is a set of heap, model-bound CBs and
!> server-bound CBs
subroutine fetch_node_shmem_structs(context)
  implicit none
  class(ioserver_context), intent(inout) :: context

  integer             :: i
  integer(C_INTPTR_T) :: new
  type(C_PTR)         :: my_base, local_addr
  logical             :: success
  integer :: target_rank

  my_base = context % shmem % pe(context % smp_rank) % arena_ptr

  do i = 0, context % max_smp_pe ! Loop on every PE on this node
    if (is_color_model(context % shmem % pe(i) % color)) then ! Only work on model PEs

      target_rank = context % shmem % pe(i) % rank

      ! Model-bound CB
      new        = transfer(my_base, new)                                 ! make large integer from C pointer
      new        = new + context % shmem % pe(i) % model_bound_cb_offset  ! add offset to my base
      local_addr = transfer(new, local_addr)                              ! honest C pointer
      success    = context % model_bound_cbs(target_rank) % create_bytes(local_addr)   ! Initialize the local circular buffer structure

      if (.not. success) then
        print *, 'ERROR: Could not fetch input CB from PE ', i
        error stop 1
      end if

      ! Server-bound CB
      new        = transfer(my_base, new)                                 ! make large integer from C pointer
      new        = new + context % shmem % pe(i) % server_bound_cb_offset ! add offset to my base
      local_addr = transfer(new, local_addr)                              ! honest C pointer
      success    = context % server_bound_cbs(target_rank) % create_bytes(local_addr)  ! Initialize the local circular buffer structure

      if (.not. success) then
        print *, 'ERROR: Could not fetch output CB from PE ', i
        error stop 1
      end if

      ! The heap
      new        = transfer(my_base, new)
      new        = new + context % shmem % pe(i) % heap_offset
      local_addr = transfer(new, local_addr)
      success    = context % local_heaps(target_rank) % clone(local_addr)
    end if
  end do
end subroutine fetch_node_shmem_structs

!> Set time to quit flag in control area
subroutine set_time_to_quit(context)
  implicit none
  class(ioserver_context), intent(inout) :: context
  context % shmem % time_to_quit = 1
  if (context % debug_mode()) then 
    if (context % is_relay()) then
      print *, 'DEBUG: time to quit (relay)'
    else if (context % is_model()) then
      print *, 'DEBUG: time to quit (model)'
    else if (context % is_server()) then
      print *, 'DEBUG: time to quit (server)'
    else
      print *, 'DEBUG: time to quit (other)'
    end if
  end if
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

  crs = comm_rank_size(MPI_COMM_NULL, -1, 0)
  
  select case(color)
    case(NO_COLOR)                                ! all non NO-OP PEs               (subset of global_comm)
      crs % comm = context % active_comm

    case(NODE_COLOR)                              ! all PEs on this SMP node        (subset of global_comm)
      crs % comm = context % smp_comm

    case(SERVER_COLOR)                            ! server PEs that can do work     (subset of server_comm, itself a subset of active_comm)
      crs % comm = context % server_work_comm

    case(MODEL_COLOR + RELAY_COLOR)               ! compute and relay PEs           (subset of active_comm)
      crs % comm = context % model_relay_comm

    case(MODEL_COLOR)                             ! all model compute PEs           (subset of active_comm, model_relay_comm)
      crs % comm = context % model_comm

    case(RELAY_COLOR)                             ! all IO relay PEs                (subset of active_comm, model_relay_comm)
      crs % comm = context % relay_comm

    case(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)  ! compute and relay PEs on SMP node (subset of smp_comm, model_comm, iorelay_comm)
      crs % comm = context % model_relay_smp_comm

    case(MODEL_COLOR + NODE_COLOR)                ! compute PEs on SMP node         (subset of  smp_comm, model_comm)
      crs % comm = context % model_smp_comm

    case(RELAY_COLOR + NODE_COLOR)                ! relay PEs on SMP node           (subset of  smp_comm, iorelay_comm)
      crs % comm = context % relay_smp_comm
    
    case(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR) ! server-bound relay PEs on SMP node (subset of relay_smp_comm)
      crs % comm = context % server_bound_relay_smp_comm

    case(RELAY_COLOR + SERVER_COLOR)              ! relay and server PEs            (subset of active_comm)
      crs % comm = context % io_comm

    case default
      crs % comm = MPI_COMM_NULL
  end select

  if(crs % comm .ne. MPI_COMM_NULL) then
    call MPI_Comm_rank(crs % comm, crs % rank)
    call MPI_Comm_size(crs % comm, crs % size)
  endif
end function IOserver_get_crs

!> Get the local shmem heap that belongs to this PE
function IOserver_get_local_heap(context) result(h)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(heap) :: h
  h = context % local_heap
end function IOserver_get_local_heap

!> Get the local shmem heap that belongs to this node (server only?)
function IOserver_get_node_heap(context) result(h)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(heap) :: h
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
  class(ioserver_context), intent(inout) :: context
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
function IOserver_get_stream(context, stream_id) result(stream)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_id !< ID of the stream we want to access
  type(local_server_stream), pointer     :: stream
  stream => context % local_server_streams(stream_id)
end function IOserver_get_stream

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

!> Get the list of local accessors to the server-bound CBs created on this node
function get_server_bound_cb_list(context) result(cbs)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer), dimension(:), pointer :: cbs
  cbs => context % server_bound_cbs
end function get_server_bound_cb_list

!> Get the list of local accessors to the heaps created on this node
function get_heap_list(context) result(heaps)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(heap), dimension(:), pointer :: heaps
  heaps => context % local_heaps
end function get_heap_list

!> Set the debug flag for this context
subroutine IOserver_set_debug(context, mode)
  implicit none
  class(ioserver_context), intent(inout) :: context
  logical, optional,       intent(in)    :: mode
  context % params % debug_mode = .true.
  if (present(mode)) context % params % debug_mode = mode
  print *, 'INFO: debug mode =', context % debug_mode()
end subroutine IOserver_set_debug

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

  if (context % debug_mode()) print *,'DEBUG: NO-OP process, global rank =', context % global_rank
  do while (.not. context % is_time_to_quit())    ! sleep loop until quit flag appears
    sleep_dummy = sleep(1)
  enddo
  if (context % debug_mode()) then
    write(6,'(A,(15I5))')' DEBUG: colors =', context % shmem % pe(0:context % max_smp_pe) % color
    write(6,*)'FINAL:, NO-OP', context % global_rank
  end if
  call MPI_Finalize()
  stop
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
  logical :: success

  if (this % is_model()) then
    ! Send a signal towards the server to indicate that this PE will no longer send anything
    call this % messenger % bump_tag()
    header % content_length_int8  = 0
    header % command              = MSG_COMMAND_MODEL_STOP
    header % message_tag          = this % messenger % get_msg_tag()
    header % sender_global_rank   = this % global_rank
    end_cap % msg_length = header % content_length_int8
    success = this % local_server_bound_cb % put(header, message_header_size_int8(), CB_KIND_INTEGER_8, .false.)
    success = this % local_server_bound_cb % put(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.) .and. success
  else
    print *, 'Should NOT be calling "finish_model"'
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
    if (this % debug_mode()) print *, 'Relay sending STOP signal', this % local_dcb % get_server_bound_client_id()
    header % content_length_int8  = 0
    header % command              = MSG_COMMAND_RELAY_STOP
    header % sender_global_rank   = this % global_rank
    header % relay_global_rank    = this % global_rank
    end_cap % msg_length = header % content_length_int8

    success = this % local_dcb % put_elems(header, message_header_size_int8(), CB_KIND_INTEGER_8, .false.)
    success = this % local_dcb % put_elems(end_cap, message_cap_size_int8(), CB_KIND_INTEGER_8, .true.) .and. success

    if (.not. success) then
      if (this % debug_mode()) print *, 'WARNING: Relay could not send a stop signal!!!'
      call print_message_header(header)
    end if
  end if
end subroutine finalize_relay

!> Finalize this context (on server PEs)
subroutine finalize_server(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(local_server_stream), pointer :: file
  logical :: success
  integer :: i

  if (associated(this % local_server_streams)) then
    ! Close all owned streams
    do i = 1, MAX_NUM_SERVER_STREAMS
      file => this % local_server_streams(i)
      if (file % is_open()) then
        if (file % is_owner()) then
          print *, 'DEBUG: Heeeeeyyyy forgot to close file #, owned by myself'
          success = file % close(.true.)
        end if
      end if
    end do
    deallocate(this % local_server_streams)
    nullify(this % local_server_streams)
  end if
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

    if (associated(this % iocolors)) deallocate(this % iocolors)
    if (associated(this % messenger)) deallocate(this % messenger)

    if (associated(this % node_relay_ranks)) deallocate(this % node_relay_ranks)
    if (associated(this % node_model_ranks)) deallocate(this % node_model_ranks)

    if (associated(this % model_bound_cbs)) deallocate(this % model_bound_cbs)
    if (associated(this % server_bound_cbs)) deallocate(this % server_bound_cbs)
    if (associated(this % local_heaps)) deallocate(this % local_heaps)

    call this % local_dcb % delete() ! This will block if not everyone calls it

    this % color = NO_COLOR
  end if
end subroutine ioserver_context_finalize_manually

subroutine ioserver_context_finalize(context)
  implicit none
  type(ioserver_context), intent(inout) :: context
  call context % finalize()
end subroutine ioserver_context_finalize

!> Build a list of model/relay indices and print them (for debugging purposes only)
subroutine build_print_model_index(context)
  implicit none
  class(ioserver_context), intent(inout) :: context

  type(C_PTR)         :: listr(0:context % max_relay_rank)
  integer(C_INTPTR_T) :: listr2(0:context % max_relay_rank)
  type(C_PTR)         :: listc(0:context % max_model_rank)
  integer(C_INTPTR_T) :: listc2(0:context % max_model_rank)
  integer :: i

  do i = 0, context % max_relay_rank
    listr(i) = context % shmem % pe(context % node_relay_ranks(i)) % arena_ptr
    listr2(i) = transfer(listr(i), listr2(i))
  enddo
  do i = 0, context % max_model_rank
    listc(i) = context % shmem % pe(context % node_model_ranks(i)) % arena_ptr
    listc2(i) = transfer(listc(i), listc2(i))
  enddo
  print *,'DEBUG: relay   =', context % node_relay_ranks(0:context % max_relay_rank)
  print 1, listr2(0:context % max_relay_rank)
  print *,'DEBUG: compute =', context % node_model_ranks(0:context % max_model_rank)
  print 1, listc2(0:context % max_model_rank)
1   format(10X, 8Z18.16)
end subroutine build_print_model_index

!> Translate node color table into relay and model rank tables
subroutine build_relay_model_index(context)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer :: i

  do i = 0, context % max_smp_pe
    if (is_color_model(context % shmem % pe(i) % color)) then
      context % node_model_ranks(context % shmem % pe(i) % rank) = i
      context % max_model_rank = max(context % max_model_rank, context % shmem % pe(i) % rank)
    endif
    if (is_color_relay(context % shmem % pe(i) % color)) then
      context % node_relay_ranks(context % shmem % pe(i) % rank) = i
      context % max_relay_rank = max(context % max_relay_rank, context % shmem % pe(i) % rank)
    endif
  enddo
end subroutine build_relay_model_index

subroutine print_shared_mem_sizes(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  if (context % is_server()) then
    print '(A,F8.1,A)', '(Server node) Shared memory heap: ', context % params % server_heap_size_mb, ' MB'
    print '(A,F8.1,A)', '(Server node) Server-bound DCB:   ', context % params % dcb_server_bound_size_mb * context % local_dcb % get_num_server_bound_clients(), ' MB (total)'
    print '(A,F8.1,A)', '(Server node) Model-bound DCB:    ', context % params % dcb_model_bound_size_mb, ' MB (per client)'
  else
    print '(A,F8.1,A)', '(Model node)  Shared memory heap: ', context % params % model_heap_size_mb, ' MB'
    print '(A,F8.1,A)', '(Model node)  Server-bound CB:    ', context % params % server_bound_cb_size_mb, ' MB'
    print '(A,F8.1,A)', '(Model node)  Model-bound CB:     ', context % params % model_bound_cb_size_mb, ' MB'
    if (context % is_relay()) then
      print '(A,F8.1,A)', '(Relay)  Server-bound DCB:        ', &
        real(context % local_dcb % get_capacity_local(CB_KIND_CHAR), kind=4) / MBYTE, ' MB'
    end if
  end if
end subroutine print_shared_mem_sizes

!> Allocate a block from the node shared memory arena
function allocate_from_arena(context, num_bytes, name, id) result(ptr)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer(C_SIZE_T),       intent(in)    :: num_bytes !< Size of the block (in bytes)
  character(len=4),        intent(in)    :: name      !< Short name for the block
  integer,                 intent(in)    :: id        !< ID of the process that created the block
  type(C_PTR) :: ptr

  character(len=1) :: prefix
  character(len=8) :: full_block_name

  ! Choose a prefix for the block, based on the role of the calling process
  if (context % is_model()) then
    prefix = 'M'
  else if (context % is_relay()) then
    prefix = 'R'
  else
    prefix = '_'
  end if

  ! Prepend prefix, append ID to the name
  write(full_block_name, '(A1, A4, I3.3)') prefix, name, id
  ptr = context % arena % newblock(num_bytes, full_block_name)

  if (C_ASSOCIATED(ptr) .and. context % debug_mode()) print *, 'DEBUG: Allocated block from arena: ', full_block_name
end function allocate_from_arena

!> Allocate a block from the shared memory arena and create a heap in it
function create_local_heap(context, num_bytes) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer(C_SIZE_T),       intent(in)    :: num_bytes !< Desired size of the heap (in bytes)
  logical :: success

  type(C_PTR)    :: heap_ptr

  success = .false.

  heap_ptr = context % allocate_from_arena(num_bytes, 'HEAP', context % shmem % pe(context % smp_rank) % rank)

  if (.not. C_ASSOCIATED(heap_ptr)) return

  success  = context % local_heap % create(heap_ptr, num_bytes)    ! allocate local heap
  context % shmem % pe(context % smp_rank) % heap_offset = Pointer_offset(context % arena % addr(), heap_ptr, 1)    ! offset of my heap in memory arena

  if (C_ASSOCIATED(heap_ptr)) success = .true.
end function create_local_heap

!> Allocate a block from the shared memory arena and create a circular buffer in it
function create_local_cb(context, num_bytes, is_output) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer(C_SIZE_T),       intent(in)    :: num_bytes !< Desired size of the circular buffer (in bytes)
  logical,                 intent(in)    :: is_output !< Whether the CB will be used for output (server-bound) or input (model-bound)
  logical :: success

  type(C_PTR)      :: cb_ptr
  character(len=4) :: name

  success = .false.

  if (is_output) then
    name = 'OUTB'
  else
    name = 'INPB'
  end if

  cb_ptr = context % allocate_from_arena(num_bytes, name, context % shmem % pe(context % smp_rank) % rank)

  if (.not. C_ASSOCIATED(cb_ptr)) return

  ! Create and compute offset in memory arena
  if (is_output) then
    success = context % local_server_bound_cb % create_bytes(cb_ptr, num_bytes)
    context % shmem % pe(context % smp_rank) % server_bound_cb_offset = Pointer_offset(context % arena % addr(), cb_ptr, 1)
  else
    success = context % local_cio_in % create_bytes(cb_ptr, num_bytes)
    context % shmem % pe(context % smp_rank) % model_bound_cb_offset = Pointer_offset(context % arena % addr(), cb_ptr, 1)
  end if

end function create_local_cb

!> Check whether CB element type is the same as JAR elements
function check_cb_jar_elem() result(success)
  use jar_module
  implicit none
  logical :: success

  success = CB_DATA_ELEMENT == JAR_ELEMENT
end function check_cb_jar_elem

!> Create all MPI communicators that will be used within this IO-server context, based on the given number of each kind of process.
!> This must be called by every process that want to participate in the IO-server
function init_communicators(context) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  logical :: success

  type(MPI_Comm) :: temp_comm
  integer :: temp_rank, temp_size
  logical :: is_mpi_initialized

  success = .false.

  ! Initialize MPI if not already done
  call MPI_Initialized(is_mpi_initialized)
  if(.not. is_mpi_initialized) call MPI_Init()

  ! Retrieve global MPI comm info
  call MPI_Comm_rank(context % global_comm, context % global_rank)
  call MPI_Comm_size(context % global_comm, context % global_size)

  ! Determine exact color for server processes, temporary color for processes on model nodes
  if (context % params % is_on_server) then
    call MPI_Comm_split(context % global_comm, 0, context % global_rank, temp_comm)
    call MPI_Comm_rank(temp_comm, temp_rank)
    call MPI_Comm_size(temp_comm, temp_size)

    block
      integer, dimension(4) :: bounds

      bounds(1) = context % params % num_server_bound_server
      bounds(2) = bounds(1) + context % params % num_model_bound_server
      bounds(3) = bounds(2) + context % params % num_grid_processors
      bounds(4) = bounds(3) + context % params % num_channels
      context % num_server_noop = temp_size - bounds(4)

      if (context % num_server_noop < 0) then
        if (temp_rank == 0) &
          print *, 'ERROR: There are not enough processes on the server to fill the requested roles: ', bounds(4)
          print *, '       Server-bound:    ', context % params % num_server_bound_server
          print *, '       Model-bound:     ', context % params % num_model_bound_server
          print *, '       Grid processors: ', context % params % num_grid_processors
          print *, '       Channels:        ', context % params % num_channels
        return
      end if


      if (temp_rank < bounds(1)) then
        context % color = SERVER_COLOR + SERVER_BOUND_COLOR
      else if (temp_rank < bounds(2)) then
        context % color = SERVER_COLOR + MODEL_BOUND_COLOR
      else if (temp_rank < bounds(3)) then
        context % color = SERVER_COLOR + GRID_PROCESSOR_COLOR
      else if (temp_rank < bounds(4)) then
        context % color = SERVER_COLOR + CHANNEL_COLOR
      else
        context % color = NO_OP_COLOR
      end if
    end block

  else
    call MPI_Comm_split(context % global_comm, 1, context % global_rank, temp_comm)
    context % color = MODEL_COLOR + RELAY_COLOR
  end if

  ! Split all PEs by node (mostly used for model nodes)
  call MPI_Comm_split_type(context % global_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, context % smp_comm)
  call MPI_Comm_rank(context % smp_comm, context % smp_rank)    ! rank on SMP node
  call MPI_Comm_size(context % smp_comm, context % smp_size)    ! population of SMP node

  ! Assign a node ID to each SMP node (and 0 to the server node)
  block
    integer :: key
    if (context % smp_rank == 0) then
      if (context % params % is_on_server) then
        key = 0
      else
        key = 1000000
      end if
      call MPI_Comm_split(context % global_comm, 0, context % global_rank + key, temp_comm)
      call MPI_Comm_rank(temp_comm, context % node_id)
      call MPI_Comm_size(temp_comm, context % num_nodes)
    else
      call MPI_Comm_split(context % global_comm, 1, context % global_rank, temp_comm)
    end if

    call MPI_Bcast(context % node_id, 1, MPI_INTEGER, 0, context % smp_comm)
  end block

  ! Split global communicator into : (server+model+relay) and no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node (a deadlock will happen if this condition is not respected)
  ! Arrange it so that the overall ranks are grouped by node and that server processes have the lowest ranks
  if (context % color .ne. NO_OP_COLOR) then
    call MPI_Comm_split(context % global_comm, 0, context % node_id * 100000 + mod(context % global_rank, 100000), context % active_comm)
    call MPI_Comm_rank(context % active_comm, context % active_rank)
    call MPI_Comm_size(context % active_comm, context % active_size)
  else
    call MPI_Comm_split(context % global_comm, 1, 0, temp_comm)
  end if

  ! no-op processes don't need to go any further
  if (context % color == NO_OP_COLOR) then
    success = .true.
    return
  end if

  ! Split the all useful communicator (active_comm) into : server and (model+relay)
  if (context % is_server()) then
    call MPI_Comm_split(context % active_comm, 0, context % active_rank, context % server_comm)
  else
    call MPI_Comm_split(context % active_comm, 1, context % active_rank, context % model_relay_comm)
  end if

  if (context % is_server()) then
    !-------------------
    ! Server processes
    !-------------------
    block ! Verify that all server processes are indeed on the same node
      type(MPI_Comm) :: server_comm_tmp
      integer :: server_comm_size_tmp, server_comm_size

      call MPI_Comm_split_type(context % server_comm, MPI_COMM_TYPE_SHARED, context % active_rank, MPI_INFO_NULL, server_comm_tmp)
      call MPI_Comm_rank(server_comm_tmp, context % server_comm_rank)     ! rank on SMP node
      call MPI_Comm_size(server_comm_tmp, server_comm_size_tmp)           ! population of SMP node
      call MPI_Comm_size(context % server_comm, server_comm_size)         ! population of SMP node

      if(server_comm_size_tmp .ne. server_comm_size) then
        ! for now, error if server_comm_size is not equal to server_size
        print *, 'ERROR: Processes declared as "server" are not all located on the same node'
        context % color = NO_COLOR
        return
      endif
    end block

    ! Split between active (server- and model-bound) and channel processes (communication channels, or 'ghost' processes)
    if (context % is_channel()) then
      call MPI_Comm_split(context % server_comm, CHANNEL_COLOR, 0, temp_comm)
    else
      call MPI_Comm_split(context % server_comm, SERVER_COLOR, context % server_comm_rank, context % server_work_comm)

      ! Split between server-bound and model-bound server processes
      if (context % is_server_bound()) then
        call MPI_Comm_split(context % server_work_comm, SERVER_BOUND_COLOR, context % active_rank, context % server_bound_server_comm)
      else if (context % is_model_bound()) then
        call MPI_Comm_split(context % server_work_comm, MODEL_BOUND_COLOR, context % active_rank, context % model_bound_server_comm)
      else if (context % is_grid_processor()) then
        call MPI_Comm_split(context % server_work_comm, GRID_PROCESSOR_COLOR, context % active_rank, temp_comm)
      else
        print *, 'ERROR: Unhandled server process type'
        return
      end if
    end if

  else
    !----------------------------
    ! Model and relay processes
    !----------------------------

    ! Split model_relay_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(context % model_relay_comm, MPI_COMM_TYPE_SHARED, context % active_rank, MPI_INFO_NULL, context % model_relay_smp_comm)
    call MPI_Comm_rank(context % model_relay_smp_comm, context % model_relay_smp_rank)     ! rank on SMP node
    call MPI_Comm_size(context % model_relay_smp_comm, context % model_relay_smp_size) ! population of SMP node

    ! Split between model and relay processes
    ! Spread relay PEs across the node (lowest and highest node ranks)
    if (context % model_relay_smp_rank >= ((context % params % num_relay_per_node+1)/2) .and. &
        context % model_relay_smp_rank < (context % model_relay_smp_size - ((context % params % num_relay_per_node)/2))) then   ! model compute process
      ! Model process
      context % color = MODEL_COLOR

      call MPI_Comm_split(context % model_relay_smp_comm, MODEL_COLOR, context % model_relay_smp_rank, context % model_smp_comm) ! Model processes on this node
      call MPI_Comm_split(context % model_relay_comm, MODEL_COLOR, context % active_rank, context % model_comm)                  ! Model processes on all nodes
    else
      ! Relay process
      context % color = RELAY_COLOR

      call MPI_Comm_split(context % model_relay_smp_comm, RELAY_COLOR, context % model_relay_smp_rank, context % relay_smp_comm) ! Relay processes on this node
      call MPI_Comm_split(context % model_relay_comm, RELAY_COLOR, context % active_rank, context % relay_comm)                  ! Relay processes on all nodes

      ! Compute number of model processes on this node
      block
        integer :: node_size, relay_size
        call MPI_Comm_size(context % model_relay_smp_comm, node_size)
        call MPI_Comm_size(context % relay_smp_comm, relay_size)
        context % num_local_model_proc = node_size - relay_size
        if (context % num_local_model_proc < relay_size) then
          print '(A, I2, A, I3, A)', 'WARNING: There are more relay processes (', relay_size,                     &
              ') than model processes (', context % num_local_model_proc, '). Is this really what you want?'
        end if
      end block

      ! Determine and split server-bound vs model-bound relays (only use a server-bound communicator for now)
      call MPI_Comm_rank(context % relay_smp_comm, temp_rank)
      if (mod(temp_rank, 2) == 0) then
        context % color = RELAY_COLOR + SERVER_BOUND_COLOR
        call MPI_Comm_split(context % relay_smp_comm, SERVER_BOUND_COLOR, temp_rank, context % server_bound_relay_smp_comm)
      else
        context % color = RELAY_COLOR + MODEL_BOUND_COLOR
        call MPI_Comm_split(context % relay_smp_comm, MODEL_BOUND_COLOR, temp_rank, temp_comm)
      end if
    endif
  end if

  ! Split active_comm into model and IO (relay+server) processes
  if (context % is_relay() .or. context % is_server()) then  
    call MPI_Comm_split(context % active_comm, RELAY_COLOR + SERVER_COLOR, context % active_rank, context % io_comm) 
  else
    call MPI_Comm_split(context % active_comm, MODEL_COLOR, context % active_rank, temp_comm)
  endif

  if (context % debug_mode() .and. (context % is_relay() .or. context % is_server())) then
    block
      integer :: relay_smp_rank, io_rank
      integer :: num_models, num_server_bound, num_model_bound

      context % num_model_nodes = max(context % num_nodes - 1, 1)
      context % num_model_pes = 0
      num_models = 0
      num_server_bound = 0
      num_model_bound = 0

      if (context % is_relay()) then
        call MPI_Comm_rank(context % relay_smp_comm, relay_smp_rank)
        if (relay_smp_rank == 0) then
          num_models = context % num_local_model_proc
        end if
        if (context % is_server_bound()) then
          num_server_bound = 1
        else if (context % is_model_bound()) then
          num_model_bound = 1
        else
          print *, 'ERROR: Relay is neither server- nor model-bound'
          return
        end if
      end if

      call MPI_Allreduce(num_models, context % num_model_pes, 1, MPI_INTEGER, MPI_SUM, context % io_comm)
      call MPI_Allreduce(num_server_bound, context % num_server_bound_relays, 1, MPI_INTEGER, MPI_SUM, context % io_comm)
      call MPI_Allreduce(num_model_bound, context % num_model_bound_relays, 1, MPI_INTEGER, MPI_SUM, context % io_comm)

      call MPI_Comm_rank(context % io_comm, io_rank)
      if (io_rank == 0) then
        print '(A, I5, A, I5, A)', 'DEBUG: We have ', context % num_nodes, ' nodes (', context % num_model_nodes, ' model nodes)'
        print '(A, I5, A)',        '               ', context % num_server_bound_relays, ' server-bound relays'
        print '(A, I5, A)',        '               ', context % num_model_bound_relays, ' model-bound relays'
        print '(A, I5, A)',        '               ', context % num_model_pes, ' model processes'
        print '(A, I5, A)',        '               ', context % params % num_server_bound_server, ' server-bound server processes'
        print '(A, I5, A)',        '               ', context % params % num_model_bound_server, ' model-bound server processes'
        print '(A, I5, A)',        '               ', context % params % num_grid_processors, ' grid processors'
        print '(A, I5, A)',        '               ', context % params % num_channels, ' channel processes'
        print '(A, I5, A)',        '               ', context % num_server_noop, ' NO-OP server processes'
      end if
    end block
  end if

  success = .true.

end function init_communicators

!> Allocate shared memory on the node and create all the structs in it that will be used by the IO server.
!> Must be called by all processes involved in the IO server
function init_shared_mem(context) result(success)
  use shared_mem_alloc_module
  use simple_mutex_module
  implicit none

  class(ioserver_context), intent(inout) :: context
  logical :: success

  integer            :: num_errors
  integer(C_INT64_T) :: shmem_num_bytes

  integer     :: total_errors
  integer     :: i_stream
  type(C_PTR) :: temp_ptr
  type(shared_server_stream)  :: dummy_server_stream
  logical :: alloc_success, heap_create_success

  success = .false.
  num_errors = 0              ! none so far

  ! Allocate shared memory segment used for control, communicator = smp_comm
  context % ctrl_shmem_size = storage_size(context % shmem) / 8
  context % ctrl_shmem_c    = RPN_allocate_shared(context % ctrl_shmem_size, context % smp_comm)

  call C_F_POINTER(context % ctrl_shmem_c, context % shmem)             ! main control structure points to shared memory at ctrl_shmem_c
  context % max_smp_pe = context % smp_size - 1

  ! First PE on each node initializes control segment to 0
  if (context % smp_rank == 0) then
    context % shmem % pe(0:context % max_smp_pe) = pe_info(C_NULL_PTR, 0, 0, 0, 0, 0, [0, 0, 0, 0])
    context % shmem % time_to_quit = 0   ! initialize quit flag to "DO NOT QUIT"
  end if

  ! Wait until control area initialization is done everywhere
  call MPI_barrier(context % global_comm)

  if (context % color == NO_OP_COLOR) then       ! this is a NO-OP process, enter wait loop for finalize
    context % shmem % pe(context % smp_rank) % color = NO_OP_COLOR
    call context % no_op()              ! this subroutine will never return and call finalize
    call MPI_Finalize()                 ! no_op should never return, but ... in case it does
    stop
  endif

  ! ===================================================================================
  ! at this point we only have "active" PEs (model, relay, server)
  ! allocate node local shared memory used for intra node communications
  ! ===================================================================================

  ! Allocate local tables. Size is overkill, but makes it easier
  allocate(context % node_relay_ranks(0:context % max_smp_pe))
  allocate(context % node_model_ranks(0:context % max_smp_pe))
  context % node_relay_ranks(:) = -1
  context % node_model_ranks(:) = -1

  allocate(context % model_bound_cbs (0:context % max_smp_pe))
  allocate(context % server_bound_cbs(0:context % max_smp_pe))
  allocate(context % local_heaps     (0:context % max_smp_pe))

  ! Determine how many server-bound server processes can actually open streams (it has to be <= the number of relays)
  block
    integer :: val
    val = 0
    if (context % is_server() .or. context % is_relay()) then
      context % num_server_stream_owners = 0
      if (context % is_relay() .and. context % is_server_bound()) val = 1
      call MPI_Allreduce(val, context % num_server_stream_owners, 1, MPI_INTEGER, MPI_SUM, context % io_comm)
      context % num_server_stream_owners = min(context % num_server_stream_owners, context % params % num_server_bound_server)
    end if
  end block

  if (context % is_server()) then
    ! =========================================================================
    ! ============================ IO server process ==========================
    ! =========================================================================

    ! Allocate shared memory used for intra node communication between PEs on a server node
    context % server_heap_shmem_size = int(context % params % server_heap_size_mb * MBYTE, kind=8)
    context % server_heap_shmem = RPN_allocate_shared(context % server_heap_shmem_size, context % server_comm) ! The heap

    ! Allocate shared memory to hold server stream instances
    context % server_file_shmem_size = MAX_NUM_SERVER_STREAMS * (storage_size(dummy_server_stream) / 8)
    context % server_file_shmem = RPN_allocate_shared(context % server_file_shmem_size, context % server_comm) ! The set of files that can be opened/written

    if (.not. c_associated(context % server_heap_shmem) .or. .not. c_associated(context % server_file_shmem)) then
      print *, 'ERROR: We have a problem! server_heap_shmem/server_file_shmem are not valid!'
      goto 2
    end if

    call c_f_pointer(context % server_file_shmem, context % common_server_streams, [MAX_NUM_SERVER_STREAMS])

    ! Create shared data structures
    if (context % server_comm_rank == 0) then
      heap_create_success = context % node_heap % create(context % server_heap_shmem, context % server_heap_shmem_size)    ! Initialize heap
      do i_stream = 1, MAX_NUM_SERVER_STREAMS
        context % common_server_streams(i_stream) = shared_server_stream(i_stream, mod(i_stream-1, context % num_server_stream_owners))  ! Initialize stream files
      end do

    else
      ! Create local accessors to shared structures
      heap_create_success = context % node_heap % clone(context % server_heap_shmem)
    end if

    ! Create local stream instances on server-bound PEs
    if (context % is_server_bound()) then
      block
        integer :: rank
        type(shared_server_stream), pointer :: tmp_ptr
        call MPI_Comm_rank(context % server_bound_server_comm, rank)
        allocate(context % local_server_streams(MAX_NUM_SERVER_STREAMS))
        do i_stream = 1, MAX_NUM_SERVER_STREAMS
          tmp_ptr => context % common_server_streams(i_stream)
          call context % local_server_streams(i_stream) % init(rank, tmp_ptr, context % node_heap)
        end do
      end block
    end if

  else
    ! =========================================================================
    ! ======================= model compute or IO relay process ===============
    ! =========================================================================

    ! Allocate shared memory used for intra node communication between model and relay PEs
    block
      integer(C_INT64_T) :: shmem_size
      real :: shmem_size_r

      ! Compute size
      shmem_size_r = context % params % model_heap_size_mb + context % params % server_bound_cb_size_mb + context % params % model_bound_cb_size_mb
      shmem_size = int(shmem_size_r * MBYTE, kind=8)
      call MPI_Allreduce(shmem_size, context % model_shmem_size, 1, MPI_INTEGER8, MPI_SUM, context % model_relay_smp_comm)

      ! Allocate
      context % model_shmem = RPN_allocate_shared(context % model_shmem_size, context % model_relay_smp_comm)

      if (context % debug_mode()) then
        write(6,'(A, I10, A, I4)') 'DEBUG: after MPI_Win_allocate_shared, size = ', context % model_shmem_size, ', rank = ', context % model_relay_smp_rank
        call flush(6)
      end if

      if (.not. c_associated(context % model_shmem)) then
        print '(A, I9, A)', 'ERROR: Unable to allocated shared memory on model node (', context % model_shmem_size , ' MB)'
        goto 2
      end if
    end block

    if (context % model_relay_smp_rank == 0) then
      ! Create the memory arena
      shmem_num_bytes = context % model_shmem_size
      temp_ptr = context % arena % create(context % model_shmem, 256, shmem_num_bytes)
    else
      ! Create local accessor to shared memory arena (cloning is O.K. even if arena is not initialized)
      temp_ptr = context % arena % clone(context % model_shmem)
    endif

    ! Set value in this process' section of the control area
    context % local_arena_ptr = context % arena % addr()
    context % shmem % pe(context % smp_rank) % arena_ptr = context % local_arena_ptr  ! local address of arena segment
    context % shmem % pe(context % smp_rank) % color = context % color                ! store color of this PE in shared memory table

    ! Get rank on node in my color (model or relay)
    if (context % is_relay()) then
      call MPI_Comm_rank(context % relay_smp_comm, context % shmem % pe(context % smp_rank) % rank)
    else
      call MPI_Comm_rank(context % model_smp_comm, context % shmem % pe(context % smp_rank) % rank)
    end if

    if (context % debug_mode()) print *, 'DEBUG: rank', context % shmem % pe(context % smp_rank) % rank, ' in color', context % color

    ! Wait for memory arena to be initialized by rank 0 before allocating heap and circular buffer(s)
    call MPI_Barrier(context % model_relay_smp_comm)

    ! Create PE's own heap, model-bound CB and server-bound CB in the shared memory arena
    shmem_num_bytes = context % model_shmem_size / context % model_relay_smp_size        ! available size (in bytes) per PE on SMP node
    alloc_success = context % create_local_heap(int(shmem_num_bytes * 0.85_8, kind=C_SIZE_T))
    alloc_success = context % create_local_cb(int(shmem_num_bytes * 0.04_8, kind=C_SIZE_T), .false.) .and. alloc_success
    alloc_success = context % create_local_cb(int(shmem_num_bytes * 0.1_8, kind=C_SIZE_T), .true.) .and. alloc_success
    if (.not. alloc_success) goto 2

    if (context % debug_mode()) then
      write(6,'(A,3Z18.16)') ' DEBUG: displacements =', &
        context % shmem % pe(context % smp_rank) % heap_offset, &
        context % shmem % pe(context % smp_rank) % model_bound_cb_offset, &
        context % shmem % pe(context % smp_rank) % server_bound_cb_offset
    end if
    call flush(6)
  endif   ! (server process)

  goto 3       ! no error, bypass
2 continue     ! go here upon error in memory allocation
  num_errors = num_errors + 1
3 continue     ! no error

  !  GLOBAL ERROR CHECK  (and synchronization point)
  call MPI_Allreduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, context % active_comm)
  if(total_errors > 0) then
    print *,'FATAL:',total_errors,' error(s) detected while allocating memory'
    call context % set_time_to_quit()      ! tell NO-OP PEs to quit
    call MPI_Finalize()
    stop
  endif

  if (context % is_relay() .or. context % is_model()) then
    call context % build_relay_model_index()
    call context % fetch_node_shmem_structs()
  endif

  ! IO Relay or Server
  if (context % is_relay() .or. context % is_server()) then              ! IO relay or IO server
    block
      integer :: allio_comm_size
      call MPI_Comm_size(context % io_comm, allio_comm_size)
      allocate(context % iocolors(0:allio_comm_size))             ! collect colors for IO PEs, index in array is rank in io_comm
      call MPI_Allgather(context % color, 1, MPI_INTEGER, context % iocolors, 1, MPI_INTEGER, context % io_comm)
      if (context % debug_mode()) write(6,'(A,10I8,(/19X,10I8))') ' DEBUG: IO colors =', context % iocolors(0:allio_comm_size-1)
    end block

    ! Create DCB
    block 
      integer(C_INT64_T) :: server_bound_size, model_bound_size
      integer :: type
      type(MPI_Comm) :: server_comm

      server_comm       = MPI_COMM_NULL
      server_bound_size = 0
      model_bound_size  = 0

      type = DCB_SERVER_BOUND_TYPE
      if (.not. context % is_server_bound()) type = DCB_CLIENT_BOUND_TYPE
      if (context % is_channel()) type = DCB_CHANNEL_TYPE

      if (context % is_server()) then
        server_bound_size = int(context % params % dcb_server_bound_size_mb * MBYTE, kind=8)
        model_bound_size  = int(context % params % dcb_model_bound_size_mb * MBYTE, kind=8)
        server_comm = context % server_comm
      end if

      success = context % local_dcb % create_bytes(context % io_comm, server_comm, type, server_bound_size, model_bound_size)
      if (.not. success) num_errors = num_errors + 1
    end block
  endif

  if (context % debug_mode()) then
    call context % print_shared_mem_sizes()
  end if

  if (context % debug_mode() .and. (context % is_relay() .or. context % is_model())) then
    call context % build_print_model_index()
  end if

  allocate(context % messenger)
  call context % messenger % set_debug(context % debug_mode())
  call context % messenger % set_model_crs(context % get_crs(MODEL_COLOR))

  success = .true.
end function init_shared_mem

!> Initialize the IO server context. *Must be called by every process that wants to participate in the IO server*
!> \return .true. if the initialization was successful, .false. otherwise
!> \sa ioserver_context::IOserver_init_communicators, ioserver_context::IOserver_init_shared_mem
function ioserver_context_init(context, params) result(success)
  implicit none
  class(ioserver_context),         intent(inout) :: context !< The context we are initialising ("this")
  type(ioserver_input_parameters), intent(in)    :: params  !< All details about how it should be initialized
  logical :: success

  success = .false.

  if (.not. check_cb_jar_elem()) then
    print *, 'ERROR: CB elements are not the same as JAR elements. That is a problem.'
    return
  end if

  context % params = params

  success = context % init_communicators()

  if (.not. success) then
    print *, 'ERROR: Could not properly initialize all communicators!'
    return
  end if

  success = context % init_shared_mem()

  if (.not. success) then
    print *, 'ERROR: There were errors during shared memory initialization'
    return
  end if

end function ioserver_context_init

end module ioserver_context_module
