!  functions for C and FORTRAN programming
!  Copyright (C) 2021  Recherche en Prevision Numerique
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021
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

  integer(C_SIZE_T), parameter :: DCB_SERVER_BOUND_SIZE = 4 * MBYTE !< Size of each server-bound CB within the DCB
  integer(C_SIZE_T), parameter :: DCB_MODEL_BOUND_SIZE  = 2 * MBYTE !< Size of each model-bound CB within the DCB

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

  !> Context that allows to interact with the IO-server library (initialization + the rest of the API)
  type, public :: ioserver_context
    private

    integer :: color = NO_COLOR !< Color of this PE. Describes its role (server vs relay vs model, server-bound vs relay-bound, etc.)

    ! -----------------------------------------------------
    ! Shared memory areas (raw C pointers and area sizes)

    ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
    type(C_PTR)       :: ctrl_shmem_c      = C_NULL_PTR       !< Address of main (control) shared memory area on this node
    integer(C_SIZE_T) :: ctrl_shmem_size = 0 !< Size of control shared memory

    ! information for model compute and IO relay PEs on a given SMP node
    ! shared memory used for heaps and circular buffers on a given SMP node
    ! (one heap and 2 circular buffers per compute PE)
    ! this memory is used for communications between IO relay and model compute PEs
    type(C_PTR)       :: model_shmem          = C_NULL_PTR    !< Address of relay+model PEs shared memory area
    integer(C_SIZE_T) :: model_shmem_size     = GBYTE         !< Size of relay/model shared memory area

    type(C_PTR)       :: server_heap_shmem      = C_NULL_PTR  !< Address of io server PEs shared memory area
    integer(C_SIZE_T) :: server_heap_shmem_size = 10 * GBYTE  !< Size of server shared memory area (we want a lot on the server for grid assembly)

    type(C_PTR)       :: server_file_shmem      = C_NULL_PTR  !< Address of server shared memory area for holding open file data
    integer(C_SIZE_T) :: server_file_shmem_size = 0           !< Size of server shared memory area for holding open file data (computed at initialization)

    type(C_PTR) :: local_arena_ptr                            !< Pointer to start of shared memory arena
    
    ! -----------------
    ! Communicators
    type(MPI_Comm) :: global_comm   = MPI_COMM_WORLD        !< MPI "WORLD" for this set of PEs
    integer        :: global_rank   = -1                    !< rank in global_comm
    integer        :: global_size   =  0                    !< population of global_comm

    type(MPI_Comm) :: smp_comm      = MPI_COMM_NULL         !< PEs on this SMP node        (any kind ) (subset of global_comm)
    integer        :: smp_rank      = -1                    !< rank in smp_comm
    integer        :: smp_size      =  0                    !< population of smp_comm

    type(MPI_Comm) :: all_comm                    = MPI_COMM_NULL  !< non NO-OP PEs       (all nodes) (subset of global_comm)
    type(MPI_Comm) :: allio_comm                  = MPI_COMM_NULL  !< all IO PEs     (relay + server) (subset of all_comm)
    type(MPI_Comm) :: model_relay_comm            = MPI_COMM_NULL  !< model and relay PEs (all nodes) (subset of all_comm)
    type(MPI_Comm) :: model_comm                  = MPI_COMM_NULL  !< model PEs           (all nodes) (subset of model_relay_comm)
    type(MPI_Comm) :: relay_comm                  = MPI_COMM_NULL  !< relay PEs           (all nodes) (subset of model_relay_comm)

    type(MPI_Comm) :: server_comm                 = MPI_COMM_NULL  !< IO server PEs               (subset of all_comm)
    type(MPI_Comm) :: server_active_comm          = MPI_COMM_NULL  !< Server PEs that process relay data (subset of server_comm)
    type(MPI_Comm) :: server_bound_server_comm    = MPI_COMM_NULL  !< Server PEs that process server-bound relay data (subset of server_active_comm)
    type(MPI_Comm) :: model_bound_server_comm     = MPI_COMM_NULL  !< Server PEs that process model-bound relay data (subset of server_active_comm)

    type(MPI_Comm) :: model_relay_smp_comm        = MPI_COMM_NULL  !< model+relays on current node
    type(MPI_Comm) :: model_smp_comm              = MPI_COMM_NULL  !< model PEs on current SMP node
    type(MPI_Comm) :: relay_smp_comm              = MPI_COMM_NULL  !< relay PEs on current SMP node
    type(MPI_Comm) :: server_bound_relay_smp_comm = MPI_COMM_NULL  !< server-bound relay PEs on current SMP node, subset of relay_smp_comm

    integer :: model_relay_smp_rank = -1    !< rank in model_relay_smp_comm
    integer :: model_relay_smp_size = 0     !< population of model_relay_smp_comm
    integer :: server_comm_rank     = -1    !< rank in server_comm
    integer :: num_local_model_proc = -1    !< Number of model processes on this SMP node

    !----------------------------------------------------
    ! Local instances of objects located in shared memory
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

    !---------------------------------------------------
    ! Direct (Fortran) pointers to actual shared memory
    type(control_shared_memory),              pointer :: shmem                 => NULL() !< Will point to start of control shared memory
    type(shared_server_stream), dimension(:), pointer :: common_server_streams => NULL() !< Stream files used to assemble grids and write them (in shared memory)

    ! Stuff
    integer :: max_smp_pe     =  0 !< Highest ID/rank of PEs on this node
    integer :: max_relay_rank = -1 !< Highest rank of relay PEs on this node
    integer :: max_model_rank = -1 !< Highest rank of model PEs on this node
    integer, dimension(:), pointer :: node_relay_ranks  => NULL() !< ranks of relay PEs on this node
    integer, dimension(:), pointer :: node_model_ranks  => NULL() !< ranks of model PEs on this node

    integer :: num_server_bound_server  = -1 !< How many server-bound server processes there are
    integer :: num_server_stream_owners = -1 !< How many server processes can own a stream (can be lower than number of server-bound processes)

    ! ------------------
    ! Miscellaneous
    logical :: debug_mode = .false. !< Whether we are in debug mode (activates additional prints/checks when true)

    type(C_FUNPTR) :: io_relay_fn  = C_NULL_FUNPTR !< Procedure to call on relay processes (if not NULL)
    type(C_FUNPTR) :: io_server_fn = C_NULL_FUNPTR !< Procedure to call on server processes (if not NULL)

    integer, dimension(:), pointer :: iocolors => NULL()     !< Color table for io server and relay processes
    type(ioserver_messenger), pointer :: messenger => NULL() !< Will be shared among open model files

  contains
    private

    ! Initialization
    procedure, pass, public :: init => IOserver_init
    procedure, pass         :: init_communicators => IOserver_init_communicators
    procedure, pass         :: init_shared_mem => IOserver_init_shared_mem
    procedure, pass         :: build_relay_model_index
    procedure, pass         :: build_print_model_index
    procedure, pass         :: fetch_node_shmem_structs
    procedure, pass         :: is_initialized
    procedure, pass         :: allocate_from_arena
    procedure, pass         :: create_local_heap
    procedure, pass         :: create_local_cb

    ! Process type query
    procedure, pass, public :: is_relay  => IOserver_is_relay
    procedure, pass, public :: is_server => IOserver_is_server
    procedure, pass, public :: is_model  => IOserver_is_model
    procedure, pass, public :: is_server_bound => IOserver_is_server_bound
    procedure, pass, public :: is_model_bound  => IOserver_is_model_bound
    procedure, pass, public :: is_channel => IOserver_is_channel

    procedure, pass, public :: has_debug_mode

    ! Finalization
    procedure, pass         :: set_time_to_quit => IOserver_set_time_to_quit
    procedure, pass         :: is_time_to_quit  => IOserver_is_time_to_quit
    procedure, pass         :: finalize_model
    procedure, pass         :: finalize_relay
    procedure, pass         :: finalize_server
    procedure, pass, public :: finalize => ioserver_context_finalize_manually
    final                   :: IOserver_finalize

    ! Getters
    procedure, pass, public :: get_num_local_model

    procedure, pass, public :: get_global_rank
    procedure, pass, public :: get_crs => IOserver_get_crs
    procedure, pass, public :: get_local_heap => IOserver_get_local_heap
    procedure, pass, public :: get_node_heap => IOserver_get_node_heap
    procedure, pass, public :: get_server_bound_cb => IOserver_get_server_bound_cb
    procedure, pass, public :: get_model_bound_cb => IOserver_get_model_bound_cb
    procedure, pass, public :: get_dcb => IOserver_get_dcb
    procedure, pass, public :: get_relay_shmem => IOserver_get_relay_shmem
    procedure, pass, public :: get_messenger => IOserver_get_messenger
    procedure, pass, public :: get_local_arena_ptr
    procedure, pass, public :: get_stream => IOserver_get_stream

    procedure, pass, public :: get_server_bound_cb_list
    procedure, pass, public :: get_heap_list

    ! Process function management
    procedure, pass         :: no_op => IOserver_noop
    procedure, pass, public :: set_relay_fn  => IOserver_set_relay
    procedure, pass, public :: set_server_fn => IOserver_set_server
    procedure, pass, public :: set_debug => IOserver_set_debug

    ! File management
    procedure, pass, public :: open_stream_model
    procedure, pass, public :: open_stream_server
    procedure, pass, public :: close_stream_server
    
    ! Pointer translation between processes on the same node
    procedure, pass, public :: ptr_translate_to
    procedure, pass, public :: ptr_translate_from

    ! Debugging
    procedure, pass :: print_io_colors
    procedure, pass :: verify_translations
  end type ioserver_context

contains

!> Whether this context belongs to a relay process (server- or model-bound)
function IOserver_is_relay(context) result(is_relay)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_relay
  is_relay = is_color_relay(context % color)
end function IOserver_is_relay

!> Whether this context belongs to a server process
function IOserver_is_server(context) result(is_server)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_server
  is_server = is_color_server(context % color)
end function IOserver_is_server

!> Whether this context belongs to a model process
function IOserver_is_model(context) result(is_model)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_model
  is_model = is_color_model(context % color)
end function IOserver_is_model

!> Whether this context belongs to a process that handles communications going towards the server
function IOserver_is_server_bound(context) result(is_server_bound)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_server_bound
  is_server_bound = iand(context % color, SERVER_BOUND_COLOR) == SERVER_BOUND_COLOR
end function IOserver_is_server_bound

!> Whether this context belongs to a process that handles communications going towards the model
function IOserver_is_model_bound(context) result(is_model_bound)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_model_bound
  is_model_bound = iand(context % color, MODEL_BOUND_COLOR) == MODEL_BOUND_COLOR
end function IOserver_is_model_bound

!> Whether this context to a process that serves as a MPI communication channel (on server only)
function IOserver_is_channel(context) result(is_channel)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_channel
  is_channel = iand(context % color, CHANNEL_COLOR) == CHANNEL_COLOR
end function IOserver_is_channel

!> Whether debug mode is enabled for this context
function has_debug_mode(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: has_debug_mode
  has_debug_mode = context % debug_mode
end function has_debug_mode

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

  if (context % debug_mode) print *, 'DEBUG: (Model) Opening file with name ', filename

  new_file = model_stream(context % global_rank, &
                          context % local_heap, &
                          context % local_server_bound_cb, &
                          context % debug_mode, &
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


!> Translate an address from *another* PE address space to the *local* address space (in shared memory arena)
!> get my address in shared memory arena from a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
!> \return The local C_PTR that corresponds to the given C_PTR
function ptr_translate_from(context, from_ptr, from_color, from_rank) result(local)
  implicit none
  class(ioserver_context), intent(in)        :: context
  type(C_PTR),             intent(IN), value :: from_ptr    !< Address on other PE, that we want to translate
  integer(C_INT),          intent(IN), value :: from_color  !< Color of the other PE (RELAY_COLOR, MODEL_COLOR)
  integer(C_INT),          intent(IN), value :: from_rank   !< Rank of the other PE
  type(C_PTR) :: local

  integer(C_INTPTR_T) :: offset, new
  type(C_PTR)         :: my_base, new_base

  local = C_NULL_PTR
  my_base = context % shmem % pe(context % smp_rank) % arena_ptr   ! local address of memory arena

  new_base = C_NULL_PTR                  ! find new base
  if (from_color == NODE_COLOR) then     ! translate from address of PE of rank from_rank in SMP node
    new_base = context % shmem % pe(from_rank) % arena_ptr
  else if (from_color == MODEL_COLOR) then    ! translate from address of compute PE of rank from_rank
    new_base = context % shmem % pe(context % node_model_ranks(from_rank)) % arena_ptr
  else if (from_color == RELAY_COLOR) then    ! translate from address of relay PE of rank from_rank
    new_base = context % shmem % pe(context % node_relay_ranks(from_rank)) % arena_ptr
  else
    print *,'ERROR(ptr_translate_from): invalid from_color',from_color
    return   ! invalid from_color
  endif

  offset = Pointer_offset(new_base, from_ptr, 1)      ! offset in other PE space
  if (offset < 0) print *,'ERROR(ptr_translate_from): negative offset'
  if (offset < 0) return                 ! not in shared memory arena

  new   = transfer(my_base, new)         ! make large integer from C pointer
  new   = new + offset                   ! add offset to my base
  local = transfer(new, local)           ! honest C pointer
  ! print *,'DEBUG(ptr_translate_from): from, from_color, from_rank, local', &
  !        transfer(from_ptr,1_8), from_color, from_rank, transfer(local,1_8)
end function ptr_translate_from

!> Translate the given address from *local* address space to the address space of *another* PE (in shared memory arena)
!> translate my address in shared memory arena into a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
!> \return The C_PTR on *other PE* that corresponds to the given C_PTR
function ptr_translate_to(context, from, to_color, to_rank) result(to)
  implicit none
  class(ioserver_context), intent(in) :: context
  type(C_PTR),    intent(IN), value :: from     !< Local pointer
  integer(C_INT), intent(IN), value :: to_color !< Color of the target PE (RELAY_COLOR, MODEL_COLOR)
  integer(C_INT), intent(IN), value :: to_rank  !< Rank of the target PE
  type(C_PTR) :: to

  integer(C_INTPTR_T) :: offset, new
  type(C_PTR)         :: my_base, new_base

  to = C_NULL_PTR
  my_base = context % shmem % pe(context % smp_rank) % arena_ptr   ! local address of memory arena
  offset = Pointer_offset(my_base, from, 1)       ! offset in local space
  if(offset < 0) print *,'ERROR(ptr_translate_to): negative offset'
  if(offset < 0) return                  ! not in shared memory arena

  new_base = C_NULL_PTR                  ! find new base
  if (to_color == NODE_COLOR) then       ! translate to address of PE of rank to_rank in SMP node
    new_base = context % shmem % pe(to_rank) % arena_ptr
  else if (to_color == MODEL_COLOR) then      ! translate to address of compute PE of rank to_rank
    new_base = context % shmem % pe(context % node_model_ranks(to_rank)) % arena_ptr
  else if (to_color == RELAY_COLOR) then      ! translate to address of relay PE of rank to_rank
    new_base = context % shmem % pe(context % node_relay_ranks(to_rank)) % arena_ptr
  else
    print *,'ERROR(ptr_translate_to): invalid to_color', to_color
    return   ! invalid to_color
  endif

  new = transfer(new_base, new)          ! make large integer from C pointer
  new = new + offset                     ! add offset to new base
  to  = transfer(new, to)                ! honest C pointer
! print *,'DEBUG(ptr_translate_to): from, to_color, to_rank, to', &
!         transfer(from,1_8), to_color, to_rank, transfer(to,1_8)
end function ptr_translate_to

!> Fetch the shared memory structs created by other model PEs on this node and initialize the
!> corresponding access to them on this PE. There is a set of heap, model-bound CBs and
!> server-bound CBs
subroutine fetch_node_shmem_structs(context)
  implicit none
  class(ioserver_context), intent(inout) :: context

  integer             :: i
  integer(C_INTPTR_T) :: new
  type(C_PTR)         :: my_base, local_addr, temp
  logical             :: success
  integer :: target_rank, num_heaps

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
      temp       = context % local_heaps(target_rank) % clone(local_addr)

      num_heaps = context % local_heaps(target_rank) % register(local_addr)
      if (num_heaps < 0) then
        print *, 'ERROR: Could not register other PE heap locally', i
        error stop 1
      end if

      call context % local_heaps(target_rank) % set_base(my_base)
    end if
  end do
end subroutine fetch_node_shmem_structs

!> Set time to quit flag in control area
subroutine IOserver_set_time_to_quit(context)
  implicit none
  class(ioserver_context), intent(inout) :: context
  context % shmem % time_to_quit = 1
  if (context % debug_mode) then 
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
end subroutine IOserver_set_time_to_quit

!> Check whether the "time-to-quit" flag has been set on this node
function IOserver_is_time_to_quit(context) result(is_time_to_quit)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_time_to_quit
  is_time_to_quit = (context % shmem % time_to_quit == 1)
end function IOserver_is_time_to_quit

!> Get a pointer to the shared memory region specific to model nodes (so model + relay PEs)
function IOserver_get_relay_shmem(context) result(p_relay)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(C_PTR) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  p_relay  = context % model_shmem
end function IOserver_get_relay_shmem

!> Check that address translations are coherent local <--> remote
subroutine verify_translations(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  integer(C_INTPTR_T), dimension(0:1024) :: iora1, iora2
  integer(C_INTPTR_T) :: iora0
  integer :: i, errors
  type(C_PTR) :: temp
  
  iora0 = transfer(context % model_shmem, iora0)  ! local address of relay shared memory (shared between relay and compute PEs)

  do i = 0, context % smp_size -1              ! model_shmem for all PEs for which it makes sense (put into long integer for later comparison)
    iora1(i) = transfer(context % shmem % pe(i) % arena_ptr, iora1(i))
  enddo
  !   write(6,'(A,/(5Z18.16))') 'IO-RA :', iora1(0:smp_size -1)
  do i = 0, context % smp_size -1              ! translate local address into other PE address (put into long integer for later comparison)
    temp = context % ptr_translate_to(context % model_shmem, NODE_COLOR, i)
    iora2(i) = transfer(temp, iora2(i))
  enddo
  !   write(6,'(A,/(5Z18.16))') '      :', iora2(0:smp_size -1)
  errors = 0
  ! iora2(0) = 1  ! force error to test detection
  do i = 0, context % smp_size -1              ! check that local --> remote translation is correct
    if(iora1(i) .ne. iora2(i) ) then
      errors = errors + 1
      write(6,'(A,Z18.16,A,Z18.16)') 'ERROR: bad local -> remote address translation, expected',iora1(i) , ' found', iora2(i)
    endif
  enddo
  if(errors == 0) then
    ! write(6,*) 'INFO: local -> remote address translations are coherent'
  else
    write(6,*) 'INFO: number of errors in local -> remote address translations =',errors
  endif
  do i = 0, context % smp_size -1     ! translate other PE adddress into local address (put into long integer for later comparison)
    temp = context % ptr_translate_from(context % shmem % pe(i) % arena_ptr, NODE_COLOR, i)
    iora2(i) = transfer(temp, iora2(i))
  enddo
  errors = 0
  ! iora2(0) = 0  ! force error to test detection
  do i = 0, context % smp_size -1              ! check that remote --> local translation is correct
    if(iora0 .ne. iora2(i) ) then
      errors = errors + 1
      write(6,'(A,Z18.16,A,Z18.16)') 'ERROR: bad remote -> local address translation, expected',iora0 , ' found', iora2(i)
    endif
  enddo
  if(errors == 0) then
    ! write(6,*) 'INFO: remote -> local address translations are coherent'
  else
    write(6,*) 'INFO: number of errors in remote -> local address translations =',errors
  endif

end subroutine verify_translations

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
      crs % comm = context % all_comm

    case(NODE_COLOR)                              ! all PEs on this SMP node        (subset of global_comm)
      crs % comm = context % smp_comm

    case(SERVER_COLOR)                            ! server PEs                      (subset of all_comm)
      crs % comm = context % server_comm

    case(MODEL_COLOR + RELAY_COLOR)               ! compute and relay PEs           (subset of all_comm)
      crs % comm = context % model_relay_comm

    case(MODEL_COLOR)                             ! all model compute PEs           (subset of all_comm, model_relay_comm)
      crs % comm = context % model_comm

    case(RELAY_COLOR)                             ! all IO relay PEs                (subset of all_comm, model_relay_comm)
      crs % comm = context % relay_comm

    case(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)  ! compute and relay PEs on SMP node (subset of smp_comm, model_comm, iorelay_comm)
      crs % comm = context % model_relay_smp_comm

    case(MODEL_COLOR + NODE_COLOR)                ! compute PEs on SMP node         (subset of  smp_comm, model_comm)
      crs % comm = context % model_smp_comm

    case(RELAY_COLOR + NODE_COLOR)                ! relay PEs on SMP node           (subset of  smp_comm, iorelay_comm)
      crs % comm = context % relay_smp_comm
    
    case(RELAY_COLOR + NODE_COLOR + SERVER_BOUND_COLOR) ! server-bound relay PEs on SMP node (subset of relay_smp_comm)
      crs % comm = context % server_bound_relay_smp_comm

    case(RELAY_COLOR + SERVER_COLOR)              ! relay and server PEs            (subset of all_comm)
      crs % comm = context % allio_comm

    case(SERVER_COLOR + NODE_COLOR)               ! server PEs on SMP node          (subset of smp_comm, server_comm)
      ! crs % comm = context % servercom
      crs % comm = context % server_active_comm

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

!> Get a pointer to the shared memory arena on this node
function get_local_arena_ptr(context) result(ptr)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(C_PTR) :: ptr
  ptr = context % local_arena_ptr
end function get_local_arena_ptr

!> Get a local accessor to a specific stream
function IOserver_get_stream(context, stream_id) result(stream)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_id !< ID of the stream we want to access
  type(local_server_stream), pointer     :: stream
  stream => context % local_server_streams(stream_id)
end function IOserver_get_stream

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
  logical,                 intent(in)    :: mode
  context % debug_mode = mode
  print *, 'INFO: debug mode =', context % debug_mode
end subroutine IOserver_set_debug

!> Set the function that will be called by relay PEs to do their work
subroutine IOserver_set_relay(context, fn) 
  implicit none
  class(ioserver_context), intent(inout) :: context
  external :: fn
  context % io_relay_fn = C_FUNLOC(fn)
end subroutine IOserver_set_relay

!> [unused] Set the function that will be called by server PEs to do their work
subroutine IOserver_set_server(context, fn)  ! set server function to call to fn
  implicit none
  class(ioserver_context), intent(inout) :: context
  external :: fn
  context % io_server_fn = C_FUNLOC(fn)
  return
end subroutine IOserver_set_server

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

  if (context % debug_mode) print *,'DEBUG: NO-OP process, global rank =', context % global_rank
  do while (.not. context % is_time_to_quit())    ! sleep loop until quit flag appears
    if (context % debug_mode) print *,'MSG: SLEEP LOOP'
    sleep_dummy = sleep(1)
  enddo
  if (context % debug_mode) then
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
    header % content_length     = 0
    header % command            = MSG_COMMAND_MODEL_STOP
    header % tag                = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank
    end_cap % msg_length = header % content_length
    success = this % local_server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % local_server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success
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
    if (this % debug_mode) print *, 'Relay sending STOP signal', this % local_dcb % get_server_bound_client_id()
    header % content_length     = 0
    header % command            = MSG_COMMAND_RELAY_STOP
    header % sender_global_rank = this % global_rank
    header % relay_global_rank  = this % global_rank
    end_cap % msg_length = header % content_length

    success = this % local_dcb % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % local_dcb % put_elems(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success

    if (.not. success) then
      if (this % debug_mode) print *, 'WARNING: Relay could not send a stop signal!!!'
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

subroutine IOserver_finalize(context)
  implicit none
  type(ioserver_context), intent(inout) :: context
  call context % finalize()
end subroutine IOserver_finalize

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

  if (C_ASSOCIATED(ptr) .and. context % debug_mode) print *, 'DEBUG: Allocated block from arena: ', full_block_name
end function allocate_from_arena

!> Allocate a block from the shared memory arena and create a heap in it
function create_local_heap(context, num_bytes) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer(C_SIZE_T),       intent(in)    :: num_bytes !< Desired size of the heap (in bytes)
  logical :: success

  type(C_PTR)    :: heap_ptr
  integer(C_INT) :: index

  success = .false.

  heap_ptr = context % allocate_from_arena(num_bytes, 'HEAP', context % shmem % pe(context % smp_rank) % rank)

  if (.not. C_ASSOCIATED(heap_ptr)) return

  heap_ptr = context % local_heap % create(heap_ptr, num_bytes)    ! allocate local heap
  index    = context % local_heap % set_default()                  ! make local_heap the default heap
  call context % local_heap % set_base(context % arena % addr())   ! set arena address as offset base for heap
  context % shmem % pe(context % smp_rank) % heap_offset = Pointer_offset(context % arena % addr(), heap_ptr, 1)    ! offset of my heap in memory arena

  if (C_ASSOCIATED(heap_ptr) .and. index >= 0) success = .true.
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

!> Create all MPI communicators that will be used within this IO-server context, based on the given number of each kind of process.
!> This must be called by every process that want to participate in the IO-server
function IOserver_init_communicators(context, is_on_server, num_relay_per_node, num_server_bound_server, num_channels, num_server_noop) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  logical,                 intent(in)    :: is_on_server            !< Whether the calling process is located on the server
  integer,                 intent(in)    :: num_relay_per_node      !< How many relays are on each model node (half will be server-bound, the other half model-bound) (ignored on server node)
  integer,                 intent(in)    :: num_server_bound_server !< How many server processes will handle server-bound data (ignored on model nodes)
  integer,                 intent(in)    :: num_channels            !< How many server processes will be communication channels (ignored on model nodes)
  integer,                 intent(in)    :: num_server_noop         !< How many server processes won't actually do anything (ignored on model nodes)

  logical :: success

  type(MPI_Comm) :: temp_comm
  integer :: temp_rank, temp_size
  logical :: is_initialized
  integer :: split_color ! Temporary, used to define splits in MPI communicators

  success = .false.

  ! Initialize MPI if not already done
  call MPI_Initialized(is_initialized)
  if(.not. is_initialized) call MPI_Init()

  ! Retrieve global MPI comm info
  call MPI_Comm_rank(context % global_comm, context % global_rank)
  call MPI_Comm_size(context % global_comm, context % global_size)

  ! Determine exact color for server processes, temporary color for processes on model nodes
  if (is_on_server) then
    call MPI_Comm_split(context % global_comm, 0, context % global_rank, temp_comm)
    call MPI_Comm_rank(temp_comm, temp_rank)
    call MPI_Comm_size(temp_comm, temp_size)

    if (temp_rank >= temp_size - num_server_noop) then
      context % color = NO_OP_COLOR
    else if (temp_rank >= temp_size - num_server_noop - num_channels) then
      context % color = SERVER_COLOR + CHANNEL_COLOR
    else if (temp_rank < num_server_bound_server) then
      context % color = SERVER_COLOR + SERVER_BOUND_COLOR
    else
      context % color = SERVER_COLOR + MODEL_BOUND_COLOR
    end if

    context % num_server_bound_server = num_server_bound_server
  else
    call MPI_Comm_split(context % global_comm, 1, context % global_rank, temp_comm)
    context % color = MODEL_COLOR + RELAY_COLOR
  end if

  if (context % debug_mode) print *, 'DEBUG: Process global_rank, color: ', context % global_rank, context % color

  ! Split all PEs by node (mostly used for model nodes)
  call MPI_Comm_split_type(context % global_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, context % smp_comm)
  call MPI_Comm_rank(context % smp_comm, context % smp_rank)    ! rank on SMP node
  call MPI_Comm_size(context % smp_comm, context % smp_size)    ! population of SMP node

  ! Split global communicator into : (server+model+relay) and no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node (a deadlock will happen if this condition is not respected)
  ! color/NO_OP_COLOR has value 1 if NO-OP, 0 otherwise (NO_OP_COLOR MUST BE THE LARGEST CODE VALUE)
  call MPI_Comm_split(context % global_comm, context % color / NO_OP_COLOR, context % global_rank, context % all_comm)

  ! no-op processes don't need to go any further
  if (context % color == NO_OP_COLOR) then
    success = .true.
    return
  end if

  ! Split the all useful communicator (all_comm) into : server and (model+relay)
  split_color = context % color
  if (context % is_server()) split_color = SERVER_COLOR                                  ! treat INPUT and OUTPUT server PEs the same way for this split
  call MPI_Comm_split(context % all_comm, split_color, context % global_rank, temp_comm) ! temp_comm is either server_comm or model_relay_comm

  if (context % is_server()) then
    !-------------------
    ! Server processes
    !-------------------
    context % server_comm = temp_comm             ! communicator for the "io server(s)"

    block
      type(MPI_Comm) :: server_comm_tmp
      integer :: server_comm_size_tmp, server_comm_size

      ! Verify that all server processes are indeed on the same node
      call MPI_Comm_split_type(context % server_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, server_comm_tmp)
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
      call MPI_Comm_split(context % server_comm, SERVER_COLOR, context % server_comm_rank, context % server_active_comm)

      ! Split between server-bound and model-bound server processes
      if (context % is_server_bound()) then
        call MPI_Comm_split(context % server_active_comm, SERVER_BOUND_COLOR, context % global_rank, context % server_bound_server_comm)
      else
        call MPI_Comm_split(context % server_active_comm, MODEL_BOUND_COLOR, context % global_rank, context % model_bound_server_comm)
      end if

    end if

  else
    !----------------------------
    ! Model and relay processes
    !----------------------------
    context % model_relay_comm = temp_comm              ! communicator for "model compute and io relay" PEs

    ! Split model_relay_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(context % model_relay_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, context % model_relay_smp_comm)
    call MPI_Comm_rank(context % model_relay_smp_comm, context % model_relay_smp_rank)     ! rank on SMP node
    call MPI_Comm_size(context % model_relay_smp_comm, context % model_relay_smp_size) ! population of SMP node

    ! Split between model and relay processes
    ! Spread relay PEs across the node (lowest and highest node ranks)
    if (context % model_relay_smp_rank >= ((num_relay_per_node+1)/2) .and. &
        context % model_relay_smp_rank < (context % model_relay_smp_size - ((num_relay_per_node)/2))) then   ! model compute process
      ! Model process
      context % color = MODEL_COLOR
      if (context % debug_mode) print *,'DEBUG: model compute process, node rank =', &
          context % model_relay_smp_rank, context % model_relay_smp_size, num_relay_per_node/2, (context % model_relay_smp_size - ((num_relay_per_node+1)/2))

      call MPI_Comm_split(context % model_relay_smp_comm, MODEL_COLOR, context % model_relay_smp_rank, context % model_smp_comm) ! Model processes on this node
      call MPI_Comm_split(context % model_relay_comm, MODEL_COLOR, context % global_rank, context % model_comm)                  ! Model processes on all nodes
    else
      ! Relay process
      context % color = RELAY_COLOR
      if (context % debug_mode) print *,'DEBUG: IO relay process, node rank =', &
          context % model_relay_smp_rank, context % model_relay_smp_size, num_relay_per_node/2, (context % model_relay_smp_size - ((num_relay_per_node+1)/2))

      call MPI_Comm_split(context % model_relay_smp_comm, RELAY_COLOR, context % model_relay_smp_rank, context % relay_smp_comm) ! Relay processes on this node
      call MPI_Comm_split(context % model_relay_comm, RELAY_COLOR, context % global_rank, context % relay_comm)                  ! Relay processes on all nodes

      ! Compute number of model processes on this node
      block
        integer :: node_size, relay_size
        call MPI_Comm_size(context % model_relay_smp_comm, node_size)
        call MPI_Comm_size(context % relay_smp_comm, relay_size)
        context % num_local_model_proc = node_size - relay_size
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

  ! Split all_comm into model and IO (relay+server) processes
  if (context % is_relay() .or. context % is_server()) then  
    call MPI_Comm_split(context % all_comm, RELAY_COLOR + SERVER_COLOR, context % global_rank, context % allio_comm) 
  else
    call MPI_Comm_split(context % all_comm, MODEL_COLOR, context % global_rank, temp_comm)
  endif

  success = .true.

end function IOserver_init_communicators

!> Allocate shared memory on the node and create all the structs in it that will be used by the IO server.
!> Must be called by all processes involved in the IO server
function IOserver_init_shared_mem(context) result(success)
  use shared_mem_alloc_module
  use simple_mutex_module
  implicit none

  class(ioserver_context), intent(inout) :: context
  logical :: success

  integer            :: num_errors
  integer(C_INT64_T) :: shmem_num_bytes

  integer     :: status, temp, total_errors
  integer     :: i_stream
  type(C_PTR) :: temp_ptr
  type(shared_server_stream)  :: dummy_server_stream
  logical :: alloc_success

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

  ! Allocate local tables
  allocate(context % node_relay_ranks(0:context % max_smp_pe))  ! size is overkill but it is easier
  context % node_relay_ranks(:) = -1
  allocate(context % node_model_ranks(0:context % max_smp_pe))  ! size is overkill but it is easier
  context % node_model_ranks(:) = -1

  allocate(context % model_bound_cbs (0:context % max_smp_pe))  ! size is overkill but it is easier
  allocate(context % server_bound_cbs(0:context % max_smp_pe))  ! size is overkill but it is easier
  allocate(context % local_heaps     (0:context % max_smp_pe))  ! size is overkill but it is easier

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

  ! Determine how many server-bound server processes can actually open streams (it has to be <= the number of relays)
  block
    integer :: val
    val = 0
    if (context % is_server() .or. context % is_relay()) then
      context % num_server_stream_owners = 0
      if (context % is_relay() .and. context % is_server_bound()) val = 1
      call MPI_Allreduce(val, context % num_server_stream_owners, 1, MPI_INTEGER, MPI_SUM, context % allio_comm)
      context % num_server_stream_owners = min(context % num_server_stream_owners, context % num_server_bound_server)
    end if
  end block

  if (context % is_server()) then
    ! =========================================================================
    ! ============================ IO server process ==========================
    ! =========================================================================

    ! Allocate shared memory used for intra node communication between PEs on a server node
    context % server_heap_shmem = RPN_allocate_shared(context % server_heap_shmem_size, context % server_comm) ! The heap

    ! Allocate shared memory to hold server stream instances
    context % server_file_shmem_size = MAX_NUM_SERVER_STREAMS * (storage_size(dummy_server_stream) / 8)
    context % server_file_shmem = RPN_allocate_shared(context % server_file_shmem_size, context % server_comm) ! The set of files that can be opened/written

    if (.not. c_associated(context % server_heap_shmem) .or. .not. c_associated(context % server_file_shmem)) then
      print *, 'ERROR: We have a problem! server_heap_shmem/server_file_shmem are not valid!'
      error stop 1
    end if

    call c_f_pointer(context % server_file_shmem, context % common_server_streams, [MAX_NUM_SERVER_STREAMS])

    ! Create shared data structures
    if (context % server_comm_rank == 0) then
      temp_ptr = context % node_heap % create(context % server_heap_shmem, context % server_heap_shmem_size)    ! Initialize heap
      do i_stream = 1, MAX_NUM_SERVER_STREAMS
        context % common_server_streams(i_stream) = shared_server_stream(i_stream, mod(i_stream-1, context % num_server_stream_owners))  ! Initialize stream files
      end do
    else
      ! Create local accessors to shared structures
      temp_ptr = context % node_heap % clone(context % server_heap_shmem)
      status   = context % node_heap % register(context % server_heap_shmem)

      if (status < 0) then
        print *, 'ERROR: Could not register cloned heap on server node!'
        num_errors = num_errors + 1
        goto 2
      end if
    end if

    temp = context % node_heap % set_default()
    call context % node_heap % set_base(context % server_heap_shmem)

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
    context % model_shmem = RPN_allocate_shared(context % model_shmem_size, context % model_relay_smp_comm)
    if (context % debug_mode) then
      write(6,'(A, I10, A, I4)') 'DEBUG: after MPI_Win_allocate_shared, size = ', context % model_shmem_size, ', rank = ', context % model_relay_smp_rank
      call flush(6)
    end if

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

    if (context % debug_mode) print *, 'DEBUG: rank', context % shmem % pe(context % smp_rank) % rank, ' in color', context % color

    ! Wait for memory arena to be initialized by rank 0 before allocating heap and circular buffer(s)
    call MPI_Barrier(context % model_relay_smp_comm)

    ! Create PE's own heap, model-bound CB and server-bound CB in the shared memory arena
    shmem_num_bytes = context % model_shmem_size / context % model_relay_smp_size        ! available size (in bytes) per PE on SMP node
    alloc_success = context % create_local_heap(int(shmem_num_bytes * 0.85_8, kind=C_SIZE_T))
    alloc_success = context % create_local_cb(int(shmem_num_bytes * 0.04_8, kind=C_SIZE_T), .false.) .and. alloc_success
    alloc_success = context % create_local_cb(int(shmem_num_bytes * 0.1_8, kind=C_SIZE_T), .true.) .and. alloc_success
    if (.not. alloc_success) goto 2

    if (context % debug_mode) then
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
  call MPI_Allreduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, context % all_comm)
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
      call MPI_Comm_size(context % allio_comm, allio_comm_size)
      allocate(context % iocolors(0:allio_comm_size))             ! collect colors for IO PEs, index in array is rank in allio_comm
      call MPI_Allgather(context % color, 1, MPI_INTEGER, context % iocolors, 1, MPI_INTEGER, context % allio_comm)
      if (context % debug_mode) write(6,'(A,10I8,(/19X,10I8))') ' DEBUG: IO colors =', context % iocolors(0:allio_comm_size-1)
    end block

    ! Create DCB
    if (context % is_relay()) then
      ! Relay process
      if (context % is_server_bound()) then
        success = context % local_dcb % create_bytes(context % allio_comm, MPI_COMM_NULL, DCB_SERVER_BOUND_TYPE, 0_8, 0_8)
      else if (context % is_model_bound()) then
        success = context % local_dcb % create_bytes(context % allio_comm, MPI_COMM_NULL, DCB_CLIENT_BOUND_TYPE, 0_8, 0_8)
      end if
    else if (context % is_channel()) then 
      ! Channel process
      success = context % local_dcb % create_bytes(context % allio_comm, context % server_comm, DCB_CHANNEL_TYPE, 0_8, 0_8)
    else
      ! "Working" server process
      if (context % is_server_bound()) then
        success = context % local_dcb % create_bytes(context % allio_comm, context % server_comm, DCB_SERVER_BOUND_TYPE, DCB_SERVER_BOUND_SIZE, DCB_MODEL_BOUND_SIZE)
      else
        success = context % local_dcb % create_bytes(context % allio_comm, context % server_comm, DCB_CLIENT_BOUND_TYPE, DCB_SERVER_BOUND_SIZE, DCB_MODEL_BOUND_SIZE)
      end if
    end if
    if (.not. success) num_errors = num_errors + 1
  endif

  if (context % debug_mode .and. (context % is_relay() .or. context % is_model())) then
    call context % build_print_model_index()
  end if

  allocate(context % messenger)
  call context % messenger % set_debug(context % debug_mode)
  call context % messenger % set_model_crs(context % get_crs(MODEL_COLOR))

  success = .true.
end function IOserver_init_shared_mem

!> Initialize the IO server context. *Must be called by every process that wants to participate in the IO server*
!> \return .true. if the initialization was successful, .false. otherwise
!> \sa ioserver_context::IOserver_init_communicators, ioserver_context::IOserver_init_shared_mem
function IOserver_init(context, is_on_server, num_relay_per_node, num_server_bound_server, num_channels, num_server_noop, in_debug_mode) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  logical,                 intent(in)    :: is_on_server            !< Whether the calling process is on the server
  integer,                 intent(in)    :: num_relay_per_node      !< How many relay processes we want on each node
  integer,                 intent(in)    :: num_server_bound_server !< How many server-bound processes (consumers) we want on the server
  integer,                 intent(in)    :: num_channels            !< How many MPI communication channels we want on the server
  integer,                 intent(in)    :: num_server_noop         !< How many server processes will do nothing
  logical, optional,       intent(in)    :: in_debug_mode           !< Wether we want to activate debug mode

  logical :: success

  procedure(), pointer :: p

  success = .false.
  context % debug_mode = .false.
  if (present(in_debug_mode)) context % debug_mode = in_debug_mode

  success = context % init_communicators(is_on_server, num_relay_per_node, num_server_bound_server, num_channels, num_server_noop)

  if (.not. success) then
    print *, 'ERROR were not able to properly initialize all communicators!'
    return
  end if

  success = context % init_shared_mem()

  if (.not. success) then
    print *, 'There were errors during shared memory initialization'
    return
  end if

  success = .true.

  ! ====================================================================================
  !            RELAY processes (no return to caller if io_relay_fn is defined)
  ! ====================================================================================
  if (context % is_relay()) then                    ! IO relay process, check if caller supplied relay routine

    if (C_ASSOCIATED(context % io_relay_fn)) then             ! caller supplied subroutine to be called on relay PEs
      if (context % debug_mode) print *, 'INFO: io_relay_fn is associated'
      call C_F_PROCPOINTER(context % io_relay_fn, p)          ! associate procedure pointer with caller supplied address

      ! call user supplied relay code that may or may not return
      if (context % debug_mode) write(6,*) 'INFO: no return from io_relay_fn'
      call p()    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call context % local_dcb % delete()
      call context % set_time_to_quit()         ! activate quit signal for NO-OP PEs
      if (context % debug_mode) write(6,*) 'FINAL: model+io node PE', context % model_relay_smp_rank + 1, '  of', context % model_relay_smp_size
      call MPI_Finalize()                       ! DO NOT return to caller, call finalize, then stop
      stop

    else                                        ! IO relay process on model node
      if (context % debug_mode) print *,'INFO: io_relay_fn is not associated'
      ! IO relay, back to caller, with relay status code, caller will call relay subroutine
    endif
  endif

  return
end function IOserver_init

end module ioserver_context_module
!  ==========================================================================================
!                                           END OF MODULE
!  ==========================================================================================
