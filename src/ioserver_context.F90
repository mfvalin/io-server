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

!> \author M. Valin,   Recherche en Prevision Numerique
!> \author V. Magnoux, Recherche en Prevision Numerique
!> \date 2020-2022

!> \file ioserver_context.F90
!> Fortran module that provides the main interface to the IO-server library

module ioserver_context_module
  !> \copydoc ioserver_context.F90
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
  use statistics_module
  implicit none

  private
  save

  ! Publish some types and constants that are useful
  public :: shmem_heap, circular_buffer, distributed_circular_buffer, comm_rank_size, model_stream, local_server_stream
  public :: no_op_function_template, default_no_op
  public :: MODEL_COLOR, SERVER_COLOR, RELAY_COLOR, SERVER_BOUND_COLOR, MODEL_BOUND_COLOR, NODE_COLOR
  public :: STREAM_PROCESSOR_COLOR, NO_COLOR, CHANNEL_COLOR

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
    integer :: num_stream_processors    = 0 !< How many server processes are used to process streams (open/close, write, do stuff to grids, etc.)
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

    !> How many milliseconds to wait when inserting a command into a stream's buffer (server-side).
    !> 0 means no waiting, a negative number means wait practically forever
    integer :: server_stream_put_cmd_timeout_ms = 0

    !> How many milliseconds to wait when putting a command to send to the server
    !> 0 means no waiting, a negative number means wait practically forever
    integer :: model_stream_put_cmd_timeout_ms = 0

    !> How many milliseconds to wait when putting data to send to the server 
    !> 0 means no waiting, a negative number means wait practically forever
    integer :: model_stream_put_data_timeout_ms = 0
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
  
  contains
    procedure, pass :: print => ioserver_input_parameters_print
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
    integer(C_SIZE_T) :: ctrl_shmem_size   = 0                !< Size of control shared memory

    ! information for model compute and IO relay PEs on a given SMP node
    ! shared memory used for heaps and circular buffers on a given SMP node
    ! (one heap and 2 circular buffers per compute PE)
    ! this memory is used for communications between IO relay and model compute PEs
    type(C_PTR)       :: model_shmem            = C_NULL_PTR  !< Address of relay+model PEs shared memory area
    integer(C_SIZE_T) :: model_shmem_size       = 0           !< Size of relay/model shared memory area

    type(C_PTR)       :: server_heap_shmem      = C_NULL_PTR  !< Address of io server PEs shared memory area
    integer(C_SIZE_T) :: server_heap_shmem_size = 0           !< Size of server shared memory area (we want a lot on the server for grid assembly)

    type(C_PTR)       :: server_file_shmem      = C_NULL_PTR  !< Address of server shared memory area for holding open file data
    integer(C_SIZE_T) :: server_file_shmem_size = 0           !< Size of server shared memory area for holding open file data (computed at initialization)

    type(C_PTR)       :: local_arena_ptr        = C_NULL_PTR  !< Pointer to start of shared memory arena
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

    integer :: io_comm                      = MPI_COMM_NULL  !< all IO PEs     (relay + server) (subset of active_comm)
    integer :: io_dcb_comm                  = MPI_COMM_NULL  !< all IO PEs that participate in the DCB (relay + server, *without stream processors*) (subset of active_comm)

    integer :: model_relay_comm             = MPI_COMM_NULL  !< model and relay PEs (all nodes) (subset of active_comm)
    integer :: model_comm                   = MPI_COMM_NULL  !< model PEs           (all nodes) (subset of model_relay_comm)
    integer :: relay_comm                   = MPI_COMM_NULL  !< relay PEs           (all nodes) (subset of model_relay_comm)

    integer :: server_comm                  = MPI_COMM_NULL  !< IO server PEs (subset of active_comm)
    integer :: server_work_comm             = MPI_COMM_NULL  !< Server PEs that process data (subset of server_comm, excludes channel PEs)
    integer :: server_dcb_comm              = MPI_COMM_NULL  !< Server PEs that participate in the DCB (subset of server_comm, *excludes stream processors*)
    integer :: server_bound_server_comm     = MPI_COMM_NULL  !< Server PEs that process server-bound relay data (subset of server_dcb_comm)
    integer :: model_bound_server_comm      = MPI_COMM_NULL  !< Server PEs that process model-bound relay data (subset of server_dcb_comm)
    integer :: stream_processor_server_comm = MPI_COMM_NULL  !< Server PEs that process streams, assembled grid data (subset of server_work_comm)

    integer :: model_relay_smp_comm         = MPI_COMM_NULL  !< model+relays on current node
    integer :: model_smp_comm               = MPI_COMM_NULL  !< model PEs on current SMP node
    integer :: relay_smp_comm               = MPI_COMM_NULL  !< relay PEs on current SMP node
    integer :: server_bound_relay_smp_comm  = MPI_COMM_NULL  !< server-bound relay PEs on current SMP node, subset of relay_smp_comm
    integer :: model_bound_relay_smp_comm   = MPI_COMM_NULL  !< model-bound relay PEs on current SMP node, subset of relay_smp_comm
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
    type(shared_server_stream), dimension(:), pointer :: common_server_streams => NULL() !< Streams used to assemble grids and write them (in shared memory)
    !> @}

    !-------------------
    !> @{ \name Stuff

    character(len=:), allocatable :: detailed_pe_name !< A (detailed) string containing PE role and various ranks
    character(len=:), allocatable :: short_pe_name    !< A (short) string containing PE role, node ID, node rank, and rank within the "role"

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
    procedure, pass, public :: init                     => ioctx_init                       !< \copydoc ioctx_init
    procedure, pass         :: init_communicators       => ioctx_init_communicators         !< \copydoc ioctx_init_communicators
    procedure, pass         :: init_shared_mem          => ioctx_init_shared_mem            !< \copydoc ioctx_init_shared_mem
    procedure, pass         :: build_relay_model_index  => ioctx_build_relay_model_index    !< \copydoc ioctx_build_relay_model_index
    procedure, pass         :: fetch_node_shmem_structs => ioctx_fetch_node_shmem_structs   !< \copydoc ioctx_fetch_node_shmem_structs
    procedure, pass         :: allocate_from_arena      => ioctx_allocate_from_arena        !< \copydoc ioctx_allocate_from_arena
    procedure, pass         :: create_local_heap        => ioctx_create_local_heap          !< \copydoc ioctx_create_local_heap
    procedure, pass         :: create_local_cb          => ioctx_create_local_cb            !< \copydoc ioctx_create_local_cb
    procedure, pass         :: make_pe_name             => ioctx_make_pe_name               !< \copydoc ioctx_make_pe_name
    !> @}

    !> @{ \name Process type query
    procedure, pass, public :: is_relay             => ioctx_is_relay             !< \copydoc ioctx_is_relay
    procedure, pass, public :: is_server            => ioctx_is_server            !< \copydoc ioctx_is_server
    procedure, pass, public :: is_model             => ioctx_is_model             !< \copydoc ioctx_is_model
    procedure, pass, public :: is_server_bound      => ioctx_is_server_bound      !< \copydoc ioctx_is_server_bound
    procedure, pass, public :: is_model_bound       => ioctx_is_model_bound       !< \copydoc ioctx_is_model_bound
    procedure, pass, public :: is_channel           => ioctx_is_channel           !< \copydoc ioctx_is_channel
    procedure, pass, public :: is_stream_processor  => ioctx_is_stream_processor  !< \copydoc ioctx_is_stream_processor
    procedure, pass, public :: is_no_op             => ioctx_is_no_op             !< \copydoc ioctx_is_no_op
    !> @}

    !> @{ \name Finalization
    procedure, pass         :: set_time_to_quit => ioctx_set_time_to_quit  !< \copydoc ioctx_set_time_to_quit
    procedure, pass         :: finalize_model   => ioctx_finalize_model    !< \copydoc ioctx_finalize_model
    procedure, pass         :: finalize_relay   => ioctx_finalize_relay    !< \copydoc ioctx_finalize_relay
    procedure, pass         :: finalize_server  => ioctx_finalize_server   !< \copydoc ioctx_finalize_server
    procedure, pass, public :: finalize         => ioctx_finalize_manually !< \copydoc ioctx_finalize_manually
    final                   :: ioctx_finalize                              !<
    !> @}

    !> @{ \name Getters
    procedure, pass         :: is_initialized       => ioctx_is_initialized       !< \copydoc ioctx_is_initialized
    procedure, pass, public :: is_time_to_quit      => ioctx_is_time_to_quit      !< \copydoc ioctx_is_time_to_quit
    procedure, pass, public :: get_num_local_model  => ioctx_get_num_local_model  !< \copydoc ioctx_get_num_local_model
    procedure, pass, public :: get_num_total_model  => ioctx_get_num_total_model  !< \copydoc ioctx_get_num_total_model

    procedure, pass, public :: get_global_rank     => ioctx_get_global_rank     !< \copydoc ioctx_get_global_rank
    procedure, pass, public :: get_crs             => ioctx_get_crs             !< \copydoc ioctx_get_crs
    procedure, pass, public :: get_local_heap      => ioctx_get_local_heap      !< \copydoc ioctx_get_local_heap
    procedure, pass, public :: get_node_heap       => ioctx_get_node_heap       !< \copydoc ioctx_get_node_heap
    procedure, pass, public :: get_server_bound_cb => ioctx_get_server_bound_cb !< \copydoc ioctx_get_server_bound_cb
    procedure, pass, public :: get_model_bound_cb  => ioctx_get_model_bound_cb  !< \copydoc ioctx_get_model_bound_cb
    procedure, pass, public :: get_dcb             => ioctx_get_dcb             !< \copydoc ioctx_get_dcb
    procedure, pass, public :: get_messenger       => ioctx_get_messenger       !< \copydoc ioctx_get_messenger
    procedure, pass, public :: get_stream          => ioctx_get_stream          !< \copydoc ioctx_get_stream
    procedure, pass, public :: get_relay_pipeline_depth   => ioctx_get_relay_pipeline_depth   !< \copydoc ioctx_get_relay_pipeline_depth
    procedure, pass, public :: get_server_pipeline_depth  => ioctx_get_server_pipeline_depth  !< \copydoc ioctx_get_server_pipeline_depth
    procedure, pass, public :: get_max_num_streams        => ioctx_get_max_num_streams        !< \copydoc ioctx_get_max_num_streams

    procedure, pass, public :: get_server_bound_cb_list => ioctx_get_server_bound_cb_list !< \copydoc ioctx_get_server_bound_cb_list
    procedure, pass, public :: get_heap_list            => ioctx_get_heap_list            !< \copydoc ioctx_get_heap_list
    !> @}

    !> @{ \name Process function management
    procedure, pass, public :: no_op => ioctx_noop      !< \copydoc ioctx_noop
    !> @}

    !> @{ \name Stream management
    procedure, pass, public :: open_stream_model      => ioctx_open_stream_model    !< \copydoc ioctx_open_stream_model
    !> @}
    
    !> @{ \name Debugging
    procedure, pass, public :: set_debug_level        => ioctx_set_debug_level      !< \copydoc ioctx_set_debug_level
    procedure, pass, public :: get_debug_level        => ioctx_get_debug_level      !< \copydoc ioctx_get_debug_level
    procedure, pass, public :: get_detailed_pe_name   => ioctx_get_detailed_pe_name !< \copydoc ioctx_get_detailed_pe_name
    procedure, pass, public :: get_short_pe_name      => ioctx_get_short_pe_name    !< \copydoc ioctx_get_short_pe_name

    procedure, pass :: print_io_colors        => ioctx_print_io_colors        !< \copydoc ioctx_print_io_colors
    procedure, pass :: print_shared_mem_sizes => ioctx_print_shared_mem_sizes !< \copydoc ioctx_print_shared_mem_sizes
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

contains

#include "ioserver_context_init.hf"
#include "ioserver_context_getters.hf"
#include "ioserver_context_final.hf"

function default_no_op(context) result(no_op_success)
  implicit none
  type(ioserver_context), intent(inout) :: context
  logical :: no_op_success
  no_op_success = .false.
  call context % no_op()
  no_op_success = .true.
end function default_no_op

!> Open a stream where the model can write data
subroutine ioctx_open_stream_model(context, new_stream)
  implicit none
  class(ioserver_context),     intent(inout) :: context     !< ioserver_context instance
  type(model_stream), pointer, intent(out)   :: new_stream  !< [out] A pointer to a newly-opened stream. NULL() if there was an error

  integer :: i_stream
  type(model_stream), pointer :: tmp_stream

  nullify(new_stream)

  if (.not. context % is_initialized()) then
    print '(A, A)', context % short_pe_name, ' ERROR: Cannot open file, context is *not* initialized.'
    return
  end if

  do i_stream = 1, context % params % max_num_concurrent_streams
    tmp_stream => context % local_model_streams(i_stream)
    if (.not. tmp_stream % is_open()) then
      if (tmp_stream % open()) then
        new_stream => tmp_stream
      else
        print '(A, A, I3)', context % short_pe_name, ' ERROR: Unable to open new model stream with rank ', i_stream
      end if
      return
    end if
  end do

  print '(A, A)', context % short_pe_name, ' ERROR: No space left in the list of (model) streams to open a new one'
end subroutine ioctx_open_stream_model

!> Set time to quit flag in control area
subroutine ioctx_set_time_to_quit(context)
  implicit none
  class(ioserver_context), intent(inout) :: context !< ioserver_context instance
  context % shmem % time_to_quit = 1
  if (context % get_debug_level() >= 2) print '(A, A)', context % short_pe_name, ' DEBUG: time to quit '
end subroutine ioctx_set_time_to_quit

!> Print some debugging information
subroutine ioctx_print_io_colors(context)
    implicit none
    class(ioserver_context), intent(in) :: context
    write(6,'(A, A,(15I5))') context % short_pe_name, ' DEBUG: colors =', context % shmem % pe(0:context % max_smp_pe) % color
end subroutine ioctx_print_io_colors

!> Set the debug flag for this context. See ioserver_input_parameters::debug_level
subroutine ioctx_set_debug_level(context, level)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer, intent(in) :: level
  context % params % debug_level = max(level, 0)
end subroutine ioctx_set_debug_level

!> NO OP loop to park processes with minimal CPU consumption
subroutine ioctx_noop(context) 
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
end subroutine ioctx_noop

!> Print information about shared memory allocation for this context
subroutine ioctx_print_shared_mem_sizes(context)
  implicit none
  class(ioserver_context), intent(in) :: context !< ioserver_context instance
  if (context % is_server()) then
    print '(A, A, F8.1, A)', context % short_pe_name, ' Shared memory heap: ', context % params % server_heap_size_mb, ' MB'
    if (.not. context % is_stream_processor()) then
      if (context % local_dcb % is_valid()) then
        print '(A,A,F8.1,A)', context % short_pe_name, ' Server-bound DCB:   ',                                   &
            context % params % dcb_server_bound_size_mb * context % local_dcb % get_num_server_bound_clients(),   &
            ' MB (total)'
        print '(A,A,F8.1,A)', context % short_pe_name, ' Model-bound DCB:    ', context % params % dcb_model_bound_size_mb, ' MB (per client)'
      end if
    end if
  else
    print '(A,A,F8.1,A)', context % short_pe_name, ' Shared memory heap: ', context % params % model_heap_size_mb, ' MB'
    print '(A,A,F8.1,A)', context % short_pe_name, ' Server-bound CB:    ', context % params % server_bound_cb_size_mb, ' MB'
    print '(A,A,F8.1,A)', context % short_pe_name, ' Model-bound CB:     ', context % params % model_bound_cb_size_mb, ' MB'
    if (context % is_relay()) then
      print '(A,A,F8.1,A)', context % short_pe_name, ' Server-bound DCB:   ', &
        real(context % local_dcb % get_capacity_local(CB_KIND_CHAR), kind=4) / MBYTE, ' MB'
    end if
  end if
end subroutine ioctx_print_shared_mem_sizes

subroutine ioserver_input_parameters_print(params)
  implicit none
  class(ioserver_input_parameters), intent(in) :: params

  integer, parameter :: LINE_LENGTH = 50
  integer, parameter :: COL = 30
  integer :: pos

  character(len=1) :: NL = new_line('a')
  character(len=5000) :: output

  pos = 1
  call add_line(output, pos, '----------- Input parameters ---------------')
  call add_line(output, pos, 'Debug level: ', '(I2)', val_i = params % debug_level)
  if (params % is_on_server) then
    call add_line(output, pos, 'Node type: ', val_str = 'Server node')
  else
    call add_line(output, pos, 'Node type: ', val_str = 'Model node')
  end if

  call add_line(output, pos, 'Process counts: ')
  if (params % is_on_server) then
    call add_line(output, pos, '  # stream processors ',      '(I10)', val_i = params % num_stream_processors)
    call add_line(output, pos, '  # server-bound server PEs', '(I10)', val_i = params % num_server_bound_server)
    call add_line(output, pos, '  # model-bound server PEs',  '(I10)', val_i = params % num_model_bound_server)
    call add_line(output, pos, '  # channel PEs',             '(I10)', val_i = params % num_channels)
  else
    call add_line(output, pos, '  # relays per node ',        '(I10)', val_i = params % num_relay_per_node)
  end if

  call add_line(output, pos, 'Shared memory: ')
  if (params % is_on_server) then
    call add_line(output, pos, '  Server heap size (MB)', '(F10.2)', val_r = params % server_heap_size_mb)
    call add_line(output, pos, '  Server-bound DCB size (MB)', '(F10.2)', val_r = params % dcb_server_bound_size_mb)
    call add_line(output, pos, '  Model-bound DCB size (MB)', '(F10.2)', val_r = params % dcb_model_bound_size_mb)
  else
    call add_line(output, pos, '  Model heap size (MB)', '(F10.2)', val_r = params % model_heap_size_mb)
    call add_line(output, pos, '  Server-bound CB size (MB)', '(F10.2)', val_r = params % server_bound_cb_size_mb)
    call add_line(output, pos, '  Model-bound CB size (MB)', '(F10.2)', val_r = params % model_bound_cb_size_mb)
  end if

  call add_line(output, pos, 'Stream control: ')
  call add_line(output, pos, '  Max # concurrent open streams', '(I10)', val_i = params % max_num_concurrent_streams)
  if (params % is_on_server) then
    call add_line(output, pos, '  Put command timeout (ms)', '(I10)', val_i = params % server_stream_put_cmd_timeout_ms)
  else
    call add_line(output, pos, '  Put command timeout (ms)', '(I10)', val_i = params % model_stream_put_cmd_timeout_ms)
    call add_line(output, pos, '  Put data timeout (ms)', '(I10)', val_i = params % model_stream_put_data_timeout_ms)
  end if

  call add_line(output, pos, 'Pipeline control: ')
  if (params % is_on_server) then
    call add_line(output, pos, '  Max server pipeline depth', '(I10)', val_i = params % server_pipeline_depth)
  else
    call add_line(output, pos, '  Max relay pipeline depth', '(I10)', val_i = params % relay_pipeline_depth)
  end if
  call add_line(output, pos, '--------------------------------------------')

  print '(A)', output(:pos-2)

  contains

  subroutine next_line(output, pos)
    implicit none
    character(len=*), intent(inout) :: output
    integer,          intent(inout) :: pos
    output(pos + LINE_LENGTH - 1: pos + LINE_LENGTH - 1) = NL
    pos = pos + LINE_LENGTH
  end subroutine next_line

  subroutine add_line(output, pos, name, fmt, val_r, val_i, val_str)
    implicit none
    character(len=*), intent(inout) :: output
    integer,          intent(inout) :: pos
    character(len=*), intent(in)    :: name
    character(len=*), intent(in), optional :: fmt
    integer,          intent(in), optional :: val_i
    real,             intent(in), optional :: val_r
    character(len=*), intent(in), optional :: val_str
    output(pos:) = name
    if (present(fmt) .and. present(val_i)) write(output(pos+COL:), fmt) val_i
    if (present(fmt) .and. present(val_r)) write(output(pos+COL:), fmt) val_r
    if (present(val_str)) output(pos+COL:) = val_str
    call next_line(output, pos)
  end subroutine add_line

end subroutine ioserver_input_parameters_print

end module ioserver_context_module
