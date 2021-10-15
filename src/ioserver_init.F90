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
module ioserver_memory_mod
  use ISO_C_BINDING
  use ioserver_constants
  use circular_buffer_module
  use shmem_heap
  implicit none

  integer, parameter :: MAX_NUM_STREAM_FILES = 128
  integer, parameter :: MAX_PES_PER_NODE = 128

  type, bind(C) :: pe_info 
    type(C_PTR)         :: ctrl_ra                  ! local address of control shared memory segment
    type(C_PTR)         :: io_ra                    ! local address of relay or server shared memory segment
    integer(C_INTPTR_T) :: heap                     ! offset into shared memory arena
    integer(C_INTPTR_T) :: cio_in                   ! offset into shared memory arena
    integer(C_INTPTR_T) :: cio_out                  ! offset into shared memory arena
    integer(C_INT)      :: color                    ! PE type (compute/relay/server/...)
    integer(C_INT)      :: rank                     ! rank on node + same color
    integer(C_INT), dimension(4) :: pad             ! pad to a size of 64 bytes
  end type

  integer, parameter :: MAX_SMS = 255
  type, bind(C) :: shared_memory
    integer(C_INT) :: version      = 10000                 ! version 1.0.0
    integer(C_INT) :: time_to_quit = 0
    ! NO-OP PEs do not use anything beyond this point
    type(C_PTR)         :: relay_out_ra = C_NULL_PTR       ! arena address for outbound relay process
    type(C_PTR)         :: relay_in_ra  = C_NULL_PTR       ! arena address for inbound relay process
    integer(C_INT), dimension(10) :: pad                   ! next item to be aligned on a 64 byte boundary
    type(qualified_address), dimension(0:MAX_SMS) :: sms   ! message pointer area
    ! pe MUST be the last element of this structure
    type(pe_info), dimension(0:MAX_PES_PER_NODE-1) :: pe   ! origin 0, indexed by rank
  end type

  save

  ! offsets with respect to local address for shared memory arena (add to local address to get correct target address)
  integer(C_INTPTR_T) :: relay_out_offset = 0               ! offset for server-bound relay PE arena address
  integer(C_INTPTR_T) :: relay_in_offset  = 0               ! offset for model-bound relay PE arena address

  type(shared_memory), pointer, volatile :: mem       => NULL()      !  will point to start of control shared memory
  integer :: max_smp_pe = 0
  integer, dimension(:), pointer :: relay_index       => NULL()      ! sm_rank of relay PEs
  integer :: max_relay_index = -1
  integer, dimension(:), pointer :: compute_index     => NULL()      ! sm_rank of model PEs
  integer :: max_compute_index = -1

  integer :: relay_out_rank = -1                            ! rank in pe_info table of server-bound relay process
  integer :: relay_in_rank  = -1                            ! rank in pe_info table of model-bound relay process

  private :: initialized
  logical :: initialized = .false.

  type(circular_buffer), dimension(:), allocatable, target :: circ_buffer_in   ! The CB objects belonging to model PEs (model-bound)
  type(circular_buffer), dimension(:), allocatable, target :: circ_buffer_out  ! The CB objects belonging to model PEs (server-bound)
  type(heap),            dimension(:), allocatable, target :: local_heaps      ! Shared memory heaps belonging to model PEs

  contains

  subroutine ioserver_memory_mod_init(address, n_pe)
    implicit none
    type(C_PTR), intent(IN), value     :: address
    integer, intent(IN), value :: n_pe

    if(initialized) return

    call C_F_POINTER(address, mem)             ! main control structure points to shared memory at ctrl_shmem
    max_smp_pe = n_pe-1
    mem % pe(0:max_smp_pe) = pe_info(C_NULL_PTR, C_NULL_PTR, 0, 0, 0, 0, 0, [0, 0, 0, 0])

    allocate(relay_index(0:max_smp_pe))        ! size is overkill but it is easier
    relay_index = -1
    allocate(compute_index(0:max_smp_pe))      ! size is overkill but it is easier
    compute_index = -1

    allocate(circ_buffer_in(0:max_smp_pe))     ! size is overkill but it is easier
    allocate(circ_buffer_out(0:max_smp_pe))    ! size is overkill but it is easier
    allocate(local_heaps(0:max_smp_pe))        ! size is overkill but it is easier

    initialized = .true.
  end subroutine ioserver_memory_mod_init

  subroutine build_print_model_index()
    implicit none
    type(C_PTR) :: listr(0:max_relay_index)
    integer(C_INTPTR_T) :: listr2(0:max_relay_index)
    type(C_PTR) :: listc(0:max_compute_index)
     integer(C_INTPTR_T) :: listc2(0:max_compute_index)
    integer :: i

    do i = 0, max_relay_index
      listr(i) = mem % pe(relay_index(i)) % io_ra
      listr2(i) = transfer(listr(i), listr2(i))
    enddo
    do i = 0, max_compute_index
      listc(i) = mem % pe(compute_index(i)) % io_ra
      listc2(i) = transfer(listc(i), listc2(i))
    enddo
    print *,'DEBUG: relay   =',relay_index(0:max_relay_index)
    print 1, listr2(0:max_relay_index)
    print *,'DEBUG: compute =',compute_index(0:max_compute_index)
    print 1, listc2(0:max_compute_index)
1   format(10X, 8Z18.16)
  end subroutine build_print_model_index

  subroutine build_relay_model_index()  ! translate node color table into relay and compute index tables
    implicit none
    integer :: i

    do i = 0, max_smp_pe
      if(mem % pe(i) % color == MODEL_COLOR) then
        compute_index(mem % pe(i) % rank) = i
        max_compute_index = max(max_compute_index, mem % pe(i) % rank)
      endif
      if(mem % pe(i) %color == RELAY_COLOR) then
        relay_index(mem % pe(i) % rank) = i
        max_relay_index = max(max_relay_index, mem % pe(i) % rank)
      endif
    enddo
  end subroutine build_relay_model_index

end module ioserver_memory_mod

module ioserver_internal_mod
  use ISO_C_BINDING
  use mpi_f08

  use circular_buffer_module
  use distributed_circular_buffer_module
  use ioserver_constants
  use ioserver_file_module
  use ioserver_memory_mod
  use ioserver_message_module
  use shmem_arena_mod
  use shmem_heap
  implicit none

  private

  ! Publish some types that are useful
  public :: heap, circular_buffer, distributed_circular_buffer, comm_rank_size

  save

  integer(C_SIZE_T), parameter :: DCB_SIZE = 4 * MBYTE                  ! Size of each CB within the DCB

  type, public :: ioserver_context
    private

    integer :: color = NO_COLOR

    !  ==========================================================================================
    !                                     shared memory areas
    !  ==========================================================================================
    ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
    type(C_PTR)       :: ctrl_shmem      = C_NULL_PTR !  address of main (control) shared memory area on this node
    integer(C_SIZE_T) :: ctrl_shmem_size = MBYTE      !  size of above, MUST BE AT LEAST the size of shared_memory

    ! information for model compute and IO relay PEs on a given SMP node
    ! shared memory used for heaps and circular buffers on a given SMP node
    ! (one heap and 2 circular buffers per compute PE)
    ! this memory is used for communications between IO relay and model compute PEs
    type(C_PTR)       :: relay_shmem          = C_NULL_PTR     !  base address of relay+model PEs shared memory area
    integer(C_SIZE_T) :: relay_shmem_size     = GBYTE          !  size of above

    ! shared memory on io server node(s), used by io server PEs to communicate
    ! used for 1 sided communications window between relay and server PEs
    ! not much memory (if any) is normally allocated on relay PEs for this purpose
    type(C_PTR)       :: server_heap_shmem      = C_NULL_PTR   !  base address of io server PEs shared memory area
    integer(C_SIZE_T) :: server_heap_shmem_size = 10 * GBYTE   !  size of above (we want a lot on the server for grid assembly)

    type(C_PTR)       :: server_file_shmem      = C_NULL_PTR
    ! integer(C_SIZE_T) :: server_file_shmem_size = MAX_NUM_STREAM_FILES * (storage_size(stream_file) / 8 + 4)
    integer(C_SIZE_T) :: server_file_shmem_size = 0

    !  ==========================================================================================
    !                                       communicators
    !  ==========================================================================================
    type(MPI_Comm) :: global_comm   = MPI_COMM_WORLD      !  MPI "WORLD" for this set of PEs
    integer        :: global_rank   = -1                  !  rank in global_comm
    integer        :: global_size   =  0                  !  population of global_comm

    type(MPI_Comm) :: smp_comm      = MPI_COMM_NULL       !  PEs on this SMP node        (any kind ) (subset of global_comm)
    integer        :: smp_rank      = -1                  !  rank in smp_comm
    integer        :: smp_size      =  0                  !  population of smp_comm

    type(MPI_Comm) :: model_relay_smp_comm = MPI_COMM_NULL  ! model+relays on current node
    type(MPI_Comm) :: model_smp_comm       = MPI_COMM_NULL  ! model PEs on current SMP node
    type(MPI_Comm) :: relay_smp_comm       = MPI_COMM_NULL  ! relay PEs on current SMP node

    ! global_comm  = all_comm + NO-OP PEs
    ! all_comm     = modelio_comm + server_comm
    ! modelio_comm = model_comm   + iorelay_comm
    type(MPI_Comm) :: all_comm             = MPI_COMM_NULL  ! non NO-OP PEs               (all nodes) (subset of global_comm)
    type(MPI_Comm) :: allio_comm           = MPI_COMM_NULL  ! all IO PEs (relay + server) (subset of all_comm)
    type(MPI_Comm) :: modelio_comm         = MPI_COMM_NULL  ! model compute and relay PEs (all nodes) (subset of all_comm)
    type(MPI_Comm) :: model_comm           = MPI_COMM_NULL  ! model compute PEs           (all nodes) (subset of all_comm and modelio_comm)
    type(MPI_Comm) :: iorelay_comm         = MPI_COMM_NULL  ! relay PEs                   (all nodes) (subset of all_comm and modelio_comm)
    type(MPI_Comm) :: server_comm          = MPI_COMM_NULL  ! IO server PEs               (subset of all_comm)
    type(MPI_Comm) :: server_active_comm   = MPI_COMM_NULL  ! Server PEs that consume relay data (subset of server_comm)

    integer :: model_relay_smp_rank = -1    !  rank in model_relay_smp_comm
    integer :: model_relay_smp_size = 0     !  population of model_relay_smp_comm
    integer :: server_comm_rank     = -1    !  rank in server_comm

    !  ==========================================================================================
    !                        miscellaneous
    !  ==========================================================================================
    logical :: debug_mode = .false.

    type(C_PTR) :: local_arena_ptr      ! will be used to compute offsets into local arena

    type(shmem_arena)      :: local_arena          ! local memory arena
    type(heap)             :: local_heap           ! local heap (located in memory arena)
    type(circular_buffer)  :: local_cio_in         ! inbound circular buffer  (located in memory arena)
    type(circular_buffer)  :: local_server_bound_cb        ! outbound circular buffer  (located in memory arena)
    type(distributed_circular_buffer) :: local_dcb ! Distributed circular buffer for communication b/w relay and server processes
    type(heap)             :: node_heap            ! On server node only. Heap that everyone there can use

    type(C_FUNPTR) :: io_relay_fn  = C_NULL_FUNPTR ! procedure to call on relay processes (if not NULL)
    type(C_FUNPTR) :: io_server_fn = C_NULL_FUNPTR ! procedure to call on server processes (if not NULL)

    integer, dimension(:), pointer :: iocolors => NULL()     ! color table for io server and relay processes

    type(ioserver_messenger),        pointer :: messenger => NULL()
    type(stream_file), dimension(:), pointer :: common_stream_files => NULL()  ! Located in shared memory
  contains

    private

    procedure, pass, public :: init => IOserver_int_init
    procedure, pass         :: init_communicators => IOserver_init_communicators
    procedure, pass         :: init_shared_mem => IOserver_init_shared_mem
    procedure, pass         :: is_initialized

    procedure, pass         :: finalize_model
    procedure, pass         :: finalize_relay
    procedure, pass         :: finalize_server
    procedure, pass, public :: finalize => ioserver_context_finalize_manually
    final                   :: IOserver_int_finalize

    procedure, pass, public :: get_crs => IOserver_get_crs
    procedure, pass, public :: get_local_heap => IOserver_get_heap
    procedure, pass, public :: get_server_bound_cb => IOserver_get_server_bound_cb
    procedure, pass, public :: get_model_bound_cb => IOserver_get_cio_in
    procedure, pass, public :: get_dcb => IOserver_get_dcb
    procedure, pass, public :: get_relay_shmem => IOserver_get_relay_shmem
    procedure, pass, public :: get_messenger => IOserver_get_messenger
    procedure, pass, public :: get_local_arena_ptr
    procedure, pass, public :: get_stream => IOserver_get_stream

    procedure, pass, public :: get_server_bound_cb_list
    procedure, pass, public :: get_heap_list

    procedure, pass         :: no_op => IOserver_noop
    procedure, pass, public :: set_relay_fn  => IOserver_set_relay
    procedure, pass, public :: set_server_fn => IOserver_set_server
    procedure, pass, public :: set_debug => IOserver_set_debug

    ! procedure, pass, public :: open_file => IOserver_open_file
    procedure, pass, public :: open_file_model
    procedure, pass, public :: open_file_server
    procedure, pass, public :: close_file_server
    
    procedure, pass, public :: ptr_translate_to
    procedure, pass, public :: ptr_translate_from

    procedure, pass :: fetch_node_shmem_structs
    procedure, pass :: print_io_colors
    procedure, pass :: verify_translations
  end type ioserver_context

contains

function open_file_model(context, filename) result(new_file)
  implicit none
  class(ioserver_context), intent(inout) :: context
  character(len=*),        intent(in)    :: filename
  type(server_file) :: new_file

  logical :: success

  if (.not. context % is_initialized()) then
    print *, 'ERROR: Cannot open file, context is *not* initialized.'
    error stop 1
  end if

  if (context % debug_mode) print *, 'DEBUG: Opening file with name ', filename

  new_file = server_file(context % global_rank, &
                         context % local_heap, &
                         context % local_server_bound_cb, &
                         context % debug_mode, &
                         context % messenger)
  success = new_file % open(filename)

  if (.not. success) print *, 'ERROR: Unable to open file, for some reason'
end function open_file_model

function open_file_server(context, filename, stream_id) result(new_file)
  implicit none
  class(ioserver_context), intent(inout) :: context
  character(len=*),        intent(in)    :: filename
  integer,                 intent(in)    :: stream_id
  type(stream_file), pointer :: new_file

  type(stream_file), pointer :: tmp_file
  logical :: success

  nullify(new_file)
  tmp_file => context % common_stream_files(stream_id)

  if (mod(stream_id, context % local_dcb % get_num_consumers()) .ne. context % local_dcb % get_consumer_id() + 1) then
    print *, 'ERROR, this file should be opened by consumer ', stream_id, context % local_dcb % get_consumer_id()
    error stop 1
  end if

  if (tmp_file % is_open()) then
    if (.not. tmp_file % is_same_name(filename)) then
      print *, 'ERROR, trying to RE-open a file with a different name!', filename
      error stop 1
    end if
    new_file => tmp_file
    return
  end if

  success = tmp_file % open(stream_id, filename, context % local_dcb % get_consumer_id())
  
  if (.not. success) then
    print *, 'ERROR, unable to properly open stream file ', filename, stream_id
    error stop 1
  end if

  new_file => tmp_file
end function open_file_server

function close_file_server(context, stream_id) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_id
  logical :: success

  type(stream_file), pointer :: the_file

  success = .false.
  the_file => context % common_stream_files(stream_id)
  if (the_file % is_open()) then
    if (the_file % get_owner_id() .ne. context % local_dcb % get_consumer_id()) then
      print *, 'ERROR, trying to close a file not owned by this consumer', stream_id, context % local_dcb % get_consumer_id()
      return
    end if
    success = the_file % close()
  else
    success = .true.
  end if
end function close_file_server


! from OTHER PE space address ==> LOCAL space address (in shared memory arena)
! get my address in shared memory arena from a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
function ptr_translate_from(context, from, from_color, from_rank) result(local)
  implicit none
  class(ioserver_context), intent(in)        :: context
  type(C_PTR),             intent(IN), value :: from
  integer(C_INT),          intent(IN), value :: from_color
  integer(C_INT),          intent(IN), value :: from_rank
  type(C_PTR) :: local

  integer(C_INTPTR_T) :: offset, new
  type(C_PTR)         :: my_base, new_base

  local = C_NULL_PTR
  my_base = mem % pe(context % smp_rank) % io_ra   ! local address of memory arena

  new_base = C_NULL_PTR                  ! find new base
  if(from_color == NODE_COLOR) then      ! translate from address of PE of rank from_rank in SMP node
    new_base = mem % pe(from_rank) % io_ra
  else if(from_color == MODEL_COLOR) then     ! translate from address of compute PE of rank from_rank
    new_base = mem % pe(compute_index(from_rank)) % io_ra
  else if(from_color == RELAY_COLOR) then     ! translate from address of relay PE of rank from_rank
    new_base = mem % pe(relay_index(from_rank)) % io_ra
  else
    print *,'ERROR(ptr_translate_from): invalid from_color',from_color
    return   ! invalid from_color
  endif

  offset = Pointer_offset(new_base, from, 1)      ! offset in other PE space
  if(offset < 0) print *,'ERROR(ptr_translate_from): negative offset'
  if(offset < 0) return                  ! not in shared memory arena

  new   = transfer(my_base, new)         ! make large integer from C pointer
  new   = new + offset                   ! add offset to my base
  local = transfer(new, local)           ! honest C pointer
! print *,'DEBUG(ptr_translate_from): from, from_color, from_rank, local', &
!        transfer(from,1_8), from_color, from_rank, transfer(local,1_8)
end function ptr_translate_from

! from LOCAL space address ==> OTHER PE space address (in shared memory arena)
! translate my address in shared memory arena into a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
function ptr_translate_to(context, from, to_color, to_rank) result(to)
  implicit none
  class(ioserver_context), intent(in) :: context
  type(C_PTR),    intent(IN), value :: from
  integer(C_INT), intent(IN), value :: to_color
  integer(C_INT), intent(IN), value :: to_rank
  type(C_PTR) :: to

  integer(C_INTPTR_T) :: offset, new
  type(C_PTR)         :: my_base, new_base

  to = C_NULL_PTR
  my_base = mem % pe(context % smp_rank) % io_ra   ! local address of memory arena
  offset = Pointer_offset(my_base, from, 1)       ! offset in local space
  if(offset < 0) print *,'ERROR(ptr_translate_to): negative offset'
  if(offset < 0) return                  ! not in shared memory arena

  new_base = C_NULL_PTR                  ! find new base
  if(to_color == NODE_COLOR) then        ! translate to address of PE of rank to_rank in SMP node
    new_base = mem % pe(to_rank) % io_ra
  else if(to_color == MODEL_COLOR) then       ! translate to address of compute PE of rank to_rank
    new_base = mem % pe(compute_index(to_rank)) % io_ra
  else if(to_color == RELAY_COLOR) then       ! translate to address of relay PE of rank to_rank
    new_base = mem % pe(relay_index(to_rank)) % io_ra
  else
    print *,'ERROR(ptr_translate_to): invalid to_color',to_color
    return   ! invalid to_color
  endif

  new = transfer(new_base, new)          ! make large integer from C pointer
  new = new + offset                     ! add offset to new base
  to  = transfer(new, to)                ! honest C pointer
! print *,'DEBUG(ptr_translate_to): from, to_color, to_rank, to', &
!         transfer(from,1_8), to_color, to_rank, transfer(to,1_8)
end function ptr_translate_to

subroutine fetch_node_shmem_structs(context)
  implicit none
  class(ioserver_context), intent(inout) :: context

  integer             :: i
  integer(C_INTPTR_T) :: new
  type(C_PTR)         :: my_base, local_addr, temp
  logical             :: success
  integer :: target_rank, num_heaps

  my_base = mem % pe(context % smp_rank) % io_ra

  do i = 0, max_smp_pe
    if (mem % pe(i) % color == MODEL_COLOR) then

      target_rank = mem % pe(i) % rank

      new        = transfer(my_base, new)              ! make large integer from C pointer
      new        = new + mem % pe(i) % cio_in          ! add offset to my base
      local_addr = transfer(new, local_addr)           ! honest C pointer
      success    = circ_buffer_in(target_rank) % create_bytes(local_addr)   ! Initialize the local circular buffer structure

      if (.not. success) then
        print *, 'ERROR: Could not fetch input CB from PE ', i
        error stop 1
      end if

      new        = transfer(my_base, new)              ! make large integer from C pointer
      new        = new + mem % pe(i) % cio_out         ! add offset to my base
      local_addr = transfer(new, local_addr)           ! honest C pointer
      success    = circ_buffer_out(target_rank) % create_bytes(local_addr)  ! Initialize the local circular buffer structure

      if (.not. success) then
        print *, 'ERROR: Could not fetch output CB from PE ', i
        error stop 1
      end if

      new        = transfer(my_base, new)
      new        = new + mem % pe(i) % heap
      local_addr = transfer(new, local_addr)
      temp       = local_heaps(target_rank) % clone(local_addr)

      num_heaps = local_heaps(target_rank) % register(local_addr)
      if (num_heaps < 0) then
        print *, 'ERROR: Could not register other PE heap locally', i
        error stop 1
      end if

      call local_heaps(target_rank) % set_base(my_base)
    end if
  end do
end subroutine fetch_node_shmem_structs

subroutine IOserver_set_time_to_quit() BIND(C,name='IOserver_set_time_to_quit')   ! set time to quit flag in control area
  implicit none
  mem % time_to_quit = 1
  print *,'MSG: time to quit'
end subroutine IOserver_set_time_to_quit

function IOserver_is_time_to_quit() result(status)  BIND(C,name='IOserver_is_time_to_quit')  ! is it time to quit ?
  implicit none
  integer(C_INT) :: status   ! .true. if time to quit
  status = mem % time_to_quit
end function IOserver_is_time_to_quit

function IOserver_get_relay_shmem(context) result(p_relay)!  get communication window / shared memory area addresses
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(C_PTR) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  p_relay  = context % relay_shmem
end function IOserver_get_relay_shmem

subroutine print_created(temp, name, sz32)
  implicit none
  type(C_PTR), intent(IN), value :: temp
  character(len=*), intent(IN) :: name
  integer, intent(IN), value :: sz32
  if(C_ASSOCIATED(temp)) then
    write(6,*)"block "//name//" created, size =",sz32
  else
    write(6,*)"block "//name//" creation failed, size =",sz32
  endif
end subroutine print_created

subroutine verify_translations(context)  ! chech that address translations are coherent local <--> remote
  implicit none
  class(ioserver_context), intent(in) :: context
  integer(C_INTPTR_T), dimension(0:1024) :: iora1, iora2
  integer(C_INTPTR_T) :: iora0
  integer :: i, errors
  type(C_PTR) :: temp
  
  iora0 = transfer(context % relay_shmem, iora0)  ! local address of relay shared memory (shared between relay and compute PEs)

  do i = 0, context % smp_size -1              ! relay_shmem for all PEs for which it makes sense (put into long integer for later comparison)
    iora1(i) = transfer(mem % pe(i) % io_ra , iora1(i))
  enddo
  !   write(6,'(A,/(5Z18.16))') 'IO-RA :', iora1(0:smp_size -1)
  do i = 0, context % smp_size -1              ! translate local address into other PE address (put into long integer for later comparison)
    temp = context % ptr_translate_to(context % relay_shmem, NODE_COLOR, i)
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
    write(6,*) 'INFO: local -> remote address translations are coherent'
  else
    write(6,*) 'INFO: number of errors in local -> remote address translations =',errors
  endif
  do i = 0, context % smp_size -1     ! translate other PE adddress into local address (put into long integer for later comparison)
    temp = context % ptr_translate_from(mem % pe(i) % io_ra, NODE_COLOR, i)
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
    write(6,*) 'INFO: remote -> local address translations are coherent'
  else
    write(6,*) 'INFO: number of errors in remote -> local address translations =',errors
  endif

end subroutine verify_translations

subroutine print_io_colors(context)
    implicit none
    class(ioserver_context), intent(in) :: context
    if (context % debug_mode) write(6,'(A,(15I5))')' DEBUG: colors =', mem % pe(0:max_smp_pe) % color
end subroutine print_io_colors

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
      crs % comm = context % modelio_comm

    case(MODEL_COLOR)                             ! all model compute PEs           (subset of all_comm, modelio_comm)
      crs % comm = context % model_comm

    case(RELAY_COLOR)                             ! all IO relay PEs                (subset of all_comm, modelio_comm)
      crs % comm = context % iorelay_comm

    case(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)  ! compute and relay PEs on SMP node (subset of smp_comm, model_comm, iorelay_comm)
      crs % comm = context % model_relay_smp_comm

    case(MODEL_COLOR + NODE_COLOR)                ! compute PEs on SMP node         (subset of  smp_comm, model_comm)
      crs % comm = context % model_smp_comm

    case(RELAY_COLOR + NODE_COLOR)                ! relay PEs on SMP node           (subset of  smp_comm, iorelay_comm)
      crs % comm = context % relay_smp_comm

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

function IOserver_get_heap(context) result(h)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(heap) :: h
  h = context % local_heap
end function IOserver_get_heap

function IOserver_get_cio_in(context) result(cio)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer) :: cio
  cio = context % local_cio_in
end function IOserver_get_cio_in

function IOserver_get_server_bound_cb(context) result(cio)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer) :: cio
  cio = context % local_server_bound_cb
end function IOserver_get_server_bound_cb

function IOserver_get_dcb(context) result(dcb)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(distributed_circular_buffer) :: dcb
  dcb = context % local_dcb
end function IOserver_get_dcb

function IOserver_get_messenger(context) result(messenger)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(ioserver_messenger), pointer :: messenger
  messenger => context % messenger
end function IOserver_get_messenger

function get_local_arena_ptr(context) result(ptr)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(C_PTR) :: ptr
  ptr = context % local_arena_ptr
end function get_local_arena_ptr

function IOserver_get_stream(context, stream_id) result(stream)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: stream_id
  type(stream_file), pointer :: stream
  stream => context % common_stream_files(stream_id)
end function IOserver_get_stream

function get_server_bound_cb_list(context) result(cbs)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(circular_buffer), dimension(:), pointer :: cbs
  if (context % debug_mode) print *, 'Gotta fix this function: get_server_bound_cb_list'
  cbs => circ_buffer_out
end function get_server_bound_cb_list

function get_heap_list(context) result(heaps)
  implicit none
  class(ioserver_context), intent(inout) :: context
  type(heap), dimension(:), pointer :: heaps
  if (context % debug_mode) print *, 'Gotta fix this function: get_heap_list'
  heaps => local_heaps
end function get_heap_list

subroutine IOserver_set_debug(context, mode)  ! set io server debug mode
  implicit none
  class(ioserver_context), intent(inout) :: context
  logical,                 intent(in)    :: mode
  context % debug_mode = mode
  print *,'INFO: debug mode =', context % debug_mode
end subroutine IOserver_set_debug

subroutine IOserver_set_relay(context, fn)  ! set relay function to call to fn
  implicit none
  class(ioserver_context), intent(inout) :: context
  external :: fn
  context % io_relay_fn = C_FUNLOC(fn)
  return
end subroutine IOserver_set_relay

subroutine IOserver_set_server(context, fn)  ! set server function to call to fn
  implicit none
  class(ioserver_context), intent(inout) :: context
  external :: fn
  context % io_server_fn = C_FUNLOC(fn)
  return
end subroutine IOserver_set_server

subroutine IOserver_noop(context)  !  NO OP loop to park processes with minimal CPU consumption
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
  do while (0 == IOserver_is_time_to_quit())    ! sleep loop until quit flag appears
    if (context % debug_mode) print *,'MSG: SLEEP LOOP'
    sleep_dummy = sleep(1)
  enddo
  write(6,'(A,(15I5))')' DEBUG: colors =',mem % pe(0:max_smp_pe) % color
  write(6,*)'FINAL:, NO-OP', context % global_rank
  call MPI_Finalize()
  stop
end subroutine IOserver_noop

function is_initialized(context)
  implicit none
  class(ioserver_context), intent(in) :: context
  logical :: is_initialized
  is_initialized = context % color .ne. NO_COLOR
end function is_initialized

subroutine finalize_model(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(message_header) :: header
  logical :: success

  if (this % color == MODEL_COLOR) then
      call this % messenger % bump_tag()
      header % length  = message_header_size_int()
      header % command = MSG_COMMAND_STOP
      header % tag     = this % messenger % get_msg_tag()
      success = this % local_server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
      success = this % local_server_bound_cb % put(header % length, 1_8, CB_KIND_INTEGER_4, .true.) .and. success
  else
    print *, 'Should NOT be calling "finish_model"'
  end if
end subroutine finalize_model

subroutine finalize_relay(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(message_header) :: header
  logical :: success

  if (this % color == RELAY_COLOR) then
    ! Send a stop signal
    if (this % debug_mode) print *, 'Relay sending STOP signal'
    header % length = 0
    header % command = MSG_COMMAND_STOP
    success = this % local_dcb % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % local_dcb % put_elems(header % length, 1_8, CB_KIND_INTEGER_4, .true.) .and. success

    if (.not. success) then
      if (this % debug_mode) print *, 'WARNING: Could not send a stop signal!!!'
    end if
  end if
end subroutine finalize_relay

subroutine finalize_server(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  type(stream_file), pointer :: file
  logical :: success
  integer :: i

  do i = 1, MAX_NUM_STREAM_FILES
    file => this % common_stream_files(i)
    if (file % is_open()) then
      if (file % get_owner_id() == this % local_dcb % get_consumer_id()) then
        print *, 'Heeeeeyyyy forgot to close file #, owned by #', i, file % get_owner_id()
        success = file % close()
      end if
    end if
  end do
end subroutine finalize_server

subroutine ioserver_context_finalize_manually(this)
  implicit none
  class(ioserver_context), intent(inout) :: this

  if (this % is_initialized()) then
    call IOserver_set_time_to_quit()

    if (this % color == SERVER_COLOR) then
      call this % finalize_server()
    else if (this % color == MODEL_COLOR) then
      call this % finalize_model()
    else if (this % color == RELAY_COLOR) then
      call this % finalize_relay()
    end if

    if (associated(this % iocolors)) deallocate(this % iocolors)
    if (associated(this % messenger)) deallocate(this % messenger)
    call this % local_dcb % delete() ! This will block if not everyone calls it

    this % color = NO_COLOR
  end if
end subroutine ioserver_context_finalize_manually

subroutine IOserver_int_finalize(context)
  implicit none
  type(ioserver_context), intent(inout) :: context
  call context % finalize()
end subroutine IOserver_int_finalize

function IOserver_init_communicators(context, nio_node, app_class) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: nio_node     ! number of relay processes per compute SMP node (1 or 2)
  character(len=*),        intent(in)    :: app_class

  logical :: success
  ! integer :: color

  type(MPI_Comm) :: temp_comm
  logical :: is_initialized
  integer :: io_color

  success = .false.

  call MPI_Initialized(is_initialized)      ! is MPI library already initialized ?
  if(.not. is_initialized) call MPI_Init()  ! initialize MPI if not already done
  call MPI_Comm_rank(context % global_comm, context % global_rank)
  call MPI_Comm_size(context % global_comm, context % global_size)

  if(app_class(1:1) == 'M') context % color = MODEL_COLOR  + RELAY_COLOR    ! model or relay app
  if(app_class(1:1) == 'S') context % color = SERVER_COLOR + INOUT_COLOR    ! IO server app (both input and output)
  if(app_class(1:1) == 'C') context % color = SERVER_COLOR + CHANNEL_COLOR  ! Channel process (on server, to communicate with relays)
  if(app_class(1:1) == 'O') context % color = OUTPUT_COLOR + SERVER_COLOR   ! IO server app (output only)
  if(app_class(1:1) == 'I') context % color = INPUT_COLOR  + SERVER_COLOR   ! IO server app (input only)
  if(app_class(1:1) == 'Z') context % color = NO_OP_COLOR                   ! null app
  if(context % color == NO_COLOR) return                                    ! miserable failure

  if (context % debug_mode) print *, 'DEBUG: Process global_rank, color: ', context % global_rank, context % color

  ! split by node, all PEs (the result communicator is temporary and not kept)
  call MPI_Comm_split_type(context % global_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, context % smp_comm)
  call MPI_Comm_rank(context % smp_comm, context % smp_rank)                  ! rank on SMP node
  call MPI_Comm_size(context % smp_comm, context % smp_size)                  ! population of SMP node

  ! split global communicator into : server+model+relay / no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node (a deadlock will happen if this condition is not respected)
  ! color/NO_OP_COLOR has value 1 if NO-OP, 0 otherwise (NO_OP_COLOR MUST BE THE LARGEST CODE VALUE)
  call MPI_Comm_split(context % global_comm, context % color / NO_OP_COLOR, context % global_rank, context % all_comm)

  ! no-op processes don't need to go any further
  if (context % color == NO_OP_COLOR) then
    success = .true.
    return
  end if

  ! split the all useful communicator (all_comm) into : server / model+relay
  io_color = context % color
  if (iand(context % color, SERVER_COLOR) == SERVER_COLOR) io_color = SERVER_COLOR  ! treat INPUT and OUTPUT server PEs the same way
  call MPI_Comm_split(context % all_comm, io_color, context % global_rank, temp_comm)         ! temp_comm is either server_comm or modelio_comm

  if (iand(context % color, SERVER_COLOR) == SERVER_COLOR) then
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

    ! Split between active and channel processes (communication channels, or 'ghost' processes)
    if (iand(context % color, CHANNEL_COLOR) == CHANNEL_COLOR) then
      call MPI_Comm_split(context % server_comm, CHANNEL_COLOR, 0, temp_comm)
    else
      call MPI_Comm_split(context % server_comm, SERVER_COLOR, context % server_comm_rank, context % server_active_comm)
    end if

  else
    !----------------------------
    ! Model and relay processes
    !----------------------------
    context % modelio_comm = temp_comm              ! communicator for "model compute and io relay" PEs

    ! Split modelio_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(context % modelio_comm, MPI_COMM_TYPE_SHARED, context % global_rank, MPI_INFO_NULL, context % model_relay_smp_comm)
    call MPI_Comm_rank(context % model_relay_smp_comm, context % model_relay_smp_rank)     ! rank on SMP node
    call MPI_Comm_size(context % model_relay_smp_comm, context % model_relay_smp_size) ! population of SMP node

    ! Split between model and relay processes
    ! Spread the nio_node relay PEs across the node (lowest and highest node ranks)
    if (context % model_relay_smp_rank >= ((nio_node+1)/2) .and. &
        context % model_relay_smp_rank < (context % model_relay_smp_size - ((nio_node)/2))) then   ! model compute process
    ! Model process
      context % color = MODEL_COLOR
      if (context % debug_mode) print *,'DEBUG: model compute process, node rank =', &
          context % model_relay_smp_rank, context % model_relay_smp_size, nio_node/2, (context % model_relay_smp_size - ((nio_node+1)/2))

      call MPI_Comm_split(context % model_relay_smp_comm, MODEL_COLOR, context % model_relay_smp_rank, context % model_smp_comm) ! Model processes on this node
      call MPI_Comm_split(context % modelio_comm, MODEL_COLOR, context % global_rank, context % model_comm)                 ! Model processes on all nodes
    else
    ! Relay process
      context % color = RELAY_COLOR
      if (context % debug_mode) print *,'DEBUG: IO relay process, node rank =', &
          context % model_relay_smp_rank, context % model_relay_smp_size, nio_node/2, (context % model_relay_smp_size - ((nio_node+1)/2))

      call MPI_Comm_split(context % model_relay_smp_comm, RELAY_COLOR, context % model_relay_smp_rank, context % relay_smp_comm) ! Relay processes on this node
      call MPI_Comm_split(context % modelio_comm, RELAY_COLOR, context % global_rank, context % iorelay_comm)               ! Relay processes on all nodes
    endif
  end if

  ! Split all_comm into model and IO (relay+server) processes
  if (iand(context % color, SERVER_COLOR) == SERVER_COLOR) context % color = SERVER_COLOR
  if (context % color == RELAY_COLOR .or. context % color == SERVER_COLOR) then  
    call MPI_Comm_split(context % all_comm, RELAY_COLOR + SERVER_COLOR, context % global_rank, context % allio_comm) 
  else
    call MPI_Comm_split(context % all_comm, MODEL_COLOR, context % global_rank, temp_comm)
  endif

  success = .true.

end function IOserver_init_communicators

function IOserver_init_shared_mem(context) result(success)
  use ioserver_file_module
  use shared_mem_alloc_module
  use simple_mutex_module
  implicit none

  class(ioserver_context), intent(inout) :: context
  logical :: success

  interface
    function sleep(nsec) result(left) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: nsec
      integer(C_INT) :: left
    end function sleep
  end interface

  integer :: num_errors
  integer(C_SIZE_T)  :: cb_size
  integer(C_INT64_T) :: shmsz64
  integer            :: sz32
  character(len=8)  :: heap_name, cioin_name, cioout_name

  integer     :: status, temp, total_errors
  type(C_PTR) :: temp_ptr
  type(stream_file)  :: dummy_stream_file
  ! type(simple_mutex) :: dummy_simple_mutex

  success = .false.
  num_errors = 0              ! none so far

  ! allocate shared memory segment used for control, communicator = smp_comm
  context % ctrl_shmem = RPN_allocate_shared(context % ctrl_shmem_size, context % smp_comm)

!   if(debug_mode) print *,'DEBUG: ctrl_shmem_size, smp_rank, ierr =',ctrl_shmem_size, smp_rank, ierr
  call ioserver_memory_mod_init(context % ctrl_shmem, context % smp_size) ! main control structure will point to shared memory at ctrl_shmem
  mem % pe(context % smp_rank) % ctrl_ra = context % ctrl_shmem           ! local address of control memory segment

  if (context % smp_rank == 0) mem % time_to_quit = 0   ! initialize quit flag to "DO NOT QUIT"
  call MPI_barrier(context % global_comm)    ! wait until control area initialization is done everywhere

  if (context % color == NO_OP_COLOR) then       ! this is a NO-OP process, enter wait loop for finalize
    mem % pe(context % smp_rank) % color = NO_OP_COLOR
    call context % no_op()              ! this subroutine will never return and call finalize
    call MPI_Finalize()                 ! no_op should never return, but ... in case it does
    stop
  endif

  ! ===================================================================================
  ! at this point we only have "active" PEs (model, relay, server)
  ! allocate node local shared memory used for intra node communications
  ! ===================================================================================
  if (context % debug_mode) temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/relay/server nodes in debug mode

  if (iand(context % color,SERVER_COLOR) == SERVER_COLOR) then
  ! =========================================================================
  ! ============================ IO server process ==========================
  ! =========================================================================
    ! Allocate shared memory used for intra node communication between PEs on a server node
    context % server_heap_shmem = RPN_allocate_shared(context % server_heap_shmem_size, context % server_comm) ! The heap

    context % server_file_shmem_size = MAX_NUM_STREAM_FILES * (storage_size(dummy_stream_file) / 8)
    context % server_file_shmem = RPN_allocate_shared(context % server_file_shmem_size, context % server_comm) ! The set of files that can be opened/written

    if  (.not. c_associated(context % server_heap_shmem) .or. .not. c_associated(context % server_file_shmem)) then
      print *, 'ERROR: We have a problem! server_heap_shmem/server_file_shmem are not valid!'
      error stop 1
    end if

    call c_f_pointer(context % server_file_shmem, context % common_stream_files, [MAX_NUM_STREAM_FILES])

    ! Create server shared memory heap
    if (context % server_comm_rank == 0) then
      temp_ptr = context % node_heap % create(context % server_heap_shmem, context % server_heap_shmem_size)  ! Initialize heap
      context % common_stream_files(:) = stream_file()                                                        ! Initialize stream files
    else
      temp_ptr = context % node_heap % clone(context % server_heap_shmem)
      status   = context % node_heap % register(context % server_heap_shmem)

      if (status < 0) then
        print *, 'ERROR Could not register cloned heap on server node!'
        num_errors = num_errors + 1
        goto 2
      end if
    end if

    temp = context % node_heap % set_default()
    call context % node_heap % set_base(context % server_heap_shmem)

  else
  ! =========================================================================
  ! ======================= model compute or IO relay process ===============
  ! =========================================================================
    ! Allocate shared memory used for intra node communication between model and relay PEs
    context % relay_shmem = RPN_allocate_shared(context % relay_shmem_size, context % model_relay_smp_comm)
    if (context % debug_mode) write(6,*) 'DEBUG: after MPI_Win_allocate_shared relaywin, size, rank =', context % relay_shmem_size, context % model_relay_smp_rank
    call flush(6)

    if (context % model_relay_smp_rank == 0) then                   ! PE with rank on node == 0 creates the memory arena
      shmsz64 = context % relay_shmem_size
      temp_ptr = context % local_arena % create(context % relay_shmem, 128, shmsz64)
    else
      temp_ptr = context % local_arena % clone(context % relay_shmem)   !  cloning is O.K. even if arena is not initialized
    endif
    context % local_arena_ptr = context % local_arena % addr()
    mem % pe(context % smp_rank) % io_ra = context % local_arena_ptr    ! local address of arena segment
    mem % pe(context % smp_rank) % color = context % color              ! store color of this PE in shared memory table

    ! rank on node in my color
    if (context % color == RELAY_COLOR) then
      call MPI_Comm_rank(context % relay_smp_comm, mem % pe(context % smp_rank) % rank)
    else
      call MPI_Comm_rank(context % model_smp_comm, mem % pe(context % smp_rank) % rank)
    end if

    if (context % debug_mode) print *, 'DEBUG: rank', mem % pe(context % smp_rank) % rank, ' in color', context % color

    ! wait for memory arena to be initialized by rank 0 before allocating heap and circular buffer(s)
    call MPI_Barrier(context % model_relay_smp_comm)

    if (context % color == RELAY_COLOR) then                            ! io relay processes
    ! =========================================================================
    ! ================================ IO relay process =======================
    ! =========================================================================
      shmsz64 = context % relay_shmem_size / context % model_relay_smp_size        ! available size (in bytes) per PE on SMP node

      !  allocate nominal heap and circular buffers (8K elements)
      ! 85%  of per PE size for the heap
      write(heap_name  ,'(A5,I3.3)') "RHEAP", mem % pe(context % smp_rank) % rank
      sz32 = INT(shmsz64 / 4 * 0.85_8, 4)                    ! Heap size in 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32, heap_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      temp_ptr = context % local_heap % create(temp_ptr, sz32)         ! allocate local heap
      temp     = context % local_heap % set_default()                  ! make local_heap the default heap
      call context % local_heap % set_base( context % local_arena % addr() )     ! set arena address as offset base for heap
      mem % pe(context % smp_rank) % heap = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)    ! offset of my heap in memory arena

      !  4%  of per PE size for inbound circular buffer
      write(cioin_name ,'(A4,I4.4)') "RCIO", mem % pe(context % smp_rank)%rank
      cb_size = INT(shmsz64 * 0.04_8, 4)                     ! 4% for model-bound CB (in bytes)
      sz32    = INT(cb_size / 4, 4)                          ! Size in 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32, cioin_name)
      if (.not. C_ASSOCIATED(temp_ptr)) goto 2               ! allocation failed
      success = context % local_cio_in % create_bytes(temp_ptr, cb_size)
      if (.not. success) goto 2                              ! cio creation failed
      mem % pe(context % smp_rank) % cio_in = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)  ! offset of my inbound CIO in memory arena

      !  10%  of per PE size for outbound circular buffer
      write(cioout_name,'(A4,I4.4)') "RCIO", mem % pe(context % smp_rank) % rank + 1000
      cb_size = INT(shmsz64 * 0.1_8, 4)                      ! 10% for relay-bound CB (in bytes)
      sz32    = INT(cb_size / 4, 4)                          ! Size in 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32 / 4, cioout_name)
      if (.not. C_ASSOCIATED(temp_ptr)) goto 2               ! allocation failed
      success = context % local_server_bound_cb % create_bytes(temp_ptr, cb_size)
      if (.not. success) goto 2                              ! cio creation failed
      mem % pe(context % smp_rank) % cio_out = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)  ! offset of my outbound CIO in memory arena

    else                                                     ! compute processes
    ! =========================================================================
    ! ============================ model compute process ======================
    ! =========================================================================
      shmsz64 = context % relay_shmem_size / context % model_relay_smp_size      ! available size in bytes per PE on SMP node

      ! will also need rank on node for compute in rat table
      !  pe() % relay_ra = local_arena % addr()

      ! 85%  of per PE size for the heap
      write(heap_name  ,'(A5,I3.3)') "MHEAP",mem % pe(context % smp_rank) % rank
      sz32 = INT(shmsz64 / 4 * 0.85_8, 4)                    ! Heap size in number of 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32, heap_name)
      if (.not. C_ASSOCIATED(temp_ptr)) goto 2               ! allocation failed
      if (context % debug_mode) call print_created(temp_ptr, heap_name, sz32)
      temp_ptr = context % local_heap % create(temp_ptr, sz32)
      temp     = context % local_heap % set_default()                  ! make local_heap the default heap
      call context % local_heap % set_base( context % local_arena % addr() )     ! set arena address as offset base for heap
      mem % pe(context % smp_rank) % heap = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)    ! offset of my heap in memory arena

      !  4%  of per PE size for relay -> compute circular buffer
      write(cioin_name ,'(A4,I4.4)') "MCIO", mem % pe(context % smp_rank) % rank
      cb_size = INT(shmsz64 * 0.04_8, 4)                     ! Size in bytes, model-bound CB
      sz32    = INT(cb_size / 4,      4)                     ! Size in number of 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32, cioin_name)
      if (.not. C_ASSOCIATED(temp_ptr)) goto 2               ! allocation failed
      if (context % debug_mode) call print_created(temp_ptr, cioin_name, sz32)
      success = context % local_cio_in % create_bytes(temp_ptr, cb_size)
      if (.not. success) goto 2                              ! cio creation failed
      mem % pe(context % smp_rank) % cio_in = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)  ! offset of my inbound CIO in memory arena

      ! 10%  of per PE size for compute -> relay circular buffer
      write(cioout_name,'(A4,I4.4)') "MCIO", mem % pe(context % smp_rank) % rank + 1000
      cb_size = INT(shmsz64 * 0.1_8, 4)                      ! 10% for relay-bound circular buffer (in bytes)
      sz32    = INT(cb_size / 4,     4)                      ! Size in 32-bit elements
      temp_ptr = context % local_arena % newblock(sz32 / 4, cioout_name)
      if (.not. C_ASSOCIATED(temp_ptr)) goto 2               ! allocation failed
      if (context % debug_mode) call print_created(temp_ptr, cioout_name, sz32)
      success = context % local_server_bound_cb % create_bytes(temp_ptr, cb_size)
      if (.not. success) goto 2                              ! cio creation failed
      ! inject signature data into outbound buffer to prime the pump
!       nfree = local_server_bound_cb % atomic_put( [mem % pe(smp_rank) % rank + 10000], 1, .true.)
      mem % pe(context % smp_rank) % cio_out = Pointer_offset(context % local_arena % addr() , temp_ptr, 1)  ! offset of my outbound CIO in memory arena
    endif

    if (context % debug_mode) write(6,*)'DEBUG: allocated '//heap_name//' '//cioin_name//' '//cioout_name
    if (context % debug_mode) write(6,'(A,3Z18.16)') ' DEBUG: displacements =', &
                     mem % pe(context % smp_rank) % heap, mem % pe(context % smp_rank) % cio_in, mem % pe(context % smp_rank) % cio_out
    call flush(6)
  endif   ! (color == SERVER_COLOR)

  goto 3       ! no error, bypass
2 continue     ! go here upon error in memory allocation
  num_errors = num_errors + 1
3 continue     ! no error

  !  GLOBAL ERROR CHECK  (and synchronization point)
  call MPI_Allreduce(num_errors, total_errors, 1, MPI_INTEGER, MPI_SUM, context % all_comm)
  if (context % debug_mode) print *,'INFO: allocation error(s) detected =', total_errors
  call flush(6)
  if(total_errors > 0) then
    print *,'FATAL:',total_errors,' error(s) detected while allocating memory'
    call IOserver_set_time_to_quit()      ! tell NO-OP PEs to quit
    call MPI_Finalize()
    stop
  endif

  if (context % color == RELAY_COLOR .or. context % color == MODEL_COLOR) then
    call build_relay_model_index()
    call context % fetch_node_shmem_structs()
  endif

  ! IO Relay or Server
  if (context % color .ne. MODEL_COLOR) then              ! IO relay or IO server
    block
      integer :: allio_comm_size
      call MPI_Comm_size(context % allio_comm, allio_comm_size)
      allocate(context % iocolors(0:allio_comm_size))             ! collect colors for IO PEs, index in array is rank in allio_comm
      call MPI_Allgather(context % color, 1, MPI_INTEGER, context % iocolors, 1, MPI_INTEGER, context % allio_comm)
      if (context % debug_mode) write(6,'(A,10I8,(/19X,10I8))') ' DEBUG: IO colors =', context % iocolors(0:allio_comm_size-1)
    end block

    ! Create DCB
    if (context % color == RELAY_COLOR) then
      success = context % local_dcb % create_bytes(context % allio_comm, MPI_COMM_NULL, 0, 0_8)
    else
      success = context % local_dcb % create_bytes(context % allio_comm, context % server_comm, 2, DCB_SIZE)
    end if
    if (.not. success) num_errors = num_errors + 1
  endif

  if (context % debug_mode .and. (context % color == RELAY_COLOR .or. context % color == MODEL_COLOR)) call build_print_model_index()

  allocate(context % messenger)
  call context % messenger % set_debug(context % debug_mode)
  call context % messenger % set_model_crs(context % get_crs(MODEL_COLOR))

  success = .true.
end function IOserver_init_shared_mem

function IOserver_int_init(context, nio_node, app_class, debug_mode) result(success)
  implicit none
  class(ioserver_context), intent(inout) :: context
  integer,                 intent(in)    :: nio_node     ! number of relay processes per compute SMP node (1 or 2)
  character(len=*),        intent(in)    :: app_class
  logical,                 intent(in)    :: debug_mode
  logical :: success
  integer :: status

  procedure(), pointer :: p

  success = .false.
  status = NO_COLOR
  context % debug_mode = debug_mode

  success = context % init_communicators(nio_node, app_class)

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

  ! ===================================================================================
  !                     SERVER processes (no return to caller)
  ! ===================================================================================
  if (iand(context % color, SERVER_COLOR) == SERVER_COLOR) then
    if (context % local_dcb % get_channel_id() >= 0) then
      status = context % local_dcb % start_listening()
    else if (C_ASSOCIATED(context % io_server_fn)) then
      if (context % debug_mode) print *,'INFO: io_server_fn is associated'
      call C_F_PROCPOINTER(context % io_server_fn, p)    ! associate procedure pointer with caller supplied address
      ! call user supplied server code that may or may not return
      write(6,*) 'INFO: expecting no return from io_server_fn'
      call p()    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized
    else
      if (context % debug_mode) print *,'INFO: io_server_fn is not associated, calling default function'
      call io_server_out()                      ! not expected to return, but ...
    endif

    call context % local_dcb % delete()
    call IOserver_set_time_to_quit()          ! activate quit signal for NO-OP PEs
    write(6,*) 'FINAL: SMP node PE', context % smp_rank + 1,' of', context % smp_size
    call MPI_Finalize()                       ! DO NOT return to caller, call finalize, then stop
    stop
  endif

  ! ===================================================================================
  !                     RELAY processes (no return to caller if io_relay_fn is defined)
  ! ===================================================================================
  if (context % color == RELAY_COLOR) then                   ! IO relay process, check if caller supplied relay routine

    if (C_ASSOCIATED(context % io_relay_fn)) then             ! caller supplied subroutine to be called on relay PEs
      if (context % debug_mode) print *, 'INFO: io_relay_fn is associated'
      call C_F_PROCPOINTER(context % io_relay_fn, p)          ! associate procedure pointer with caller supplied address

      ! call user supplied relay code that may or may not return
      write(6,*) 'INFO: no return from io_relay_fn'
      call p()    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call context % local_dcb % delete()
      call IOserver_set_time_to_quit()          ! activate quit signal for NO-OP PEs
      write(6,*) 'FINAL: model+io node PE', context % model_relay_smp_rank + 1,'  of', context % model_relay_smp_size
      call MPI_Finalize()                       ! DO NOT return to caller, call finalize, then stop
      stop

    else                                        ! IO relay process on model node
      if (context % debug_mode) print *,'INFO: io_relay_fn is not associated'
      ! IO relay, back to caller, with relay status code, caller will call relay subroutine
    endif
  endif

  return
end function IOserver_int_init

end module ioserver_internal_mod
!  ==========================================================================================
!                                           END OF MODULE
!  ==========================================================================================
