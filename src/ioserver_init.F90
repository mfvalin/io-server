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
!! F_EnD
module ioserver_constants
  use ISO_C_BINDING
  implicit none
  integer(C_SIZE_T), parameter :: KBYTE = 1024
  integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
  integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

  integer, parameter :: NO_COLOR     = 0
  integer, parameter :: MODEL_COLOR  = 1
  integer, parameter :: RELAY_COLOR  = 2
  integer, parameter :: SERVER_COLOR = 4
  integer, parameter :: OUTPUT_COLOR = 8
  integer, parameter :: INPUT_COLOR  = 16
  integer, parameter :: INOUT_COLOR  = INPUT_COLOR + OUTPUT_COLOR
  integer, parameter :: NODE_COLOR   = 4096
  integer, parameter :: NO_OP_COLOR  = 8192   ! MUST BE THE HIGHEST VALUE

  integer, parameter :: IO_CONTROL   = 1000
  integer, parameter :: IO_BASE      = 1001
  integer, parameter :: IO_RELAY     = 1002
  integer, parameter :: IO_SERVER    = 1003

  type :: comm_rank_size
    integer :: comm = -1
    integer :: rank = -1
    integer :: size = 0
  end type
  type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(-1, -1, 0)

  type, bind(C) :: qualified_address
    type(C_PTR)    :: p                   ! address (C pointer)
    integer(C_INT) :: color               ! PE color (MODEL_COLOR | RELAY_COLOR | NODE_COLOR)
    integer(C_INT) :: rank                ! pe rank in above color
  end type

end module ioserver_constants

module ioserver_memory_mod
  use ISO_C_BINDING
  use ioserver_constants
  implicit none

  integer, parameter :: MAX_PES_PER_NODE = 128
  type, bind(C) :: pe_info 
    type(C_PTR)         :: ctrl_ra                  ! local address of control shared memory segment
    type(C_PTR)         :: io_ra                    ! local address of relay or server shared memory segment
    integer(C_INTPTR_T) :: heap                     ! offset into shared memory arena
    integer(C_INTPTR_T) :: cio_in                   ! offset into shared memory arena
    integer(C_INTPTR_T) :: cio_out                  ! offset into shared memory arena
    integer(C_INT)      :: color                    ! PE type (compute/relay/server/...)
    integer(C_INT)      :: rank                     ! rank on node
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
  integer(C_INTPTR_T) :: relay_out_offset = 0               ! offset for outbound relay PE arena address
  integer(C_INTPTR_T) :: relay_in_offset  = 0               ! offset for inbound relay PE arena address

  type(shared_memory), pointer, volatile :: mem       => NULL()      !  will point to start of control shared memory
  integer :: max_smp_pe = 0
  integer, dimension(:), pointer :: relay_index       => NULL()      ! sm_rank of relay PEs
  integer :: max_relay_index = -1
  integer, dimension(:), pointer :: compute_index     => NULL()      ! sm_rank of compute PEs
  integer :: max_compute_index = -1

  integer :: relay_out_rank = -1                            ! rank in pe_info table of outbound relay process
  integer :: relay_in_rank  = -1                            ! rank in pe_info table of inbound relay process

  private :: initialized
  logical :: initialized = .false.

  contains

  subroutine ioserver_memory_mod_init(address, n_pe)
    implicit none
    type(C_PTR), intent(IN), value     :: address
    integer, intent(IN), value :: n_pe

    if(initialized) return

    call C_F_POINTER(address, mem)             ! main control structure points to shared memory at ctrlmem
    max_smp_pe = n_pe-1
    mem % pe(0:max_smp_pe) = pe_info(C_NULL_PTR, C_NULL_PTR, 0, 0, 0, 0, 0, [0, 0, 0, 0])

    allocate(relay_index(0:max_smp_pe))        ! size is overkill but it is easier
    relay_index = -1
    allocate(compute_index(0:max_smp_pe))      ! size is overkill but it is easier
    compute_index = -1

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
  use ioserver_constants
  use shmem_heap
  use memory_arena_mod
  use circular_buffer_module
  use ioserver_memory_mod
  implicit none
  include 'mpif.h'

  save

  !  ==========================================================================================
  !                        shared memory areas , addresses, communicators, windows
  !  ==========================================================================================

  integer :: disp_unit     = 4                   !  displacement unit in 1 sided windows (32 bits by default)

  ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
  type(C_PTR)         :: ctrlmem   = C_NULL_PTR     !  address of main (control) shared memory area on this node
  integer(C_INTPTR_T) :: ctrlsiz   = MBYTE          !  size of above, MUST BE AT LEAST the size of shared_memory
  integer             :: ctrlwin   = MPI_WIN_NULL   !  NEVER USED for communications

  ! used by relay PEs and server PEs for 1 sided communications, used as shared memory by server PEs on same node
  type(C_PTR)         :: alliomem  = C_NULL_PTR     !  address of IO window (on all IO PEs) (same as servermem on server PEs)
  integer(C_INTPTR_T) :: alliosiz  = MBYTE          !  size of above
  integer             :: alliowin  = MPI_WIN_NULL   !  1 sided window used by RELAY + SERVER PEs
  integer             :: alliocom  = MPI_COMM_NULL  !  all IO PEs (model IO + IO server) (subset of all_comm)
  integer             :: alliorank = -1             !  rank in alliocom
  integer             :: alliosize = 0              !  population of alliocom

  ! information for model compute and IO relay PEs on a given SMP node
  ! shared memory used for heaps and circular buffers on a given SMP node
  ! (one heap and 2 circular buffers per compute PE)
  ! this memory is used for communications between IO relay and model compute PEs
  type(C_PTR)         :: relaymem  = C_NULL_PTR     !  base address of relay+model PEs shared memory area
  integer(C_INTPTR_T) :: relaysiz  = GBYTE          !  size of above
  integer             :: relaywin  = MPI_WIN_NULL   !  NEVER USED for communications
  integer             :: relaycom  = MPI_COMM_NULL  !  communicator for model + relay PEs on same SMP node
  integer             :: relayrank = -1             !  rank in relaycom
  integer             :: relaysize = 0              !  population of relaycom

  ! shared memory on io server node(s), used by io server PEs to communicate
  ! used for 1 sided communications window between relay and server PEs
  ! not much memory (if any) is normally allocated on relay PEs for this purpose
  type(C_PTR)         :: servermem = C_NULL_PTR     !  base address of io server PEs shared memory area
  integer(C_INTPTR_T) :: serversiz = GBYTE          !  size of above
  integer             :: serverwin  = MPI_WIN_NULL  !  NEVER USED for communications
  integer             :: servercom  = MPI_COMM_NULL !  communicator for  server PEs on same SMP node
  integer             :: serverrank = -1            !  rank in servercom
  integer             :: serversize = 0             !  population of servercom

  !  ==========================================================================================
  !                        communicators with no associated shared memory
  !  ==========================================================================================
  integer :: global_comm   = MPI_COMM_WORLD      !  MPI "WORLD" for this set of PEs
  integer :: global_rank   = -1                  !  rank in global_comm
  integer :: global_size   =  0                  !  population of global_comm

  integer :: smp_comm      = MPI_COMM_NULL       !  PEs on this SMP node        (any kind ) (subset of global_comm)
  integer :: smp_rank      = -1                  !  rank in smp_comm
  integer :: smp_size      =  0                  !  population of smp_comm

  integer :: model_smp_comm = MPI_COMM_NULL      !  compute PEs on same SMP node  (subset of relaycom)
  integer :: relay_smp_comm = MPI_COMM_NULL      !  relay PEs on same SMP node    (subset of relaycom)

  ! global_comm  = all_comm + NO-OP PEs
  ! all_comm     = modelio_comm + server_comm
  ! modelio_comm = model_comm   + iorelay_comm
  integer :: all_comm      = MPI_COMM_NULL       !  non NO-OP PEs               (all nodes) (subset of global_comm)
  integer :: modelio_comm  = MPI_COMM_NULL       !  model compute and relay PEs (all nodes) (subset of all_comm)
  integer :: model_comm    = MPI_COMM_NULL       !  model compute PEs           (all nodes) (subset of all_comm and modelio_comm)
  integer :: iorelay_comm  = MPI_COMM_NULL       !  relay PEs                   (all nodes) (subset of all_comm and modelio_comm)
  integer :: server_comm   = MPI_COMM_NULL       !  IO server PEs               (subset of all_comm)

  !  ==========================================================================================
  !                        miscellaneous
  !  ==========================================================================================
  logical :: debug_mode = .false.

  public :: local_arena, local_heap, local_cio_in, local_cio_out
  public :: local_arena_ptr, local_heap_ptr, local_cioin_ptr, local_cioout_ptr

  integer(C_INT), dimension(:), pointer        :: memory_ptr
  type(C_PTR), bind(C, name='LocalArenaPtr')   :: local_arena_ptr      ! will be used to compute offsets into local arena
  integer(C_INT), dimension(:), pointer        :: arena_ptr
  type(C_PTR), bind(C, name='LocalHeapPtr')    :: local_heap_ptr       ! will be used to compute offsets into local heap
  integer(DATA_ELEMENT), dimension(:), pointer :: heap_ptr
  type(C_PTR), bind(C, name='LocalCioinPtr')   :: local_cioin_ptr
  integer(DATA_ELEMENT), dimension(:), pointer :: cioin_ptr
  type(C_PTR), bind(C, name='LocalCiooutPtr')  :: local_cioout_ptr
  integer(DATA_ELEMENT), dimension(:), pointer :: cioout_ptr

  type(memory_arena)     :: local_arena          ! local memory arena
  type(heap)             :: local_heap           ! local heap (located in memory arena)
  type(circular_buffer)  :: local_cio_in         ! inbound circular buffer  (located in memory arena)
  type(circular_buffer)  :: local_cio_out        ! outbound circular buffer  (located in memory arena)

  type(C_FUNPTR) :: io_relay_fn  = C_NULL_FUNPTR ! procedure to call on relay processes (if not NULL)
  type(C_FUNPTR) :: io_server_fn = C_NULL_FUNPTR ! procedure to call on server processes (if not NULL)

  integer, dimension(:), pointer :: iocolors     ! color table for io server and relay processes
  contains

!! F_StArT
!! interface
!!
!! F_EnD

!! from OTHER PE space address ==> LOCAL space address (in shared memory arena)
!! get my address in shared memory arena from a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
!! F_StArT
function ptr_translate_from(from, from_color, from_rank) result(local) BIND(C,name='Ptr_translate_from')
!! F_EnD
  implicit none
!! F_StArT
!! import :: C_PTR, C_INT
  type(C_PTR), intent(IN), value :: from
  integer(C_INT), intent(IN), value :: from_color
  integer(C_INT), intent(IN), value :: from_rank
  type(C_PTR) :: local
!! F_EnD

  integer :: rank
  integer(C_INTPTR_T) :: offset, new
  type(C_PTR) :: my_base, new_base

  local = C_NULL_PTR
  my_base = mem % pe(smp_rank) % io_ra   ! local address of memory arena

  new_base = C_NULL_PTR                  ! find new base
  if(from_color == NODE_COLOR) then      ! translate from address of PE of rank from_rank in SMP node
    new_base = mem % pe(from_rank) % io_ra
  else if(from_color == MODEL_COLOR) then     ! translate from address of compute PE of rank from_rank
    new_base = mem % pe(compute_index(from_rank)) % io_ra
  else if(from_color == RELAY_COLOR) then     ! translate from address of relay PE of rank from_rank
    new_base = mem % pe(relay_index(from_rank)) % io_ra
  else
    return   ! invalid color
  endif

  offset = Pointer_offset(new_base, from, 1)      ! offset in other PE space
  if(offset < 0) return                  ! not in shared memory arena

  new   = transfer(my_base, new)         ! make large integer from C pointer
  new   = new + offset                   ! add offset to my base
  local = transfer(new, local)           ! honest C pointer

!! F_StArT
end function ptr_translate_from
!!
!! F_EnD

!! from LOCAL space address ==> OTHER PE space address (in shared memory arena)
!! translate my address in shared memory arena into a valid address for RELAY_COLOR|MODEL_COLOR|NODE_COLOR of rank N
!! F_StArT
function ptr_translate_to(from, to_color, to_rank) result(to) BIND(C,name='Ptr_translate_to')
!! F_EnD
  implicit none
!! F_StArT
!! import :: C_PTR, C_INT
  type(C_PTR), intent(IN), value :: from
  integer(C_INT), intent(IN), value :: to_color
  integer(C_INT), intent(IN), value :: to_rank
  type(C_PTR) :: to
!! F_EnD

  integer :: rank
  integer(C_INTPTR_T) :: offset, new
  type(C_PTR) :: my_base, new_base

  to = C_NULL_PTR
  my_base = mem % pe(smp_rank) % io_ra   ! local address of memory arena
  offset = Pointer_offset(my_base, from, 1)       ! offset in local space
  if(offset < 0) return                  ! not in shared memory arena

  new_base = C_NULL_PTR                  ! find new base
  if(to_color == NODE_COLOR) then        ! translate to address of PE of rank to_rank in SMP node
    new_base = mem % pe(to_rank) % io_ra
  else if(to_color == MODEL_COLOR) then       ! translate to address of compute PE of rank to_rank
    new_base = mem % pe(compute_index(to_rank)) % io_ra
  else if(to_color == RELAY_COLOR) then       ! translate to address of relay PE of rank to_rank
    new_base = mem % pe(relay_index(to_rank)) % io_ra
  else
    return   ! invalid color
  endif

  new = transfer(new_base, new)          ! make large integer from C pointer
  new = new + offset                     ! add offset to new base
  to  = transfer(new, to)                ! honest C pointer

!! F_StArT
end function ptr_translate_to
!!
!! F_EnD

!! F_StArT
subroutine IOserver_set_time_to_quit() BIND(C,name='IOserver_set_time_to_quit')   ! set time to quit flag in control area
!! F_EnD
  implicit none
  mem % time_to_quit = 1
  print *,'MSG: time to quit'
!! F_StArT
end subroutine IOserver_set_time_to_quit
!!
!! F_EnD

!! F_StArT  ! check time to quit flag in control area
function IOserver_is_time_to_quit() result(status)  BIND(C,name='IOserver_is_time_to_quit')  ! is it time to quit ?
!! import :: C_INT
  implicit none
  integer(C_INT) :: status   ! .true. if time to quit
!! F_EnD
  status = mem % time_to_quit
!! F_StArT
end function IOserver_is_time_to_quit
!!
!! F_EnD

function server_set_winsize(wkind, sz) result(status)  !  set shared memory area sizes
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  integer(C_SIZE_T), intent(IN) :: sz        ! shared memory area size
  logical :: status                          ! true if there was an error

  status = .true.
  if(sz <=0) return

  select case (wkind)
    case(IO_CONTROL)
      if(sz > KBYTE * 32) then         ! minimum size is the size of shared_memory
        ctrlsiz = sz                   ! control memory (normally not very large)
        status = .false.
      endif
    case(IO_BASE)
      if(sz > MBYTE) then
        alliosiz = sz                   ! 1 sided window on relay PEs
        status = .false.
      endif
    case(IO_RELAY)
      if(sz > MBYTE * 128) then
        relaysiz = sz                  ! memory shared between relay and compute PEs
        status = .false.
      endif
    case(IO_SERVER)
      if(sz > MBYTE * 128) then
        serversiz = sz                 ! shared memory and window on server PEs
        status = .false.
      endif
    case DEFAULT
      status = .true.
  end select
  return
end function server_set_winsize

function server_get_winsize(wkind) result(sz)  !  get shared memory area sizes
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  integer(C_SIZE_T)             :: sz

  select case (wkind)
    case(IO_CONTROL)
      sz = ctrlsiz
    case(IO_BASE)
      sz = alliosiz
    case(IO_RELAY)
      sz = relaysiz
    case(IO_SERVER)
      sz = serversiz
    case DEFAULT
      sz = 0
  end select
  return
end function server_get_winsize

function server_get_winptr(wkind) result(ptr)  !  get shared memory area addresses
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  type(C_PTR)                   :: ptr

  select case (wkind)
    case(IO_CONTROL)
      ptr = ctrlmem
    case(IO_BASE)
      ptr = alliomem
    case(IO_RELAY)
      ptr = relaymem
    case(IO_SERVER)
      ptr = servermem
    case DEFAULT
      ptr = C_NULL_PTR
  end select
  return
end function server_get_winptr


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

end module ioserver_internal_mod

!  ==========================================================================================
!                                           END OF MODULE
!  ==========================================================================================

!! F_StArT
subroutine verify_translations()  ! chech that address translations are coherent local <--> remote
!! F_EnD
  use ioserver_internal_mod
  implicit none
  integer(C_INTPTR_T), dimension(0:1024) :: iora1, iora2
  integer(C_INTPTR_T) :: iora0
  integer :: i, errors
  
  iora0 = transfer(relaymem, iora0)  ! local address of relay shared memory (shared between relay and compute PEs)

  do i = 0, smp_size -1              ! relaymem for all PEs for which it makes sense (put into long integer for later comparison)
    iora1(i) = transfer(mem % pe(i) % io_ra , iora1(i))
  enddo
!   write(6,'(A,/(5Z18.16))') 'IO-RA :', iora1(0:smp_size -1)
  do i = 0, smp_size -1              ! translate local address into other PE address (put into long integer for later comparison)
    iora2(i) = transfer(ptr_translate_to(relaymem, NODE_COLOR, i), iora2(i))
  enddo
!   write(6,'(A,/(5Z18.16))') '      :', iora2(0:smp_size -1)
  errors = 0
! iora2(0) = 1  ! force error to test detection
  do i = 0, smp_size -1              ! check that local --> remote translation is correct
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
  do i = 0, smp_size -1     ! translate other PE adddress into local address (put into long integer for later comparison)
    iora2(i) = transfer(ptr_translate_from(mem % pe(i) % io_ra, NODE_COLOR, i), iora2(i))
  enddo
  errors = 0
! iora2(0) = 0  ! force error to test detection
  do i = 0, smp_size -1              ! check that remote --> local translation is correct
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

!! F_StArT
end subroutine verify_translations
!!
!! F_EnD

!! F_StArT
subroutine print_io_colors()
!! F_EnD
    use ioserver_internal_mod
    implicit none
    if(debug_mode) write(6,'(A,(15I5))')' DEBUG: colors =',mem % pe(0:max_smp_pe) % color
!! F_StArT
end subroutine print_io_colors
!!
!! F_EnD

!! F_StArT
  function IOserver_get_crs(color) result(crs)
!! F_EnD
    use ioserver_internal_mod
!! F_StArT
!!  import :: comm_rank_size
    implicit none
    integer, intent(IN), value :: color
    type(comm_rank_size) :: crs
!! F_EnD
    integer :: ierr

    crs = comm_rank_size(MPI_COMM_NULL, -1, 0)

    select case(color)
      case(NO_COLOR)                                ! all non NO-OP PEs               (subset of global_comm)
        crs % comm = all_comm

      case(NODE_COLOR)                              ! all PEs on this SMP node        (subset of global_comm)
        crs % comm = smp_comm

      case(SERVER_COLOR)                            ! server PEs                      (subset of all_comm)
        crs % comm = server_comm

      case(MODEL_COLOR + RELAY_COLOR)               ! compute and relay PEs           (subset of all_comm)
        crs % comm = modelio_comm

      case(MODEL_COLOR)                             ! all model compute PEs           (subset of all_comm, modelio_comm)
        crs % comm = model_comm

      case(RELAY_COLOR)                             ! all IO relay PEs                (subset of all_comm, modelio_comm)
        crs % comm = iorelay_comm

      case(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)  ! compute and relay PEs on SMP node (subset of smp_comm, model_comm, iorelay_comm)
        crs % comm = relaycom

      case(MODEL_COLOR + NODE_COLOR)                ! compute PEs on SMP node         (subset of  smp_comm, model_comm)
        crs % comm = model_smp_comm

      case(RELAY_COLOR + NODE_COLOR)                ! relay PEs on SMP node           (subset of  smp_comm, iorelay_comm)
        crs % comm = relay_smp_comm

      case(RELAY_COLOR + SERVER_COLOR)              ! relay and server PEs            (subset of all_comm)
        crs % comm = alliocom

      case(SERVER_COLOR + NODE_COLOR)               ! server PEs on SMP node          (subset of smp_comm, server_comm)
        crs % comm = servercom

      case default
        crs % comm = MPI_COMM_NULL
    end select

    if(crs % comm .ne. MPI_COMM_NULL) then
      call MPI_Comm_rank(crs % comm, crs % rank, ierr)
      call MPI_Comm_size(crs % comm, crs % size, ierr)
    endif
!! F_StArT
  end function IOserver_get_crs
!! F_EnD

!! F_StArT
function IOserver_get_heap() result(h)
!! F_EnD
  use ioserver_internal_mod
  implicit none
!! F_StArT
!! import :: heap
  type(heap) :: h
!! F_EnD
  h = local_heap
!! F_StArT
end function IOserver_get_heap
!!
!! F_EnD

!! F_StArT
function IOserver_get_cio_in() result(cio)
!! F_EnD
  use ioserver_internal_mod
  implicit none
!! F_StArT
!! import :: circular_buffer
  type(circular_buffer) :: cio
!! F_EnD
  cio = local_cio_in
!! F_StArT
end function IOserver_get_cio_in
!!
!! F_EnD

!! F_StArT
function IOserver_get_cio_out() result(cio)
!! F_EnD
  use ioserver_internal_mod
  implicit none
!! F_StArT
!! import :: circular_buffer
  type(circular_buffer) :: cio
!! F_EnD
  cio = local_cio_out
!! F_StArT
end function IOserver_get_cio_out
!!
!! F_EnD

!! F_StArT
subroutine IOserver_debug(mode) BIND(C,name='IOserver_debug')   ! set io server debug mode
!! F_EnD
  use ioserver_internal_mod
  implicit none
!! F_StArT
!! import :: C_INT
  integer(C_INT), intent(IN), value :: mode
!! F_EnD
  debug_mode = (mode .ne. 0)
  print *,'INFO: debug mode =', debug_mode
!! F_StArT
end subroutine IOserver_debug
!!
!! F_EnD

!! F_StArT
function IOserver_CRSisnull(crs) result(status)   !  is this a NULL communicator combo ?
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: comm_rank_size
  implicit none
  type(comm_rank_size), intent(INOUT) :: crs
  logical :: status
!! F_EnD
  status = (crs % rank < 0 .or. crs % size <= 0)
  if(status) crs % comm = MPI_COMM_NULL
  return
!! F_StArT
end function IOserver_CRSisnull
!!
!! F_EnD

!! F_StArT
function IOserver_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
  implicit none
  integer, intent(IN) :: comm
  logical :: status
!! F_EnD
  status = (comm == MPI_COMM_NULL)
  return
!! F_StArT
end function IOserver_Commisnull
!!
!! F_EnD

!! F_StArT
function IOserver_set_winsize(wkind, sz) result(status)  !  set shared memory area sizes
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  integer(C_SIZE_T), intent(IN) :: sz        ! shared memory area size
  logical :: status                          ! true if there was an error
!! F_EnD

  status = server_set_winsize(wkind, sz)

  return
!! F_StArT
end function IOserver_set_winsize
!!
!! F_EnD

!! F_StArT
function IOserver_get_winsize(wkind) result(sz)  !  get shared memory area sizes
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  integer(C_SIZE_T)             :: sz
!! F_EnD
  sz = server_get_winsize(wkind)

  return
!! F_StArT
end function IOserver_get_winsize
!!
!! F_EnD

!! F_StArT
function IOserver_get_win_ptr(wkind) result(pt)  !  get shared memory area address
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_PTR
  implicit none
  integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
  type(C_PTR)                   :: pt
!! F_EnD
  pt = server_get_winptr(wkind)

  return
!! F_StArT
end function IOserver_get_win_ptr
!!
!! F_EnD

!! F_StArT
function IOserver_set_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
  logical :: status
!! F_EnD
  status = server_set_winsize(IO_BASE, sz_base)
  if(status) return

  status = server_set_winsize(IO_RELAY, sz_relay)
  if(status) return

  status = server_set_winsize(IO_SERVER, sz_server)
  if(status) return
  
  return
!! F_StArT
end function IOserver_set_winsizes
!!
!! F_EnD

!! F_StArT
subroutine IOserver_get_winsizes(sz_base, sz_relay, sz_server) !  get communication window / shared memory area sizes
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(OUT) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  integer(C_SIZE_T), intent(OUT) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  integer(C_SIZE_T), intent(OUT) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
!! F_EnD

  sz_base   = server_get_winsize(IO_BASE)
  sz_relay  = server_get_winsize(IO_RELAY)
  sz_server = server_get_winsize(IO_SERVER)
  
  return
!! F_StArT
end subroutine IOserver_get_winsizes
!!
!! F_EnD

!! F_StArT
subroutine IOserver_get_winmem(p_base, p_relay, p_server) !  get communication window / shared memory area addresses
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!! import :: C_PTR
  implicit none
  type(C_PTR), intent(OUT) :: p_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  type(C_PTR), intent(OUT) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  type(C_PTR), intent(OUT) :: p_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
!! F_EnD
  integer(C_INTPTR_T) :: tmp

  p_base   = server_get_winptr(IO_BASE)
  p_relay  = server_get_winptr(IO_RELAY)
  p_server = server_get_winptr(IO_SERVER)
  if(debug_mode) then
    if(C_ASSOCIATED(p_base))         print 1,'DEBUG: alliomem   is DEFINED',transfer(p_base,tmp),' size =',alliosiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_base))   print *,'DEBUG: alliomem   is NULL'
    if(C_ASSOCIATED(p_relay))        print 1,'DEBUG: relaymem  is DEFINED',transfer(p_relay,tmp),' size =',relaysiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_relay))  print *,'DEBUG: relaymem  is NULL'
    if(C_ASSOCIATED(p_server))       print 1,'DEBUG: servermem is DEFINED',transfer(p_server,tmp),' size =',serversiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_server)) print *,'DEBUG: servermem is NULL'
  endif

1 format(1X,A,2X,Z16.16,A,I10,A)
  return
!! F_StArT
end subroutine IOserver_get_winmem
!!
!! F_EnD

!! F_StArT
subroutine set_IOserver_global_comm(comm)   !  set global communicator to other than MPI_COMM_WORLD
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
  implicit none
  integer, intent(IN) :: comm
!! F_EnD
  global_comm = comm
  return
!! F_StArT
end subroutine set_IOserver_global_comm
!!
!! F_EnD

!! F_StArT
subroutine set_IOserver_relay(fn)  ! set relay function to call to fn
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
  implicit none
  external :: fn
!! F_EnD
  io_relay_fn = C_FUNLOC(fn)
  return
!! F_StArT
end subroutine set_IOserver_relay
!!
!! F_EnD

!! F_StArT
subroutine set_IOserver_server(fn)  ! set server function to call to fn
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
  implicit none
  external :: fn
!! F_EnD
  io_server_fn = C_FUNLOC(fn)
  return
!! F_StArT
end subroutine set_IOserver_server
!!
!! F_EnD

subroutine IOserver_noop()  !  NO OP loop to park processes with minimal CPU consumption
  use ioserver_internal_mod
  implicit none
  integer :: ierr

  interface
    function sleep(nsec) result(left) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: nsec
      integer(C_INT) :: left
    end function sleep
  end interface

  if(debug_mode) print *,'DEBUG: NO-OP process, global rank =',global_rank
  do while(0 == IOserver_is_time_to_quit())    ! sleep loop until quit flag appears
    if(debug_mode) print *,'MSG: SLEEP LOOP'
    ierr = sleep(1)
  enddo
  write(6,'(A,(15I5))')' DEBUG: colors =',mem % pe(0:max_smp_pe) % color
  write(6,*)'FINAL:, NO-OP',global_rank
  call MPI_Finalize(ierr)
  stop
end subroutine IOserver_noop

!! F_StArT
function IOserver_int_finalize() result(status)
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
  integer :: status
!! F_EnD
   ! code to be added later 
   status = 0
!! F_StArT
end function IOserver_int_finalize
!!
!! F_EnD

! function IOserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, app_class) result(status)
!   integer, intent(OUT) :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
!   integer, intent(OUT) :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
!   integer, intent(OUT) :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
!   integer, intent(OUT) :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
!   integer, intent(OUT) :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
!   integer, intent(OUT) :: nodecom      ! communicator for io server PEs on a node   (may be MPI_COMM_NULL)
!! F_StArT
function IOserver_int_init(nio_node, app_class) result(status)
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!!import :: C_FUNPTR
  implicit none
  integer :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
  integer :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
  integer :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
  integer :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
  integer :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
  integer :: nodecom      ! communicator for io server PEs on a node   (may be MPI_COMM_NULL)
  integer, intent(IN)  :: nio_node     ! number of relay processes per compute SMP node (1 or 2)
  character(len=*), intent(IN) :: app_class
  integer :: status
!! F_EnD
  character(len=8) :: heap_name, cioin_name, cioout_name
  integer :: color, temp_comm, ierr, temp, iocolor, errors, total_errors
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  procedure(), pointer :: p
  integer, dimension(:), pointer :: f_win_base
  type(C_PTR) :: c_win_base, temp_ptr
  integer(C_INT64_T) :: shmsz64
  integer :: sz32
  logical :: ok
#include <io-server/iso_c_binding_extras.hf>

  interface
    function sleep(nsec) result(left) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: nsec
      integer(C_INT) :: left
    end function sleep
  end interface

  ! 2 (model or server PE) or 3 (relay PE) of these 5 communicators will be true upon return from subroutine
  model    = MPI_COMM_NULL  ! preset all communicators to the NULL communicator
  modelio  = MPI_COMM_NULL
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
  nodecom  = MPI_COMM_NULL
  status   = NO_COLOR       ! precondition for failure
  errors   = 0              ! none so far

  call MPI_Initialized(initialized, ierr)      ! is MPI library already initialized ?
  if(.not. initialized) call MPI_Init(ierr)    ! initialize MPI if not already done
  call MPI_Comm_rank(global_comm, global_rank, ierr)
  call MPI_Comm_size(global_comm, global_size, ierr)

  color = NO_COLOR                             ! invalid
  if(app_class(1:1) == 'M') color = MODEL_COLOR  + RELAY_COLOR    ! model or relay app
  if(app_class(1:1) == 'S') color = SERVER_COLOR + INOUT_COLOR    ! IO server app (both input and output)
  if(app_class(1:1) == 'O') color = OUTPUT_COLOR + SERVER_COLOR   ! IO server app (output only)
  if(app_class(1:1) == 'I') color = INPUT_COLOR  + SERVER_COLOR   ! IO server app (input only)
  if(app_class(1:1) == 'Z') color = NO_OP_COLOR                   ! null app
  if(color == NO_COLOR) return                                    ! miserable failure

  ! split by node, all PEs (the result communicator is temporary and not kept)
  call MPI_Comm_split_type(global_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, smp_comm ,ierr)
  call MPI_Comm_rank(smp_comm, smp_rank, ierr)                  ! rank on SMP node
  call MPI_Comm_size(smp_comm, smp_size, ierr)                  ! population of SMP node

  ! allocate shared memory segment used for control, communicator = smp_comm
  call RPN_MPI_Win_allocate_shared(ctrlsiz, disp_unit, MPI_INFO_NULL, smp_comm, win_base, ctrlwin, ierr)

!   if(debug_mode) print *,'DEBUG: ctrlsiz, smp_rank, ierr =',ctrlsiz, smp_rank, ierr
  ! ctrlwin is no longer used after this point
  ctrlmem = transfer(win_base, C_NULL_PTR)   ! ctrlmem points to control shared memory segment
  call ioserver_memory_mod_init(ctrlmem, smp_size)     ! main control structure will point to shared memory at ctrlmem
  mem % pe(smp_rank) % ctrl_ra = ctrlmem               ! local address of control memory segment

  if(smp_rank == 0) mem % time_to_quit = 0   ! initialize quit flag to "DO NOT QUIT"
  call MPI_barrier(global_comm, ierr)               ! wait until control area initialization is done everywhere

  ! split global communicator into : server+model+relay / no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node (a deadlock will happen if this condition is not respected)
  ! color/NO_OP_COLOR has value 1 if NO-OP, 0 otherwise (NO_OP_COLOR MUST BE THE LARGEST CODE VALUE)
  call mpi_comm_split(global_comm, color/NO_OP_COLOR, global_rank, all_comm, ierr)


  if(color == NO_OP_COLOR) then       ! this is a NO-OP process, enter wait loop for finalize
    mem % pe(smp_rank) % color = NO_OP_COLOR
    call IOserver_noop()              ! this subroutine will never return and call finalize
    call MPI_Finalize(ierr)           ! IOserver_noop should never return, but ... in case it does
    stop
  endif

  ! split the all useful communicator (all_comm) into : server / model+relay
  iocolor = color
  if( iand(color,SERVER_COLOR) == SERVER_COLOR) iocolor = SERVER_COLOR  ! treat INPUT and OUTPUT server PEs the same way
  call mpi_comm_split(all_comm, iocolor, global_rank, temp_comm, ierr)  ! temp_comm is either server_comm or modelio_comm

! ===================================================================================
! at this point we only have "active" PEs (model, relay, server)
! allocate node local shared memory used for intra node communications
! ===================================================================================
  if(debug_mode) temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/relay/server nodes in debug mode

  if( iand(color,SERVER_COLOR) == SERVER_COLOR) then
  ! =========================================================================
  ! ============================ IO server process ==========================
  ! =========================================================================

    server_comm = temp_comm             ! communicator for the "io server(s)"
    serverio = server_comm              ! output argument
    ! split server_comm by node, PEs on same SMP node
    call MPI_Comm_split_type(server_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, servercom ,ierr)
    nodecom  = servercom                              ! server processes on same SMP node
    relaycom = MPI_COMM_NULL

    call MPI_Comm_rank(servercom, serverrank, ierr)   ! rank on SMP node
    call MPI_Comm_size(servercom, serversize, ierr)   ! population of SMP node
    ! for now, relaysize should be equal to serverio_size

    ! allocate shared memory used for intra node communication between server PEs and 1 sided get/put with relay PEs
!     if(debug_mode) print *,'DEBUG: before MPI_Win_allocate_shared serverwin, size, rank =',serversiz, serverrank
    call RPN_MPI_Win_allocate_shared(serversiz, disp_unit, MPI_INFO_NULL, servercom, win_base, serverwin, ierr)
!     if(debug_mode) print *,'DEBUG: after MPI_Win_allocate_shared serverwin, size, rank, err =',serversiz, serverrank, ierr
    if(ierr == MPI_SUCCESS) then
!       if(debug_mode) print *,'DEBUG: serverwin query, ierr =', ierr, serversiz
      servermem = transfer(win_base, C_NULL_PTR)        ! base address 
      winsize = serversiz
    else
      print *,"ERROR: IO server processes failed to allocate shared memory"
      servermem = C_NULL_PTR
!       errors = errors + 1
      goto 2                ! go immediately to error processing
    endif
    ! serverwin will never be used after this point, all we are interested in is the shared memory

  else
  ! =========================================================================
  ! ======================= model compute or IO relay process ===============
  ! =========================================================================

    modelio_comm = temp_comm              ! communicator for "model compute and io relay" PEs
    modelio      = modelio_comm
    ! split modelio_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, relaycom ,ierr)
    nodecom   = relaycom                            ! compute and relay processes on same SMP node
    servercom = MPI_COMM_NULL

    call MPI_Comm_rank(relaycom, relayrank, ierr)   ! rank on SMP node
    call MPI_Comm_size(relaycom, relaysize, ierr)   ! population of SMP node

    ! allocate shared memory window used for intra node communication between model and relay PEs
    call RPN_MPI_Win_allocate_shared(relaysiz, disp_unit, MPI_INFO_NULL, relaycom, win_base, relaywin, ierr)
    if(debug_mode) write(6,*)'DEBUG: after MPI_Win_allocate_shared relaywin, size, rank =',relaysiz,relayrank
    call flush(6)
    if(ierr == MPI_SUCCESS) then
      relaymem = transfer(win_base, C_NULL_PTR)        ! base address , integer -> C pointer
    else
      print *,"ERROR: relay processes failed to allocate shared memory"
      relaymem = C_NULL_PTR
!       errors = errors + 1
      goto 2                ! go immediately to error processing
    endif
    ! relaywin will not be used after this point (especially as RPN_MPI_Win_allocate_shared sets it to MPI_COMM_NULL)

    if(relayrank == 0) then        ! PE with rank on node == 0 creates the memory arena
      shmsz64 = relaysiz
      temp_ptr = local_arena % create(relaymem, 128, shmsz64)
    else
      temp_ptr = local_arena % clone(relaymem)   !  cloning is O.K. even if arena is not initialized
    endif
    local_arena_ptr = local_arena % addr()
    mem % pe(smp_rank) % io_ra = local_arena_ptr               ! local address of arena segment

    ! spread the nio_node relay PEs across the node (lowest and highest node ranks)
    if(relayrank >= (nio_node/2) .and. relayrank < (relaysize - ((nio_node+1)/2))) then   ! model compute process
      color  = MODEL_COLOR
      if(debug_mode) &
          print *,'DEBUG: model compute process, node rank =', &
                  relayrank,relaysize,nio_node/2,(relaysize - ((nio_node+1)/2))
    else                                                                                     ! relay IO process
      color = RELAY_COLOR  ! relay IO process
      if(debug_mode) print *,'DEBUG: IO relay process, node rank =',relayrank,relaysize,nio_node/2,(relaysize - ((nio_node+1)/2))
      ! not much memory is needed on the relay PEs for the remote IO window, allocate it
      winsize = alliosiz           ! 1 sided window size for relay <-> server PEs on relay PEs
      call MPI_Alloc_mem(winsize, MPI_INFO_NULL, win_base, ierr)     ! allocate memory through MPI library for 1 sided get/put with server PEs
      if(ierr .ne. MPI_SUCCESS) errors = errors + 1                  ! flag error (it will be intercepted later)
    endif

    mem % pe(smp_rank) % color = color                                   ! store color of this PE in shared memory table
    call MPI_Comm_split(relaycom, color, relayrank, temp_comm, ierr)      ! split into compute and IO relay
    if(color == RELAY_COLOR) then
      relay_smp_comm = temp_comm    ! relay PEs on same SMP node
    else
      model_smp_comm = temp_comm    ! compute PEs on same SMP node
    endif
    call MPI_Comm_rank(temp_comm, mem % pe(smp_rank)%rank, ierr)  ! rank on node in my color
    if(debug_mode) print *,'DEBUG: rank',mem % pe(smp_rank)%rank,' in color',color

    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)    ! split into model compute and IO relay processes

    ! wait for memory arena to be initialized by rank 0 before allocating heap and circular buffer(s)
    call MPI_Barrier(relaycom, ierr)

    if(color == RELAY_COLOR) then                            ! io relay processes
    ! =========================================================================
    ! ================================ IO relay process =======================
    ! =========================================================================
      iorelay_comm  = temp_comm                                ! for internal module
      nodeio        = iorelay_comm                             ! output argument
      shmsz64 = relaysiz / 4                                 ! size in 32 bit units of relay shared memory area
      shmsz64 = shmsz64 / relaysize                          ! available size per PE on SMP node

      !  allocate nominal heap and circular buffers (8K elements)
      ! 85%  of per PE size for the heap
      write(heap_name  ,'(A5,I3.3)') "RHEAP",mem % pe(smp_rank)%rank
      sz32 = shmsz64 * 0.85
      temp_ptr = local_arena % newblock(sz32, heap_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      temp_ptr = local_heap % create(temp_ptr, sz32)         ! allocate local heap
      temp     = local_heap % set_default()                  ! make local_heap the default heap
      call local_heap % set_base( local_arena % addr() )     ! set arena address as offset base for heap
      mem % pe(smp_rank) % heap = Pointer_offset(local_arena % addr() , temp_ptr, 1)    ! offset of my heap in memory arena

      !  4%  of per PE size for inbound circular buffer
      write(cioin_name ,'(A4,I4.4)') "RCIO", mem % pe(smp_rank)%rank
      sz32 = shmsz64 * 0.04
      temp_ptr = local_arena % newblock(sz32, cioin_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      ok = local_cio_in % create(temp_ptr, sz32)
      if( .not. ok ) goto 2                                  ! cio creation failed
      mem % pe(smp_rank) % cio_in = Pointer_offset(local_arena % addr() , temp_ptr, 1)  ! offset of my inbound CIO in memory arena

      !  10%  of per PE size for outbound circular buffer
      write(cioout_name,'(A4,I4.4)') "RCIO", mem % pe(smp_rank)%rank + 1000
      sz32 = shmsz64 * 0.1
      temp_ptr = local_arena % newblock(sz32, cioout_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      ok = local_cio_out % create(temp_ptr, sz32)
      if( .not. ok ) goto 2                                  ! cio creation failed
      mem % pe(smp_rank) % cio_out = Pointer_offset(local_arena % addr() , temp_ptr, 1)  ! offset of my outbound CIO in memory arena

    else                                                     ! compute processes
    ! =========================================================================
    ! ============================ model compute process ======================
    ! =========================================================================
      model_comm = temp_comm                                 ! for internal module
      model      = model_comm                                ! output argument
      shmsz64 = relaysiz / 4                                 ! size in 32 bit units of relay shared memory area
      shmsz64 = shmsz64 / relaysize                          ! available size per PE on SMP node

      ! will also need rank on node for compute in rat table
      !  pe() % relay_ra = local_arena % addr()

      ! 85%  of per PE size for the heap
      write(heap_name  ,'(A5,I3.3)') "MHEAP",mem % pe(smp_rank)%rank
      sz32 = shmsz64 * 0.85
      temp_ptr = local_arena % newblock(sz32, heap_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      if(debug_mode) call print_created(temp_ptr, heap_name, sz32)
      temp_ptr = local_heap % create(temp_ptr, sz32)
      temp     = local_heap % set_default()                  ! make local_heap the default heap
      call local_heap % set_base( local_arena % addr() )     ! set arena address as offset base for heap
      mem % pe(smp_rank) % heap = Pointer_offset(local_arena % addr() , temp_ptr, 1)    ! offset of my heap in memory arena

      !  4%  of per PE size for relay -> compute circular buffer
      write(cioin_name ,'(A4,I4.4)') "MCIO", mem % pe(smp_rank)%rank
      sz32 = shmsz64 * 0.04
      temp_ptr = local_arena % newblock(sz32, cioin_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      if(debug_mode) call print_created(temp_ptr, cioin_name, sz32)
      ok = local_cio_in % create(temp_ptr, sz32)
      if( .not. ok ) goto 2                                  ! cio creation failed
      mem % pe(smp_rank) % cio_in = Pointer_offset(local_arena % addr() , temp_ptr, 1)  ! offset of my inbound CIO in memory arena

      ! 10%  of per PE size for compute -> relay circular buffer
      write(cioout_name,'(A4,I4.4)') "MCIO", mem % pe(smp_rank)%rank + 1000
      sz32 = shmsz64 * 0.1           ! 10% for outbound circular buffer
      temp_ptr = local_arena % newblock(sz32, cioout_name)
      if( .not. C_ASSOCIATED(temp_ptr) ) goto 2              ! allocation failed
      if(debug_mode) call print_created(temp_ptr, cioout_name, sz32)
      ok = local_cio_out % create(temp_ptr, sz32)
      if( .not. ok ) goto 2                                  ! cio creation failed
      mem % pe(smp_rank) % cio_out = Pointer_offset(local_arena % addr() , temp_ptr, 1)  ! offset of my outbound CIO in memory arena
    endif

    if(debug_mode) write(6,*)'DEBUG: allocated '//heap_name//' '//cioin_name//' '//cioout_name
    if(debug_mode) write(6,'(A,3Z18.16)') ' DEBUG: displacements =', &
                     mem % pe(smp_rank) % heap, mem % pe(smp_rank) % cio_in, mem % pe(smp_rank) % cio_out
    call flush(6)
  endif   ! (color == SERVER_COLOR)
  goto 3       ! no error, bypass
2 continue     ! go here upon error in memory allocation
  errors = errors + 1
3 continue     ! no error
! ===================================================================================
!                              GLOBAL ERROR CHECK 
! ===================================================================================
  call MPI_Allreduce(errors, total_errors, 1, MPI_INTEGER, MPI_SUM, all_comm, ierr)
  if(debug_mode) print *,'INFO: allocation error(s) detected =',total_errors
  call flush(6)
!   total_errors = 1
  if(total_errors > 0) then
    print *,'FATAL:',total_errors,' error(s) detected while allocating memory'
    call IOserver_set_time_to_quit()      ! tell NO-OP PEs to quit
    call MPI_Finalize(ierr)
    stop
  endif
  call build_relay_model_index()
  if(debug_mode .and. (color == RELAY_COLOR .or. color == MODEL_COLOR)) call build_print_model_index()
! split IO PEs into server and relay PEs, create 1 sided communication window
! set return code to appropriate value or call supplied relay subroutine
! ===================================================================================
  if(model_comm == MPI_COMM_NULL) then    ! not compute PE, IO server or IO relay PE
    color = RELAY_COLOR + SERVER_COLOR
  else
    color = MODEL_COLOR                   ! model compute PE
  endif

  call MPI_Comm_split(all_comm, color, global_rank, temp_comm, ierr)  ! split all_comm into model and IO (relay+server) processes

  if(color == MODEL_COLOR) then                 ! model compute PE
    status = MODEL_COLOR
  else                                          ! IO relay or IO server

    alliocom  = temp_comm                       ! all IO processes
    call mpi_comm_size(alliocom, alliosize, ierr)
    allio     = alliocom
    if(server_comm .ne. MPI_COMM_NULL) then     ! IO server process on separate node(s)
      status = SERVER_COLOR                     ! IO server PE
    else
      status = RELAY_COLOR                      ! IO relay PE
    endif

    allocate(iocolors(0:alliosize))             ! collect colors for IO PEs, index in array is rank in alliocom
    color = status
    call MPI_Allgather(color, 1, MPI_INTEGER, iocolors, 1, MPI_INTEGER, alliocom, ierr)
    if(debug_mode) write(6,'(A,10I8,(/19X,10I8))') ' DEBUG: IO colors =',iocolors(0:alliosize-1)

    ! winsize and win_base have been obtained previously, allocate 1 sided window for relay and server PEs
    c_win_base = transfer(win_base, c_win_base)
    call C_F_POINTER(c_win_base, f_win_base, [winsize/4])   !  make honest Fortran pointer (f_win_base) for call to MPI_Win_create
    call MPI_Win_create(f_win_base, winsize, disp_unit, MPI_INFO_NULL, alliocom, alliowin, ierr)
    alliomem = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
    alliosiz = winsize                           ! alliomem same as servermem for server PEs

  endif

  ! ===================================================================================
  !                     SERVER processes (no return to caller)
  ! ===================================================================================
  if(server_comm .ne. MPI_COMM_NULL) then

    if(C_ASSOCIATED(io_server_fn)) then
      if(debug_mode) print *,'INFO: io_server_fn is associated'
      call C_F_PROCPOINTER(io_server_fn,p)    ! associate procedure pointer with caller supplied address
      ! call user supplied server code that may or may not return
      write(6,*)'INFO: expecting no return from io_server_fn'
      call p()    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized
    else
      if(debug_mode) print *,'INFO: io_server_fn is not associated, calling default function'
      call io_server_out()                      ! not expected to return, but ...
    endif

    call IOserver_set_time_to_quit()          ! activate quit signal for NO-OP PEs
    write(6,*)'FINAL: SMP node PE',smp_rank+1,' of',smp_size
    call MPI_Finalize(ierr)                   ! DO NOT return to caller, call finalize, then stop
    stop
  endif

  ! ===================================================================================
  !                     RELAY processes (no return to caller if io_relay_fn is defined)
  ! ===================================================================================
  if(iorelay_comm .ne. MPI_COMM_NULL) then       ! IO relay process, check if caller supplied relay routine

    if(C_ASSOCIATED(io_relay_fn)) then             ! caller supplied subroutine to be called on relay PEs
      if(debug_mode) print *,'INFO: io_relay_fn is associated'
      call C_F_PROCPOINTER(io_relay_fn,p)          ! associate procedure pointer with caller supplied address

      ! call user supplied relay code that may or may not return
      write(6,*)'INFO: no return from io_relay_fn'
      call p()    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call IOserver_set_time_to_quit()          ! activate quit signal for NO-OP PEs
      write(6,*)'FINAL: model+io node PE',relayrank+1,' of',relaysize
      call MPI_Finalize(ierr)                   ! DO NOT return to caller, call finalize, then stop
      stop

    else                                        ! IO relay process on model node

      if(debug_mode) print *,'INFO: io_relay_fn is not associated'
      status = RELAY_COLOR                      ! IO relay, back to caller, with relay status code, caller will call relay subroutine
    endif
  endif

  return
!! F_StArT
end function IOserver_int_init
!!
!! F_EnD
! same interface as MPI_Win_allocate_shared
! disp_unit_, info, win are not meaningful for this procedure
! disp_unit_ must be 0, info must be MPI_INFO_NULL, win is returned as MPI_WIN_NULL
subroutine RPN_MPI_Win_allocate_shared(wsize, disp_unit_, info, comm, baseptr, win, ierror)
  USE ISO_C_BINDING
  use ioserver_internal_mod
  implicit none
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: wsize
  INTEGER, INTENT(IN) :: disp_unit_, info, comm
  INTEGER, INTENT(OUT) :: win, ierror
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: baseptr

  integer :: myrank, shmid
  integer(C_INT64_T) :: siz
  type(C_PTR) :: p

  baseptr = 0
  win = MPI_WIN_NULL
  ierror = MPI_ERROR
  if(info .ne. MPI_INFO_NULL) return
  if(disp_unit_ == 0) return

  call MPI_Comm_rank(comm, myrank, ierror)
  if(myrank == 0) then
    siz = wsize
    p = memory_allocate_shared(shmid, siz)
  endif
  call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, comm, ierror)
  if(myrank .ne. 0) then
    p = memory_address_from_id(shmid)
  endif
  baseptr = transfer(p, baseptr)
  ierror = MPI_SUCCESS
end subroutine RPN_MPI_Win_allocate_shared
!! F_StArT
!! end interface
!! F_EnD
