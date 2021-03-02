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
module ioserver_internal_mod
  use ISO_C_BINDING
  use shmem_heap
  use memory_arena_mod
  use circular_buffer_module
  implicit none
  include 'mpif.h'
!! F_StArT
!!
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
!!
!! F_EnD

  type :: ctrl_shared_memory
    integer :: time_to_quit = 0
  end type

  save
  !  ==========================================================================================
  !                        shared memory areas , addresses, communicators, windows
  !  ==========================================================================================

  integer :: disp_unit     = 4                   !  displacement unit in 1 sided windows (32 bits by default)

  ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
  type(C_PTR)         :: ctrlmem   = C_NULL_PTR     !  address of main (control) shared memory area on this node
  integer(C_INTPTR_T) :: ctrlsiz   = MBYTE          !  size of above
  integer             :: ctrlwin   = MPI_WIN_NULL   !  NEVER USED for communications

  ! used by relay PEs for 1 sided get/put, used as shared memory by server PEs
  type(C_PTR)         :: basemem   = C_NULL_PTR     !  address of IO window (on all IO PEs) (same as servermem on server PEs)
  integer(C_INTPTR_T) :: basesiz   = MBYTE          !  size of above
  integer             :: basewin   = MPI_WIN_NULL   !  1 sided window used by RELAY + SERVER PEs
  integer             :: basecom   = MPI_COMM_NULL  !  all IO PEs (model IO + IO server) (subset of all_comm)
  integer             :: baserank  = -1             !  rank in basecom
  integer             :: basesize  = 0              !  population of basecom

  ! information for compute and relay PEs on a given SMP node
  ! shared memory used for heaps and circular buffers on a given SMP node
  ! this memory is used for cummunications between relay and model PEs
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

  ! communicators
  integer :: global_comm   = MPI_COMM_WORLD      !  MPI "WORLD" for this set of PEs
  integer :: global_rank   = -1                  !  rank in global_comm

  integer :: smp_comm      = MPI_COMM_NULL       !  PEs on this SMP node        (any kind ) (subset of global_comm)
  integer :: smp_rank      = -1                  !  rank in smp_comm
  !  ==========================================================================================
  !                        communicators
  !  ==========================================================================================
  ! global_comm  = all_comm + NO-OP PEs
  ! all_comm     = modelio_comm + server_comm
  ! modelio_comm = model_comm   + relay_comm
  integer :: all_comm      = MPI_COMM_NULL       !  non NO-OP PEs               (all nodes) (subset of global_comm)
  integer :: modelio_comm  = MPI_COMM_NULL       !  model compute and relay PEs (all nodes) (subset of all_comm)
  integer :: model_comm    = MPI_COMM_NULL       !  model compute PEs           (all nodes) (subset of all_comm and modelio_comm)
  integer :: relay_comm    = MPI_COMM_NULL       !  relay PEs                   (all nodes) (subset of all_comm and modelio_comm)
  integer :: server_comm   = MPI_COMM_NULL       !  IO server PEs               (subset of all_comm)

  !  ==========================================================================================
  !                        miscellaneous
  !  ==========================================================================================
  logical :: debug_mode = .false.

  type(memory_arena)     :: local_arena
  type(heap)             :: local_heap
  type(circular_buffer)  :: local_cio_in
  type(circular_buffer)  :: local_cio_out

  type(C_FUNPTR) :: io_relay_fn = C_NULL_FUNPTR  !  procedure to call on relay processes (if not NULL)

  contains

!! F_StArT
!! interface
!!
!! F_EnD

!! F_StArT
subroutine IOserver_set_time_to_quit() BIND(C,name='IOserver_set_time_to_quit')   ! set time to quit flag in control area
!! F_EnD
  implicit none
  type(ctrl_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  m%time_to_quit = 1
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
  type(ctrl_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  status = m%time_to_quit
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
      if(sz > KBYTE * 32) then
        ctrlsiz = sz                   ! control memory (normally very small)
        status = .false.
      endif
    case(IO_BASE)
      if(sz > MBYTE) then
        basesiz = sz                   ! 1 sided window on relay PEs
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
      sz = basesiz
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
      ptr = basemem
    case(IO_RELAY)
      ptr = relaymem
    case(IO_SERVER)
      ptr = servermem
    case DEFAULT
      ptr = C_NULL_PTR
  end select
  return
end function server_get_winptr

end module ioserver_internal_mod


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
    if(C_ASSOCIATED(p_base))         print 1,'DEBUG: basemem   is DEFINED',transfer(p_base,tmp),' size =',basesiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_base))   print *,'DEBUG: basemem   is NULL'
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
!! F_StArT
end function IOserver_int_finalize
!!
!! F_EnD

!! F_StArT
function IOserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, app_class) result(status)
!! F_EnD
  use ioserver_internal_mod
!! F_StArT
!!import :: C_FUNPTR
  implicit none
  integer, intent(OUT) :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
  integer, intent(OUT) :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
  integer, intent(OUT) :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
  integer, intent(OUT) :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
  integer, intent(OUT) :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
  integer, intent(OUT) :: nodecom      ! communicator for io server PEs on a node   (may be MPI_COMM_NULL)
  integer, intent(IN)  :: nio_node     ! number of relay processes per compute node
  character(len=*), intent(IN) :: app_class
  integer :: status
!! F_EnD
  integer :: color, temp_comm, ierr, temp, iocolor
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  type(ctrl_shared_memory), pointer :: main
  procedure(), pointer :: p
  integer, dimension(:), pointer :: f_win_base
  type(C_PTR) :: c_win_base
#include <iso_c_binding_extras.hf>

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

  call MPI_Initialized(initialized, ierr)      ! is MPI library already initialized ?
  if(.not. initialized) call MPI_Init(ierr)    ! initialize MPI if not already done
  call MPI_Comm_rank(global_comm, global_rank, ierr)

  color = -1                             ! invalid
  if(app_class(1:1) == 'M') color = MODEL_COLOR  + RELAY_COLOR    ! model or relay app
  if(app_class(1:1) == 'S') color = SERVER_COLOR + INOUT_COLOR    ! IO server app (both input and ouptut)
  if(app_class(1:1) == 'O') color = OUTPUT_COLOR + SERVER_COLOR   ! IO server app (output only)
  if(app_class(1:1) == 'I') color = INPUT_COLOR  + SERVER_COLOR   ! IO server app (input only)
  if(app_class(1:1) == 'Z') color = NO_OP_COLOR                   ! null app
  if(color == -1) return                                          ! miserable failure

  ! split by node, all PEs (the result communicator is temporary and not kept)
  call MPI_Comm_split_type(global_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, smp_comm ,ierr)
  call MPI_Comm_rank(smp_comm, smp_rank, ierr)                  ! rank on SMP node

  ! allocate shared memory segment used for control, communicator = smp_comm
  call RPN_MPI_Win_allocate_shared(ctrlsiz, disp_unit, MPI_INFO_NULL, smp_comm, win_base, ctrlwin, ierr)

  if(debug_mode) print *,'DEBUG: ctrlsiz, smp_rank, ierr =',ctrlsiz, smp_rank, ierr
  ! ctrlwin is no longer used after this query
  ctrlmem = transfer(win_base, C_NULL_PTR)
  call C_F_POINTER(ctrlmem, main)                ! main control structure points to shared memory at ctrlmem

  if(smp_rank == 0) main%time_to_quit = 0      ! initialize quit flag to "DO NOT QUIT"
  call MPI_barrier(global_comm, ierr)            ! wait until control area initialization is done everywhere

  ! split global communicator into : server+model+relay / no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node (a deadlock will happen if this condition is not respected)
  ! color/NO_OP_COLOR has value 1 if NO-OP, 0 otherwise (NO_OP_COLOR MUST BE THE LARGEST CODE VALUE)
  call mpi_comm_split(global_comm, color/NO_OP_COLOR, global_rank, all_comm, ierr)

  if(color == NO_OP_COLOR) then       ! this is a NO-OP process, enter wait loop for finalize
    call IOserver_noop()              ! this subroutine will never return and call finalize
    return                            ! should never happen
  endif                                ! this is an active process (server/model/relay)

  ! split the all useful communicator (all_comm) into : server / model+relay
  iocolor = color
  if( iand(color,SERVER_COLOR) == SERVER_COLOR) iocolor = SERVER_COLOR  ! treat INPUT and OUTPUT server PEs the same way
  call mpi_comm_split(all_comm, iocolor, global_rank, temp_comm, ierr)  ! temp_comm is either server_comm or modelio_comm

! ===================================================================================
! at this point we only have "active" PEs (model, relay, server)
! allocate node local shared memory used for intra node communications
! ===================================================================================
  if(debug_mode) temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/relay/server nodes in debug mode

  if( iand(color,SERVER_COLOR) == SERVER_COLOR) then          ! IO server process

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
    if(debug_mode) print *,'DEBUG: before MPI_Win_allocate_shared serverwin, size, rank =',serversiz, serverrank
    call RPN_MPI_Win_allocate_shared(serversiz, disp_unit, MPI_INFO_NULL, servercom, win_base, serverwin, ierr)
    if(debug_mode) print *,'DEBUG: after MPI_Win_allocate_shared serverwin, size, rank, err =',serversiz, serverrank, ierr
    if(ierr == MPI_SUCCESS) then
      if(debug_mode) print *,'DEBUG: serverwin query, ierr =', ierr, serversiz
      servermem = transfer(win_base, C_NULL_PTR)        ! base address 
      winsize = serversiz
    else
      print *,"ERROR: IO server processes failed to allocate shared memory"
      servermem = C_NULL_PTR
    endif
    ! serverwin will never be used after this point, all we are interested in is the shared memory

  else                                    ! model compute or IO relay process

    modelio_comm = temp_comm              ! communicator for "model compute and io relay" PEs
    modelio      = modelio_comm
    ! split modelio_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, relaycom ,ierr)
    nodecom   = relaycom                            ! compute and relay processes on same SMP node
    servercom = MPI_COMM_NULL

    call MPI_Comm_rank(relaycom, relayrank, ierr)   ! rank on SMP node
    call MPI_Comm_size(relaycom, relaysize, ierr)   ! population of SMP node

    ! allocate shared memory window used for intra node communication between model and relay PEs
!     if(relayrank == 0) winsize  = relaysiz           ! size is supplied by node rank 0
    if(debug_mode) print *,'DEBUG: before MPI_Win_allocate_shared relaywin, size, rank =',relaysiz,relayrank
    call RPN_MPI_Win_allocate_shared(relaysiz, disp_unit, MPI_INFO_NULL, relaycom, win_base, relaywin, ierr)
    if(debug_mode) print *,'DEBUG: after MPI_Win_allocate_shared relaywin, size, rank =',relaysiz,relayrank
    if(ierr == MPI_SUCCESS) then
!       call MPI_Win_shared_query(relaywin, 0, relaysiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
      if(debug_mode) print *,'DEBUG: relaywin query, ierr =', ierr, serversiz
      relaymem = transfer(win_base, C_NULL_PTR)        ! base address 
    else
      print *,"ERROR: relay processes failed to allocate shared memory"
      relaymem = C_NULL_PTR
    endif
    ! relaywin will no longer be needed nor used after this point

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
      winsize = basesiz           ! 1 sided window size for relay <-> server PEs on relay PEs
      call MPI_Alloc_mem(winsize, MPI_INFO_NULL, win_base, ierr)     ! allocate memory through MPI library for 1 sided get/put with server PEs
      if(debug_mode) print *,'DEBUG: after MPI_Alloc_mem, rank, ierr, base =',smp_rank,ierr, win_base
    endif

    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)    ! split into model compute and IO relay processes

    if(color == RELAY_COLOR) then ! io relay processes
      relay_comm = temp_comm     ! for module
      nodeio      = relay_comm   ! output argument
    else                          ! compute processes
      model_comm = temp_comm      ! for module
      model      = model_comm     ! output argument
    endif

  endif   ! (color == SERVER_COLOR)
! ===================================================================================
! split IO PEs into server and relay PEs, create 1 sided communication windew
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

    basecom = temp_comm                       ! all IO processes
    allio     = basecom
    if(server_comm .ne. MPI_COMM_NULL) then   ! IO server process on separate node(s)
      status = SERVER_COLOR                     ! IO server
    endif

    ! winsize and win_base have been obtained previously, allocate 1 sided window for relay and server PEs
    call MPI_Barrier(basecom, ierr)
    if(debug_mode) print *,'DEBUG: before MPI_Win_create, rank , base, size, ierr', smp_rank,win_base,winsize, ierr
    c_win_base = transfer(win_base, c_win_base)
    call C_F_POINTER(c_win_base, f_win_base, [winsize/4])   !  make honest Fortran pointer for call to MPI_Win_create
    call MPI_Win_create(f_win_base, winsize, disp_unit, MPI_INFO_NULL, basecom, basewin, ierr)
    if(debug_mode) print *,'DEBUG: after MPI_Win_create, rank, ierr, base = smp_rank', smp_rank, ierr, win_base
    basemem = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
    basesiz = winsize                           ! basemem same as servermem for server PEs

  endif

  if(relay_comm .ne. MPI_COMM_NULL) then       ! IO relay process, check if caller supplied relay routine

    if(C_ASSOCIATED(io_relay_fn)) then             ! caller supplied subroutine to be called on relay PEs
      if(debug_mode) print *,'INFO: io_relay_fn is associated'
      call C_F_PROCPOINTER(io_relay_fn,p)          ! associate procedure pointer with caller supplied address

      ! call user supplied relay code
      call p(model, modelio, allio, nodeio, serverio, nodecom)    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call IOserver_set_time_to_quit()              ! activate quit signal for NO-OP PEs
      write(6,*)'FINAL:, relay PE',relayrank
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
