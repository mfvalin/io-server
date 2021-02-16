
!!  functions for C and FORTRAN programming
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
module ioserver_mod
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
!! F_StArT
!!
  integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
  integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

  integer, parameter :: NO_COLOR     = 1
  integer, parameter :: MODEL_COLOR  = 1
  integer, parameter :: RELAY_COLOR  = 2
  integer, parameter :: SERVER_COLOR = 4
  integer, parameter :: OUTPUT_COLOR = 8
  integer, parameter :: INPUT_COLOR  = 16
  integer, parameter :: INOUT_COLOR  = INPUT_COLOR + OUTPUT_COLOR
  integer, parameter :: NO_OP_COLOR  = 8192   ! MUST BE THE HIGHEST VALUE
!!
!! F_EnD

  type :: comm_rank_size
    integer :: comm = MPI_COMM_NULL
    integer :: rank = -1
    integer :: size = 0
  end type
  type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(MPI_COMM_NULL, -1, 0)

  type :: ctrl_shared_memory
    integer :: time_to_quit = 0
  end type

  save
  ! control memory, shared by all PEs on a given SMP node (whether active PEs or NO-OP PEs)
  type(C_PTR)         :: ctrlmem   = C_NULL_PTR     !  address of main (control) shared memory area on this node
  integer(C_INTPTR_T) :: ctrlsiz   = MBYTE          !  size of above
  integer             :: ctrlwin   = MPI_WIN_NULL   !  NEVER USED for communications

  ! used by relay PEs for 1 sided get/put, used as shared memory by server PEs
  type(C_PTR)         :: basemem   = C_NULL_PTR     !  address of IO window (on all IO PEs) (same as servermem on server PEs)
  integer(C_INTPTR_T) :: basesiz   = MBYTE          !  size of above
  integer             :: allio_win = MPI_WIN_NULL   !  1 sided window used by RELAY + SERVER PEs
  integer             :: allio_com = MPI_COMM_NULL  !  all IO PEs (model IO + IO server) (subset of all_comm)

  ! information for compute and relay PEs on a given SMP node
  ! shared memory used for heaps and circular buffers on a given SMP node
  ! this memory is used for cummunications between relay and model PEs
  type(C_PTR)         :: relaymem  = C_NULL_PTR     !  base address of relay+model PEs shared memory area
  integer(C_INTPTR_T) :: relaysiz  = GBYTE          !  size of above
  integer             :: relaywin  = MPI_WIN_NULL   !  NEVER USED for communications
  integer             :: node_com  = MPI_COMM_NULL  !  communicator for model + relay PEs on same SMP node
                                                    !  or               server PEs on same SMP node
  integer             :: node_rank = -1             !  rank in node_com
  integer             :: node_size = 0              !  population of node_com

  ! shared memory on io server node(s), used by io server PEs to communicate
  ! used for 1 sided communications window between relay and server PEs
  ! not much memory (if any) is normally allocated on relay PEs for this purpose
  type(C_PTR)         :: servermem = C_NULL_PTR     !  base address of io server PEs shared memory area
  integer(C_INTPTR_T) :: serversiz = GBYTE          !  size of above
  integer             :: serverwin  = MPI_WIN_NULL  !  NEVER USED for communications

  integer :: global_comm   = MPI_COMM_WORLD      !  MPI "WORLD" for this set of PEs
  integer :: global_rank   = -1                  !  rank in global_comm

  integer :: smp_comm      = MPI_COMM_NULL       !  PEs on this SMP node        (any kind ) (subset of global_comm)
  integer :: smp_rank      = -1                  !  rank in smp_comm

  integer :: all_comm      = MPI_COMM_NULL       !  non NO-OP PEs               (all nodes) (subset of global_comm)
  integer :: model_comm    = MPI_COMM_NULL       !  model compute PEs           (all nodes) (subset of all_comm)
  integer :: modelio_comm  = MPI_COMM_NULL       !  model compute and relay PEs (all nodes) (subset of all_comm)
  integer :: nodeio_comm   = MPI_COMM_NULL       !  relay PEs on model nodes    (all nodes) (subset of modelio_comm)
  integer :: serverio_comm = MPI_COMM_NULL       !  IO server PEs

  integer :: disp_unit     = 4                   !  displacement unit in 1 sided windows (32 bits by default)

  logical :: debug_mode = .false.

  type(C_FUNPTR) :: relay_fn = C_NULL_FUNPTR

  interface
    function sleep(nsec) result(ok) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), value :: nsec
      integer :: ok
    end function sleep
  end interface

  contains

!! F_StArT
!! interface
!!
!! F_EnD

!! F_StArT
subroutine IOSERVER_time_to_quit() BIND(C,name='IOSERVER_time_to_quit')   ! set time to quit flag in control area
!! F_EnD
  implicit none
  type(ctrl_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  m%time_to_quit = 1
  print *,'MSG: time to quit'
!! F_StArT
end subroutine IOSERVER_time_to_quit
!!
!! F_EnD

!! F_StArT  ! check time to quit flag in control area
function IOSERVER_is_time_to_quit() result(status)  BIND(C,name='IOSERVER_is_time_to_quit')  ! is it time to quit ?
!! import :: C_INT
  implicit none
  integer(C_INT) :: status   ! .true. if time to quit
!! F_EnD
  type(ctrl_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  status = m%time_to_quit
!! F_StArT
end function IOSERVER_is_time_to_quit
!!
!! F_EnD

end module ioserver_mod

!! F_StArT
subroutine IOSERVER_debug(mode) BIND(C,name='IOSERVER_debug')   ! set io server debug mode
!! F_EnD
  use ioserver_mod
  implicit none
!! F_StArT
!! import :: C_INT
  integer(C_INT), intent(IN), value :: mode
!! F_EnD
  debug_mode = (mode .ne. 0)
  print *,'INFO: debug mode =', debug_mode
!! F_StArT
end subroutine IOSERVER_debug
!!
!! F_EnD

!! F_StArT
function IOSERVER_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
!! F_EnD
  use ioserver_mod
!! F_StArT
  implicit none
  integer, intent(IN) :: comm
  logical :: status
!! F_EnD
  status = (comm == MPI_COMM_NULL)
  return
!! F_StArT
end function IOSERVER_Commisnull
!!
!! F_EnD

!! F_StArT
function IOSERVER_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
!! F_EnD
  use ioserver_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
  logical :: status
!! F_EnD
  status = (sz_base >= MBYTE .and. sz_relay >= MBYTE .and. sz_server >= MBYTE)
  if(.not. status) return     ! ERROR

  basesiz   = sz_base
  relaysiz  = sz_relay
  serversiz = sz_server
  status = .false.
  
  return
!! F_StArT
end function IOSERVER_winsizes
!!
!! F_EnD

!! F_StArT
function IOSERVER_set_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
!! F_EnD
  use ioserver_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
  logical :: status
!! F_EnD
  status = (sz_base >= MBYTE .and. sz_relay >= MBYTE .and. sz_server >= MBYTE)
  if(.not. status) return     ! ERROR

  basesiz   = sz_base
  relaysiz  = sz_relay
  serversiz = sz_server
  status = .false.
  
  return
!! F_StArT
end function IOSERVER_set_winsizes
!!
!! F_EnD

!! F_StArT
subroutine IOSERVER_get_winsizes(sz_base, sz_relay, sz_server) !  get communication window / shared memory area sizes
!! F_EnD
  use ioserver_mod
!! F_StArT
!! import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(OUT) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  integer(C_SIZE_T), intent(OUT) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  integer(C_SIZE_T), intent(OUT) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
!! F_EnD

  sz_base   = basesiz
  sz_relay  = relaysiz
  sz_server = serversiz
  
  return
!! F_StArT
end subroutine IOSERVER_get_winsizes
!!
!! F_EnD

!! F_StArT
subroutine IOSERVER_get_winmem(p_base, p_relay, p_server) !  get communication window / shared memory area sizes
!! F_EnD
  use ioserver_mod
!! F_StArT
!! import :: C_PTR
  implicit none
  type(C_PTR), intent(OUT) :: p_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
  type(C_PTR), intent(OUT) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
  type(C_PTR), intent(OUT) :: p_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
!! F_EnD
  integer(C_INTPTR_T) :: t

  p_base   = basemem
  p_relay  = relaymem
  p_server = servermem
  if(debug_mode) then
    if(C_ASSOCIATED(p_base))         print 1,'DEBUG: basemem   is DEFINED',transfer(p_base,t),' size =',basesiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_base))   print *,'DEBUG: basemem   is NULL'
    if(C_ASSOCIATED(p_relay))        print 1,'DEBUG: relaymem  is DEFINED',transfer(p_relay,t),' size =',relaysiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_relay))  print *,'DEBUG: relaymem  is NULL'
    if(C_ASSOCIATED(p_server))       print 1,'DEBUG: servermem is DEFINED',transfer(p_server,t),' size =',serversiz/1024/1024,' MB'
    if(.not. C_ASSOCIATED(p_server)) print *,'DEBUG: servermem is NULL'
  endif

1 format(1X,A,2X,Z16.16,A,I10,A)
  return
!! F_StArT
end subroutine IOSERVER_get_winmem
!!
!! F_EnD

!! F_StArT
subroutine set_IOSERVER_global_comm(comm)   !  set global communicator to other than MPI_COMM_WORLD
!! F_EnD
  use ioserver_mod
!! F_StArT
  implicit none
  integer, intent(IN) :: comm
!! F_EnD
  global_comm = comm
  return
!! F_StArT
end subroutine set_IOSERVER_global_comm
!!
!! F_EnD

!! F_StArT
subroutine set_IOSERVER_relay(fn)  ! set relay function to call to fn
!! F_EnD
  use ioserver_mod
!! F_StArT
  implicit none
  external :: fn
!! F_EnD
  relay_fn = C_FUNLOC(fn)
  return
!! F_StArT
end subroutine set_IOSERVER_relay
!!
!! F_EnD

subroutine IOSERVER_noop()  !  NO OP loop to park processes with minimal CPU consumption
  use ioserver_mod
  implicit none
  integer :: ierr

  if(debug_mode) print *,'DEBUG: NO-OP process, global rank =',global_rank
  do while(0 == IOSERVER_is_time_to_quit())    ! sleep loop until quit flag appears
    if(debug_mode) print *,'MSG: SLEEP LOOP'
    ierr = sleep(1)
  enddo
  call MPI_Finalize(ierr)
  stop

end subroutine IOSERVER_noop

!! F_StArT
function IOSERVER_init(model, modelio, allio, nodeio, serverio, nio_node, app_class) result(status)
!! F_EnD
  use ioserver_mod
!! F_StArT
!!import :: C_FUNPTR
  implicit none
  integer, intent(OUT) :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
  integer, intent(OUT) :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
  integer, intent(OUT) :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
  integer, intent(OUT) :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
  integer, intent(OUT) :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
  integer, intent(IN)  :: nio_node     ! number of io processes per compute node
  character(len=*), intent(IN) :: app_class
  integer :: status
!! F_EnD
  integer :: color, temp_comm, ierr, temp_win, temp, iocolor
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  type(ctrl_shared_memory), pointer :: main
  procedure(), pointer :: p

  ! 2 (model or server PE) or 3 (relay PE) of these 5 communicators will be true upon return from subroutine
  model    = MPI_COMM_NULL  ! preset all communicators to the NULL communicator
  modelio  = MPI_COMM_NULL
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
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
  winsize = 0
  if(smp_rank == 0) winsize = ctrlsiz                            ! size is supplied by local rank 0 only
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, smp_comm, win_base, ctrlwin, ierr)
  call MPI_Win_shared_query(ctrlwin, 0, ctrlsiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
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
    call IOSERVER_noop()              ! this subroutine will never return and call finalize
    return                            ! should never happen
  endif                                ! this is an active process (server/model/relay)

  ! split the all useful communicator (all_comm) into : server / model+relay
  iocolor = color
  if( iand(color,SERVER_COLOR) == SERVER_COLOR) iocolor = SERVER_COLOR  ! treat INPUT and OUTPUT server PEs the same way
  call mpi_comm_split(all_comm, iocolor, global_rank, temp_comm, ierr)  ! temp_comm is either serverio_comm or modelio_comm

! ===================================================================================
! at this point we only have "active" PEs (model, relay, server)
! allocate node local shared memory used for intra node communications
! ===================================================================================
  if(debug_mode) temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/relay/server nodes in debug mode

  if( iand(color,SERVER_COLOR) == SERVER_COLOR) then          ! IO server process

    serverio_comm = temp_comm             ! communicator for the "io server(s)"
    serverio = serverio_comm              ! output argument
    ! split serverio_comm by node, PEs on same SMP node
    call MPI_Comm_split_type(serverio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_com ,ierr)

    call MPI_Comm_rank(node_com, node_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_com, node_size, ierr)   ! population of SMP node
    ! for now, node_size should be equal to serverio_size

    ! allocate shared memory used for intra node communication between server PEs and 1 sided get/put with relay PEs
    winsize = 0
    if(node_rank == 0) winsize  = serversiz           ! size is supplied by node rank 0
    call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, node_com, win_base, serverwin, ierr)
    call MPI_Win_shared_query(serverwin, 0, serversiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
    servermem = transfer(win_base, C_NULL_PTR)        ! base address 
    ! serverwin will never be used after this point, all we are interested in is the shared memory

  else                                    ! model compute or IO relay process

    modelio_comm = temp_comm              ! communicator for "model compute and io relay" PEs
    modelio      = modelio_comm
    ! split modelio_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_com ,ierr)

    call MPI_Comm_rank(node_com, node_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_com, node_size, ierr)   ! population of SMP node

    ! allocate shared memory window used for intra node communication between model and relay PEs
    winsize = 0
    if(node_rank == 0) winsize  = relaysiz           ! size is supplied by node rank 0
    call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, node_com, win_base, relaywin, ierr)
    call MPI_Win_shared_query(relaywin, 0, relaysiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
    relaymem = transfer(win_base, C_NULL_PTR)        ! base address 
    ! relaywin will no longer be needed nor used after this point

    ! spread the nio_node relay PEs across the node (lowest and highest node ranks)
    if(node_rank >= (nio_node/2) .and. node_rank < (node_size - ((nio_node+1)/2))) then   ! model compute process
      color  = MODEL_COLOR
      if(debug_mode) print *,'DEBUG: model compute process, node rank =',node_rank,node_size,nio_node/2,(node_size - ((nio_node+1)/2))
    else                                                                                     ! relay IO process
      color = RELAY_COLOR  ! relay IO process
      if(debug_mode) print *,'DEBUG: IO relay process, node rank =',node_rank,node_size,nio_node/2,(node_size - ((nio_node+1)/2))
      ! not much memory is needed on the relay PEs for the remote IO window, allocate it
      winsize = basesiz           ! 1 sided window size for relay <-> server PEs on relay PEs
      call MPI_Alloc_mem(winsize, MPI_INFO_NULL, win_base, ierr)     ! allocate memory through MPI library for 1 sided get/put with server PEs
    endif

    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)    ! split into model compute and IO relay processes

    if(color == RELAY_COLOR) then ! io relay processes
      nodeio_comm = temp_comm     ! for module
      nodeio      = nodeio_comm   ! output argument
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

    allio_com = temp_comm                       ! all IO processes
    allio     = allio_com
    if(serverio_comm .ne. MPI_COMM_NULL) then   ! IO server process on separate node(s)
      status = SERVER_COLOR                     ! IO server
    endif

    ! winsize and win_base have been allocated previously, allocate 1 sided window for relay and server PEs
    call MPI_Win_create(win_base, winsize, disp_unit, MPI_INFO_NULL, allio_com, allio_win, ierr)
    basemem = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
    basesiz = winsize                           ! basemem same as servermem for server PEs

  endif

  if(nodeio_comm .ne. MPI_COMM_NULL) then       ! IO relay process, check if caller supplied relay routine

    if(C_ASSOCIATED(relay_fn)) then             ! caller supplied subroutine to be called on relay PEs
      if(debug_mode) print *,'INFO: relay_fn is associated'
      call C_F_PROCPOINTER(relay_fn,p)          ! associate procedure pointer with caller supplied address

      ! call user supplied relay code
      call p(model, modelio, allio, nodeio, serverio, node_com)    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call IOSERVER_time_to_quit()              ! activate quit signal for NO-OP PEs
      call MPI_Finalize(ierr)                   ! DO NOT return to caller, call finalize, then stop
      stop

    else                                        ! IO relay process on model node

      if(debug_mode) print *,'INFO: relay_fn is not associated'
      status = RELAY_COLOR                      ! IO relay, back to caller, with relay status code, caller will call relay subroutine
    endif
  endif

  return
!! F_StArT
end function IOSERVER_init
!!
!! F_EnD

!! F_StArT
!! end interface
!! F_EnD
