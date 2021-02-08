
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
  !  control memory, shared by all PEs on a given SMP node
  type(C_PTR)         :: ctrlmem   = C_NULL_PTR  !  base address of main (control) shared memory area on this node
  integer(C_INTPTR_T) :: ctrlsiz   = MBYTE       !  size of above
  integer             :: ctrlwin   = MPI_WIN_NULL   !  NOT USED for communications

  type(C_PTR)         :: basewin   = C_NULL_PTR  !  base address of IO window (all IO PEs)
  integer(C_INTPTR_T) :: basesiz   = 0           !  size of above
  integer             :: allio_win = MPI_WIN_NULL   !  1 sided window used by RELAY + SERVER PEs
  integer             :: allio_com = MPI_COMM_NULL  !  all IO PEs (model IO + IO server) (subset of all_comm)

  ! shared memory used for heaps and circular buffers on a given SMP node
  ! this memory is used for cummunications between relay and model PEs
  type(C_PTR)         :: relaymem  = C_NULL_PTR  !  base address of relay+model PEs shared memory area
  integer(C_INTPTR_T) :: relaysiz  = GBYTE       !  size of above
  integer             :: relaywin  = MPI_WIN_NULL   !  NOT USED for communications
  integer :: node_comm     = MPI_COMM_NULL       !  model + relay PEs on same node

  ! shared memory on io server node(s), used by io server PEs to communicate
  ! used for 1 sided communications window between relay and io server PEs
  ! NO memory is normally allocated on relay PEs
  type(C_PTR)         :: servermem = C_NULL_PTR  !  base address of io server PEs shared memory area
  integer(C_INTPTR_T) :: serversiz = GBYTE       !  size of above
  integer             :: serverwin  = MPI_WIN_NULL  !  NOT USED for communications

  integer :: global_comm   = MPI_COMM_WORLD      !  MPI "WORLD" for this set of PEs
  integer :: smp_comm      = MPI_COMM_NULL       !  PEs on this SMP node        (any kind ) (subset of global_comm)
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

!! F_StArT   ! set time to quit flag in control area
subroutine IOSERVER_time_to_quit() BIND(C,name='IOSERVER_time_to_quit')
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
function IOSERVER_is_time_to_quit() result(status)  BIND(C,name='IOSERVER_is_time_to_quit')
  implicit none
  logical :: status   ! .true. if time to quit
!! F_EnD
  type(ctrl_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  status = (m%time_to_quit == 1)
!! F_StArT
end function IOSERVER_is_time_to_quit
!!
!! F_EnD

end module ioserver_mod

!! F_StArT   ! set io server debug mode
subroutine IOSERVER_debug(mode) BIND(C,name='IOSERVER_debug')
!! F_EnD
  use ioserver_mod
  implicit none
!! F_StArT
  logical, intent(IN), value :: mode
!! F_EnD
  debug_mode = mode
  print *,'INFO: debug mode =', debug_mode
!! F_StArT
end subroutine IOSERVER_debug
!!
!! F_EnD

!! F_StArT
function IOSERVER_Commisnull(comm) result(status)
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
function IOSERVER_winsizes(sz_base, sz_relay, sz_server) result(status)
!! F_EnD
  use ioserver_mod
!! F_StArT
!!import :: C_SIZE_T
  implicit none
  integer(C_SIZE_T), intent(IN) :: sz_base, sz_relay, sz_server
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
subroutine set_IOSERVER_global_comm(comm)
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
subroutine set_IOSERVER_relay(fn)
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

!! F_StArT
function IOSERVER_init(model, allio, nodeio, serverio, nio_node, app_class) result(status)
!! F_EnD
  use ioserver_mod
!! F_StArT
!!import :: C_FUNPTR
  implicit none
  integer, intent(OUT) :: model
  integer, intent(OUT) :: allio
  integer, intent(OUT) :: nodeio
  integer, intent(OUT) :: serverio
  integer, intent(IN) :: nio_node      ! number of io processes per compute node
  character(len=*), intent(IN) :: app_class
  integer :: status
!! F_EnD
  integer :: color, temp_comm, ierr, global_rank, node_rank, node_size, temp_win, temp, smp_rank, iocolor
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  type(ctrl_shared_memory), pointer :: main
  procedure(), pointer :: p

  model    = MPI_COMM_NULL  ! preset all communicators to the NULL communicator
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
  status   = NO_COLOR       ! precondition for failure

  call MPI_Initialized(initialized, ierr)
  if(.not. initialized) call MPI_Init(ierr)    ! initialize MPI if not already done
  call MPI_Comm_rank(global_comm, global_rank, ierr)

  color = -1                             ! invalid
  if(app_class(1:1) == 'M') color = MODEL_COLOR  + RELAY_COLOR    ! model or relay app
  if(app_class(1:1) == 'S') color = SERVER_COLOR + INOUT_COLOR    ! IO server app (both input and ouptut)
  if(app_class(1:1) == 'O') color = OUTPUT_COLOR + SERVER_COLOR   ! IO server app (output only)
  if(app_class(1:1) == 'I') color = INPUT_COLOR  + SERVER_COLOR   ! IO server app (input only)
  if(app_class(1:1) == 'Z') color = NO_OP_COLOR                   ! null app
  if(color == -1) return                 ! miserable failure

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

  if(color == NO_OP_COLOR) then   ! this is a NO-OP process, enter wait loop for finalize
    if(debug_mode) print *,'DEBUG: NO-OP process, glocal rank =',global_rank
    do while(.not. IOSERVER_is_time_to_quit())    ! sleep loop until quit flag appears
      if(debug_mode) print *,'MSG: SLEEP LOOP'
      temp = sleep(1)
    enddo
    call MPI_Finalize(ierr)
    stop
  else
    ! split all useful communicator (all_comm) into : server / model+relay
    iocolor = color
    if( iand(color,SERVER_COLOR) == SERVER_COLOR) iocolor = SERVER_COLOR
    call mpi_comm_split(all_comm, iocolor, global_rank, temp_comm, ierr)
  endif
! ===================================================================================
! at this point we only have "active" PEs (model, relay, server)
! allocate node local shared memory used for intra node communications
! ===================================================================================
  if(debug_mode) temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/realy/server nodes in debug mode

  if( iand(color,SERVER_COLOR) == SERVER_COLOR) then          ! IO server process

    serverio_comm = temp_comm             ! communicator for the "io server(s)"
    serverio = serverio_comm              ! output argument
    ! split serverio_comm by node, PEs on same SMP node
    call MPI_Comm_split_type(serverio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm ,ierr)

    call MPI_Comm_rank(node_comm, node_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_comm, node_size, ierr)   ! population of SMP node
    ! for now, node_size should be equal to serverio_size

    ! allocate shared memory window used for intra node communication between server PEs and remote IO window
    winsize = 0
    if(node_rank == 0) winsize  = serversiz           ! size is supplied by node rank 0
    call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, node_comm, win_base, serverwin, ierr)
    call MPI_Win_shared_query(serverwin, 0, serversiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
    servermem = transfer(win_base, C_NULL_PTR)        ! base address 
    ! serverwin will not be used after this point, all we are interested in the shared memory

  else                                    ! model compute or IO relay process

    modelio_comm = temp_comm              ! communicator for "model compute and io relay" PEs
    ! split modelio_comm by node, PEs on same SMP node (compute and IO processes)
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm ,ierr)

    call MPI_Comm_rank(node_comm, node_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_comm, node_size, ierr)   ! population of SMP node

    ! allocate shared memory window used for intra node communication between model and relay PEs
    winsize = 0
    if(node_rank == 0) winsize  = relaysiz           ! size is supplied by node rank 0
    call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, node_comm, win_base, relaywin, ierr)
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
      winsize = 1024 * 1024           ! PLACEHOLDER CODE TO BE ADJUSTED
      call MPI_Alloc_mem(winsize, MPI_INFO_NULL, win_base, ierr)     ! allocate memory through MPI library
    endif

    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)    ! split into model compute and IO relay processes

    if(color == RELAY_COLOR) then ! io relay processes
      nodeio_comm = temp_comm     ! internal module
      nodeio      = nodeio_comm   ! output argument
    else                          ! compute processes
      model_comm = temp_comm      ! internal module
      model      = model_comm     ! output argument
    endif

  endif   ! (color == SERVER_COLOR)
! ===================================================================================
! split IO PEs into server and relay PEs, create 1 sided communication windew
! set return code to appropriate value or call 
! ===================================================================================
  if(model_comm == MPI_COMM_NULL) then    ! IO server or IO process on model node
    color = RELAY_COLOR + SERVER_COLOR
  else
    color = MODEL_COLOR                   ! model compute process
  endif

  call MPI_Comm_split(all_comm, color, global_rank, temp_comm, ierr)  ! split all_comm into model and IO (relay+server) processes

  if(color .ne. MODEL_COLOR) then               ! IO relay or IO server

    allio_com = temp_comm                       ! all IO processes
    if(serverio_comm .ne. MPI_COMM_NULL) then   ! IO server process on separate node(s)
      status = SERVER_COLOR                     ! IO server
    endif

    ! winsize and win_base have been allocated previously, allocate 1 sided window
    call MPI_Win_create(win_base, winsize, disp_unit, MPI_INFO_NULL, allio_com, allio_win, ierr)
    basewin = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
    basesiz = winsize

  else                                          ! model compute 
    status = MODEL_COLOR
  endif  ! (color .ne. MODEL_COLOR)

  if(nodeio_comm .ne. MPI_COMM_NULL) then       ! IO relay process, check if user supplied relay routine

    if(C_ASSOCIATED(relay_fn)) then            ! user supplied subroutine to be called on relay processes
      if(debug_mode) print *,'INFO: relay_fn is associated'
      call C_F_PROCPOINTER(relay_fn,p)         ! associate procedure pointer to user supplied address

      ! call user supplied relay code
      call p(model, allio, nodeio, serverio)    ! PLACEHOLDER CODE TO BE ADJUSTED when API is finalized

      call IOSERVER_time_to_quit()              ! activate quit signal for NO-OP PEs
      call MPI_Finalize(ierr)                   ! do not return to caller, call finalize then stop
      stop

    else                                        ! IO relay process on model node

      if(debug_mode) print *,'INFO: relay_fn is not associated'
      status = RELAY_COLOR                      ! IO relay, back to caller, with IO relay status code
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
