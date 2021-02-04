
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

  integer, parameter :: MODEL_COLOR  = 1
  integer, parameter :: RELAY_COLOR  = 2
  integer, parameter :: SERVER_COLOR = 4
  integer, parameter :: NO_OP_COLOR  = 1024   ! MUST BE THE HIGHEST VALUE
!!
!! F_EnD

  type :: comm_rank_size
    integer :: comm = MPI_COMM_NULL
    integer :: rank = -1
    integer :: size = 0
  end type
  type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(MPI_COMM_NULL, -1, 0)

  type :: main_shared_memory
    integer :: time_to_quit = 0
  end type

  save
  type(C_PTR) ::         ctrlmem   = C_NULL_PTR  !  base address of main (control) shared memory area on this node
  integer(C_INTPTR_T) :: ctrlsiz   = MBYTE       !  size of above

  type(C_PTR) ::         basewin   = C_NULL_PTR  !  base address of IO window (all IO PEs)
  integer(C_INTPTR_T) :: basesiz   = 0           !  size of above

  type(C_PTR) ::         relaymem  = C_NULL_PTR  !  base address of relay+model PEs shared memory area
  integer(C_INTPTR_T) :: relaysiz  = GBYTE       !  size of above

  type(C_PTR) ::         servermem = C_NULL_PTR  !  base address of io server PEs shared memory area
  integer(C_INTPTR_T) :: serversiz = GBYTE       !  size of above

  integer :: global_comm   = MPI_COMM_WORLD      !  MPI WORLD for this set of PEs
  integer :: same_comm     = MPI_COMM_WORLD      !  PEs on this SMP node (any kind)
  integer :: all_comm      = MPI_COMM_NULL       !  all non NO-OP PEs 
  integer :: model_comm    = MPI_COMM_NULL       !  model compute PEs 
  integer :: node_comm     = MPI_COMM_NULL       !  PEs on same node
  integer :: modelio_comm  = MPI_COMM_NULL       !  model compute and IO IO PEs
  integer :: allio_comm    = MPI_COMM_NULL       !  all IO PEs (model IO + IO server)
  integer :: nodeio_comm   = MPI_COMM_NULL       !  IO PEs on model nodes
  integer :: serverio_comm = MPI_COMM_NULL       !  IO server PEs
  integer :: serverio_win  = MPI_WIN_NULL        !  IO window (all IO PEs)
  integer :: modelio_win   = MPI_WIN_NULL        !  IO window (model and relay PEs)
  integer :: disp_unit     = 4                   !  displacement unit in 1 sided windows (32 bits by default)

  contains

!! F_StArT
!! interface
!!
!! F_EnD

!! F_StArT   ! set time to quit flag in control area
subroutine IOSERVER_time_to_quit() BIND(C,name='IOSERVER_time_to_quit')
!! F_EnD
  implicit none
  type(main_shared_memory), pointer :: m
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
  type(main_shared_memory), pointer :: m
  call C_F_POINTER(ctrlmem,m)
  status = (m%time_to_quit == 1)
!! F_StArT
end function IOSERVER_is_time_to_quit
!!
!! F_EnD

end module ioserver_mod

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
function IOSERVER_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
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
  type(C_FUNPTR), intent(IN) :: nodeio_fn
  integer :: status
!! F_EnD
  integer :: color, temp_comm, ierr, global_rank, node_rank, node_size, temp_win, temp, local_rank
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  integer, dimension(:), pointer :: shared
  type(main_shared_memory), pointer :: main
  procedure(), pointer :: p

  interface
    function sleep(nsec) result(ok) BIND(C,name='sleep')
      import :: C_INT
      implicit none
      integer(C_INT), value :: nsec
      integer :: ok
    end function sleep
  end interface

  model    = MPI_COMM_NULL
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
  status = -1

  call MPI_Initialized(initialized, ierr)
  if(.not. initialized) call MPI_Init(ierr)    ! initialize MPI if not already done
  call MPI_Comm_rank(global_comm, global_rank, ierr)

  color = -1                             ! invalid
  if(app_class(1:1) == 'M') color = MODEL_COLOR + RELAY_COLOR     ! model or relay app
  if(app_class(1:1) == 'S') color = SERVER_COLOR                  ! IO server app
  if(app_class(1:1) == 'Z') color = NO_OP_COLOR                   ! null app
  if(color == -1) return                 ! miserable failure

  ! split by node, all PEs (the result communicator is temporary and not kept)
  call MPI_Comm_split_type(global_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, temp_comm ,ierr)
  call MPI_Comm_rank(temp_comm, local_rank, ierr)                  ! rank on SMP node

  winsize = 0
  if(local_rank == 0) winsize = ctrlsiz                            ! size is supplied by local rank 0 only
  ! allocate shared memory segment used for control
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, temp_comm, win_base, temp_win, ierr)
  call MPI_Win_shared_query(temp_win, 0, ctrlsiz, disp_unit, win_base, ierr)   ! get base address of rank 0 memory
  ctrlmem = transfer(win_base, C_NULL_PTR)
  call C_F_POINTER(ctrlmem, shared, [ctrlsiz/4]) ! bytes to integers
  call C_F_POINTER(ctrlmem, main)                ! main control structure
  if(local_rank == 0) main%time_to_quit = 0      ! initialize 
  call MPI_barrier(global_comm, ierr)            ! wait until control area initialization is done everywhere

  ! split global communicator into : server+model+relay / no-op
  ! there MUST be AT LEAST ONE non NO-OP process on each node
  call mpi_comm_split(global_comm, color/NO_OP_COLOR, global_rank, all_comm, ierr)

  if(color == NO_OP_COLOR) then   ! this is a NO-OP process
    print *,'DEBUG: NO-OP process, glocal rank =',global_rank
    do while(.not. IOSERVER_is_time_to_quit())    ! sleep loop until quit flag appears
      print *,'MSG: SLEEP LOOP'
      temp = sleep(1)
    enddo
    call MPI_Finalize(ierr)
    stop
  else
    ! split all useful communicator (all_comm) into : server / model+relay
    call mpi_comm_split(all_comm, color, global_rank, temp_comm, ierr)
  endif

  temp = sleep(2)  ! to test the NO-OP wait loop, this is only executed on model/realy/server nodes

  if(color == SERVER_COLOR) then          ! IO server process

    serverio_comm = temp_comm             ! communicator for the "io server"
    serverio = serverio_comm              ! output argument

  else                                    ! model compute or IO relay process

    modelio_comm = temp_comm              ! communicator for "model compute and io relay" nodes
    ! split modelio_comm by node
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, temp_comm ,ierr)
    node_comm = temp_comm                             ! PEs on same SMP node (compute and IO processes)

    call MPI_Comm_rank(node_comm, node_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_comm, node_size, ierr)   ! population of SMP node

    winsize   = relaysiz
    ! allocate shared window used for intra node communication between model and relay nodes
    call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, node_comm, win_base, modelio_win, ierr)

    ! spread io processes across sockets (lowest and highest local ranks)
    if(node_rank >= (nio_node/2) .and. node_rank < (node_size - ((nio_node+1)/2))) then   ! model compute process
      color  = MODEL_COLOR
      status = MODEL_COLOR
      print *,'DEBUG: model compute process, node rank =',node_rank,node_size,nio_node/2,(node_size - ((nio_node+1)/2))
    else                                                                                     ! IO relay process
      color = RELAY_COLOR  ! IO process, status can be 1 or 2
      print *,'DEBUG: IO relay process, node rank =',node_rank,node_size,nio_node/2,(node_size - ((nio_node+1)/2))
    endif

    ! split into model and IO relay processes
    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)   
    if(color == RELAY_COLOR) then ! io relay processes on model nodes
      nodeio_comm = temp_comm     ! internal module
      nodeio      = nodeio_comm   ! output argument
    else                          ! compute processes on model nodes
      model_comm = temp_comm      ! internal module
      model      = model_comm     ! output argument
    endif

  endif   ! (color == SERVER_COLOR)

  if(model_comm == MPI_COMM_NULL) then    ! IO server or IO process on model node
    color = RELAY_COLOR + SERVER_COLOR
  else
    color = MODEL_COLOR                   ! model compute process
  endif

  call MPI_Comm_split(all_comm, color, global_rank, temp_comm, ierr)  ! split into model and IO processes
  if(color .ne. MODEL_COLOR) then
    allio_comm = temp_comm                     ! all IO processes
    if(serverio_comm .ne. MPI_COMM_NULL) then  ! IO server process on separate node(s)
      winsize = 1024 * 1024 * 1024    ! PLACEHOLDER CODE TO BE ADJUSTED
      status = SERVER_COLOR                    ! IO server
    else                                       ! IO relay process on model node
      winsize = 1024 * 1024           ! PLACEHOLDER CODE TO BE ADJUSTED
      status = RELAY_COLOR                     ! IO relay
    endif
    call MPI_Win_allocate(winsize, disp_unit, MPI_INFO_NULL, allio_comm, win_base, serverio_win, ierr)
    basewin = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
    basesiz = winsize
  endif

  if(nodeio_comm .ne. MPI_COMM_NULL) then       ! IO relay process
    p => NULL()
    if(C_ASSOCIATED(nodeio_fn)) then            ! user supplied function to be called on relay processes
      print *,'INFO: nodeio_fn is associated'
      call C_F_PROCPOINTER(nodeio_fn,p)
      ! the IO process on model node code may not return
      call p(model, allio, nodeio, serverio)    ! PLACEHOLDER CODE TO BE ADJUSTED
      call IOSERVER_time_to_quit()
      call MPI_Finalize(ierr)
      stop
    else
      print *,'INFO: nodeio_fn is not associated'
      return
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

#if defined(SELF_TEST)
program test
  use ISO_C_BINDING
  implicit none
  integer :: model, allio, nodeio, serverio, status, ierr
  procedure(), pointer :: p
  external :: demo_fn
  interface
    function IOSERVER_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
      import :: C_FUNPTR
      integer, intent(OUT) :: model, allio, nodeio, serverio
      integer, intent(IN) :: nio_node
      character(len=*), intent(IN) :: app_class
      type(C_FUNPTR), intent(IN) :: nodeio_fn
      integer :: status
    end function IOSERVER_init
  end interface

  p => demo_fn
  call p(110)
  status = IOSERVER_init(model, allio, nodeio, serverio, 2, "S", C_FUNLOC(demo_fn))
  print *,'status =',status
!   status = IOSERVER_init(model, allio, nodeio, serverio, 2, "S", C_NULL_FUNPTR)
  call MPI_Finalize(ierr)
end program
subroutine demo_fn(i)
  implicit none
  integer, intent(IN) :: i
  print *,'in demo',i
end subroutine demo_fn
#endif
