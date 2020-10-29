
!!  functions for C and FORTRAN programming
!  Copyright (C) 2020  Recherche en Prevision Numerique
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
!!F_EnD
module ioserver_mod
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  save
  integer :: global_comm   = MPI_COMM_WORLD
  integer :: model_comm    = MPI_COMM_NULL
  integer :: node_comm     = MPI_COMM_NULL
  integer :: modelio_comm  = MPI_COMM_NULL
  integer :: allio_comm    = MPI_COMM_NULL
  integer :: nodeio_comm   = MPI_COMM_NULL
  integer :: serverio_comm = MPI_COMM_NULL
  integer :: serverio_win  = MPI_WIN_NULL
  type(C_PTR) :: io_base   = C_NULL_PTR
end module ioserver_mod

!!F_StArT
function RPNMPI_Commisnull(comm) result(status)
!!F_EnD
  use ioserver_mod
  implicit none
!!F_StArT
  integer, intent(IN) :: comm
  logical :: status
!!F_EnD
  status = (comm == MPI_COMM_NULL)
  return
!!F_StArT
end function RPNMPI_Commisnull
!!F_EnD
!!F_StArT
subroutine set_ioserver_global_comm(comm)
!!F_EnD
  use ioserver_mod
!!F_StArT
  implicit none
  integer, intent(IN) :: comm
!!F_EnD
  global_comm = comm
  return
!!F_StArT
end subroutine set_ioserver_global_comm
!!F_EnD
!!F_StArT
function ioserver_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
!!F_EnD
  use ioserver_mod
!!F_StArT
  implicit none
  integer, intent(OUT) :: model
  integer, intent(OUT) :: allio
  integer, intent(OUT) :: nodeio
  integer, intent(OUT) :: serverio
  integer, intent(IN) :: nio_node      ! number of io processes per compute node
  character(len=*), intent(IN) :: app_class
  type(C_FUNPTR), intent(IN) :: nodeio_fn
  integer :: status
!!F_EnD
  integer :: color, temp_comm, ierr, global_rank, local_rank, local_size, disp_unit
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, win_base
  logical :: initialized
  procedure(), pointer :: p

  model    = MPI_COMM_NULL
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
  status = -1

  call MPI_Initialized(initialized, ierr)
  if(.not. initialized) call MPI_Init(ierr)    ! initialize MPI if not already done

  color = 0
  if(app_class(1:1) == 'S') color = 1    ! IO server app
  call MPI_Comm_rank(global_comm, global_rank, ierr)
  call mpi_comm_split(global_comm, color, global_rank, temp_comm, ierr)   ! split global communicator into : server / not server

  if(color == 1) then                     ! IO server
    serverio_comm = temp_comm             ! communicator for the "io server"
  else                                    ! model compute or IO on model node
    modelio_comm = temp_comm              ! communicator for "model compute and io" nodes
    call MPI_Comm_split_type(modelio_comm, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, temp_comm ,ierr)
    node_comm = temp_comm                             ! PEs on same SMP node (compute and IO processes)

    call MPI_Comm_rank(node_comm, local_rank, ierr)   ! rank on SMP node
    call MPI_Comm_size(node_comm, local_size, ierr)   ! rank on SMP node

    ! spread io processes across sockets (lowest and highest local ranks)
    if(local_rank >= (nio_node/2) .or. local_rank < (local_size - ((nio_node+1)/2))) then
      color = 1    ! model compute process
      status = 0 
    else
      color = 0  ! IO process, status can be 1 or 2
    endif
    call MPI_Comm_split(modelio_comm, color, global_rank, temp_comm, ierr)
    if(color == 0) then
      nodeio_comm = temp_comm  ! io processes on model nodes
    else
      model_comm = temp_comm   ! compute processes on model nodes
    endif
  endif

  color = 0                                     ! model compute process
  if(model_comm == MPI_COMM_NULL) color = 1     ! IO server or IO process on model node

  call MPI_Comm_split(global_comm, color, global_rank, temp_comm, ierr)
  if(color == 1) then
    allio_comm = temp_comm                     ! all IO processes
    if(serverio_comm .ne. MPI_COMM_NULL) then  ! IO server processe
      winsize = 1024 * 1024 * 1024    ! PLACEHOLDER CODE TO BE ADJUSTED
      status = 2
    else                                       ! IO process on model node
      winsize = 1024 * 1024           ! PLACEHOLDER CODE TO BE ADJUSTED
      status = 1 
    endif
    disp_unit = 4                ! 32 bit word units
    call MPI_Win_allocate(winsize, disp_unit, MPI_INFO_NULL, allio_comm, win_base, serverio_win, ierr)
    io_base = transfer(win_base, C_NULL_PTR)    ! base address of local window (address in integer -> C pointer)
  endif

  if(nodeio_comm .ne. MPI_COMM_NULL) then       ! IO process on model node
    p => NULL()
    if(C_ASSOCIATED(nodeio_fn)) then
      print *,'nodeio_fn associated'
      call C_F_PROCPOINTER(nodeio_fn,p)
      ! the IO process on model node code may not return
      call p(220)    ! PLACEHOLDER CODE TO BE ADJUSTED
      call MPI_Finalize(ierr)
      stop
    else
      print *,'nodeio_fn is not associated'
      return
    endif
  endif

  return
!!F_StArT
end function ioserver_init
!!F_EnD




program test
  use ISO_C_BINDING
  implicit none
  integer :: model, allio, nodeio, serverio, status
  procedure(), pointer :: p
  external :: demo_fn
  interface
    function ioserver_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
      import :: C_FUNPTR
      integer, intent(OUT) :: model, allio, nodeio, serverio
      integer, intent(IN) :: nio_node
      character(len=*), intent(IN) :: app_class
      type(C_FUNPTR), intent(IN) :: nodeio_fn
      integer :: status
    end function ioserver_init
  end interface

  p => demo_fn
  call p(110)
  status = ioserver_init(model, allio, nodeio, serverio, 2, "S", C_FUNLOC(demo_fn))
  status = ioserver_init(model, allio, nodeio, serverio, 2, "S", C_NULL_FUNPTR)
end program
subroutine demo_fn(i)
  implicit none
  integer, intent(IN) :: i
  print *,'in demo',i
end subroutine demo_fn
