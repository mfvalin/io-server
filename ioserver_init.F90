
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
  integer :: allio_comm    = MPI_COMM_NULL
  integer :: nodeio_comm   = MPI_COMM_NULL
  integer :: serverio_comm = MPI_COMM_NULL
  integer :: serverio_win  = MPI_WIN_NULL
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
function ioserver_init(model, allio, nodeio, serverio, app_class, nodeio_fn) result(status)
!!F_EnD
  use ioserver_mod
!!F_StArT
  implicit none
  integer, intent(OUT) :: model
  integer, intent(OUT) :: allio
  integer, intent(OUT) :: nodeio
  integer, intent(OUT) :: serverio
  character(len=*), intent(IN) :: app_class
  type(C_FUNPTR), intent(IN) :: nodeio_fn
  integer :: status
!!F_EnD
  integer :: color, temp_comm
  procedure(), pointer :: p

  model    = MPI_COMM_NULL
  allio    = MPI_COMM_NULL
  nodeio   = MPI_COMM_NULL
  serverio = MPI_COMM_NULL
  status = -1
  color = 0
  if(app_class(1:1) == 'S') color = 1    ! IO server app
  p => NULL()
  if(C_ASSOCIATED(nodeio_fn)) then
    print *,'nodeio_fn associated'
    call C_F_PROCPOINTER(nodeio_fn,p)
    call p(220)
  else
    print *,'nodeio_fn is not associated'
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
    function ioserver_init(model, allio, nodeio, serverio, app_class, nodeio_fn) result(status)
      import :: C_FUNPTR
      integer, intent(OUT) :: model, allio, nodeio, serverio
      character(len=*), intent(IN) :: app_class
      type(C_FUNPTR), intent(IN) :: nodeio_fn
      integer :: status
    end function ioserver_init
  end interface

  p => demo_fn
  call p(110)
  status = ioserver_init(model, allio, nodeio, serverio, "S", C_FUNLOC(demo_fn))
  status = ioserver_init(model, allio, nodeio, serverio, "S", C_NULL_FUNPTR)
end program
subroutine demo_fn(i)
  implicit none
  integer, intent(IN) :: i
  print *,'in demo',i
end subroutine demo_fn
