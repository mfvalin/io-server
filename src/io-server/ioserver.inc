! Copyright (C) 2021  Environnement Canada
! 
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
! 
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

! This file has been generated from ioserver_init.F90

integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

integer, parameter :: MODEL_COLOR  = 1
integer, parameter :: RELAY_COLOR  = 2
integer, parameter :: SERVER_COLOR = 4
integer, parameter :: NO_OP_COLOR  = 1024   ! MUST BE THE HIGHEST VALUE

interface

subroutine IOSERVER_time_to_quit() BIND(C,name='IOSERVER_time_to_quit')
end subroutine IOSERVER_time_to_quit

function IOSERVER_is_time_to_quit() result(status)  BIND(C,name='IOSERVER_is_time_to_quit')
implicit none
logical :: status   ! .true. if time to quit
end function IOSERVER_is_time_to_quit

function IOSERVER_Commisnull(comm) result(status)
implicit none
integer, intent(IN) :: comm
logical :: status
end function IOSERVER_Commisnull

function IOSERVER_winsizes(sz_base, sz_relay, sz_server) result(status)
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(IN) :: sz_base, sz_relay, sz_server
logical :: status
end function IOSERVER_winsizes

subroutine set_IOSERVER_global_comm(comm)
implicit none
integer, intent(IN) :: comm
end subroutine set_IOSERVER_global_comm

function IOSERVER_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
import :: C_FUNPTR
implicit none
integer, intent(OUT) :: model
integer, intent(OUT) :: allio
integer, intent(OUT) :: nodeio
integer, intent(OUT) :: serverio
integer, intent(IN) :: nio_node      ! number of io processes per compute node
character(len=*), intent(IN) :: app_class
type(C_FUNPTR), intent(IN) :: nodeio_fn
integer :: status
end function IOSERVER_init

end interface
