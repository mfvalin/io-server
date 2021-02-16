! Copyright (C) 2021  Environnement et Changement climatique Canada
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

integer, parameter :: NO_COLOR     = 1
integer, parameter :: MODEL_COLOR  = 1
integer, parameter :: RELAY_COLOR  = 2
integer, parameter :: SERVER_COLOR = 4
integer, parameter :: OUTPUT_COLOR = 8
integer, parameter :: INPUT_COLOR  = 16
integer, parameter :: INOUT_COLOR  = INPUT_COLOR + OUTPUT_COLOR
integer, parameter :: NO_OP_COLOR  = 8192   ! MUST BE THE HIGHEST VALUE

interface

subroutine IOSERVER_time_to_quit() BIND(C,name='IOSERVER_time_to_quit')   ! set time to quit flag in control area
end subroutine IOSERVER_time_to_quit

function IOSERVER_is_time_to_quit() result(status)  BIND(C,name='IOSERVER_is_time_to_quit')  ! is it time to quit ?
import :: C_INT
implicit none
integer(C_INT) :: status   ! .true. if time to quit
end function IOSERVER_is_time_to_quit

subroutine IOSERVER_debug(mode) BIND(C,name='IOSERVER_debug')   ! set io server debug mode
import :: C_INT
integer(C_INT), intent(IN), value :: mode
end subroutine IOSERVER_debug

function IOSERVER_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
implicit none
integer, intent(IN) :: comm
logical :: status
end function IOSERVER_Commisnull

function IOSERVER_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
logical :: status
end function IOSERVER_winsizes

function IOSERVER_set_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
logical :: status
end function IOSERVER_set_winsizes

subroutine IOSERVER_get_winsizes(sz_base, sz_relay, sz_server) !  get communication window / shared memory area sizes
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(OUT) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
integer(C_SIZE_T), intent(OUT) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
integer(C_SIZE_T), intent(OUT) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
end subroutine IOSERVER_get_winsizes

subroutine IOSERVER_get_winmem(p_base, p_relay, p_server) !  get communication window / shared memory area sizes
import :: C_PTR
implicit none
type(C_PTR), intent(OUT) :: p_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
type(C_PTR), intent(OUT) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
type(C_PTR), intent(OUT) :: p_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
end subroutine IOSERVER_get_winmem

subroutine set_IOSERVER_global_comm(comm)   !  set global communicator to other than MPI_COMM_WORLD
implicit none
integer, intent(IN) :: comm
end subroutine set_IOSERVER_global_comm

subroutine set_IOSERVER_relay(fn)  ! set relay function to call to fn
implicit none
external :: fn
end subroutine set_IOSERVER_relay

function IOSERVER_init(model, modelio, allio, nodeio, serverio, nio_node, app_class) result(status)
import :: C_FUNPTR
implicit none
integer, intent(OUT) :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
integer, intent(OUT) :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
integer, intent(OUT) :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
integer, intent(OUT) :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
integer, intent(OUT) :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
integer, intent(IN)  :: nio_node     ! number of io processes per compute node
character(len=*), intent(IN) :: app_class
integer :: status
end function IOSERVER_init

end interface
