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
interface

function ptr_translate_from(from, from_color, from_rank) result(local) BIND(C,name='Ptr_translate_from')
import :: C_PTR, C_INT
type(C_PTR), intent(IN), value :: from
integer(C_INT), intent(IN), value :: from_color
integer(C_INT), intent(IN), value :: from_rank
type(C_PTR) :: local
end function ptr_translate_from

function ptr_translate_to(from, to_color, to_rank) result(to) BIND(C,name='Ptr_translate_to')
import :: C_PTR, C_INT
type(C_PTR), intent(IN), value :: from
integer(C_INT), intent(IN), value :: to_color
integer(C_INT), intent(IN), value :: to_rank
type(C_PTR) :: to
end function ptr_translate_to

subroutine IOserver_set_time_to_quit() BIND(C,name='IOserver_set_time_to_quit')   ! set time to quit flag in control area
end subroutine IOserver_set_time_to_quit

function IOserver_is_time_to_quit() result(status)  BIND(C,name='IOserver_is_time_to_quit')  ! is it time to quit ?
import :: C_INT
implicit none
integer(C_INT) :: status   ! .true. if time to quit
end function IOserver_is_time_to_quit

subroutine verify_translations()  ! chech that address translations are coherent local <--> remote
end subroutine verify_translations

subroutine print_io_colors()
end subroutine print_io_colors

function IOserver_get_crs(color) result(crs)
import :: comm_rank_size
implicit none
integer, intent(IN), value :: color
type(comm_rank_size) :: crs
end function IOserver_get_crs
function IOserver_get_heap() result(h)
import :: heap
type(heap) :: h
end function IOserver_get_heap

function IOserver_get_cio_in() result(cio)
import :: circular_buffer
type(circular_buffer) :: cio
end function IOserver_get_cio_in

function IOserver_get_cio_out() result(cio)
import :: circular_buffer
type(circular_buffer) :: cio
end function IOserver_get_cio_out

subroutine IOserver_debug(mode) BIND(C,name='IOserver_debug')   ! set io server debug mode
import :: C_INT
integer(C_INT), intent(IN), value :: mode
end subroutine IOserver_debug

function IOserver_CRSisnull(crs) result(status)   !  is this a NULL communicator combo ?
import :: comm_rank_size
implicit none
type(comm_rank_size), intent(INOUT) :: crs
logical :: status
end function IOserver_CRSisnull

function IOserver_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
implicit none
integer, intent(IN) :: comm
logical :: status
end function IOserver_Commisnull

function IOserver_set_winsize(wkind, sz) result(status)  !  set shared memory area sizes
import :: C_SIZE_T
implicit none
integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
integer(C_SIZE_T), intent(IN) :: sz        ! shared memory area size
logical :: status                          ! true if there was an error
end function IOserver_set_winsize

function IOserver_get_winsize(wkind) result(sz)  !  get shared memory area sizes
import :: C_SIZE_T
implicit none
integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
integer(C_SIZE_T)             :: sz
end function IOserver_get_winsize

function IOserver_get_win_ptr(wkind) result(pt)  !  get shared memory area address
import :: C_PTR
implicit none
integer, intent(IN)           :: wkind     ! target window window (base, relay, server, ....
type(C_PTR)                   :: pt
end function IOserver_get_win_ptr

function IOserver_set_winsizes(sz_base, sz_relay, sz_server) result(status)  !  set communication window / shared memory area sizes
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(IN) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
integer(C_SIZE_T), intent(IN) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
integer(C_SIZE_T), intent(IN) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
logical :: status
end function IOserver_set_winsizes

subroutine IOserver_get_winsizes(sz_base, sz_relay, sz_server) !  get communication window / shared memory area sizes
import :: C_SIZE_T
implicit none
integer(C_SIZE_T), intent(OUT) :: sz_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
integer(C_SIZE_T), intent(OUT) :: sz_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
integer(C_SIZE_T), intent(OUT) :: sz_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
end subroutine IOserver_get_winsizes

subroutine IOserver_get_winmem(p_base, p_relay, p_server) !  get communication window / shared memory area addresses
import :: C_PTR
implicit none
type(C_PTR), intent(OUT) :: p_base        ! 1 sided window size for relay <-> server PEs on relay PEs  (normally quite small)
type(C_PTR), intent(OUT) :: p_relay       ! shared memory size for relay PEs (relay <-> model exchanges)
type(C_PTR), intent(OUT) :: p_server      ! shared memory size for server PEs (internal exchanges + get/put with relay PEs)
end subroutine IOserver_get_winmem

subroutine set_IOserver_global_comm(comm)   !  set global communicator to other than MPI_COMM_WORLD
implicit none
integer, intent(IN) :: comm
end subroutine set_IOserver_global_comm

subroutine set_IOserver_relay(fn)  ! set relay function to call to fn
implicit none
external :: fn
end subroutine set_IOserver_relay

subroutine set_IOserver_server(fn)  ! set server function to call to fn
implicit none
external :: fn
end subroutine set_IOserver_server

function IOserver_int_finalize() result(status)
integer :: status
end function IOserver_int_finalize

function IOserver_int_init(nio_node, app_class) result(status)
import :: C_FUNPTR
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
end function IOserver_int_init

end interface
