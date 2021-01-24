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
interface
function RPNMPI_Commisnull(comm) result(status)
implicit none
integer, intent(IN) :: comm
logical :: status
end function RPNMPI_Commisnull
subroutine set_ioserver_global_comm(comm)
implicit none
integer, intent(IN) :: comm
end subroutine set_ioserver_global_comm
function ioserver_init(model, allio, nodeio, serverio, nio_node, app_class, nodeio_fn) result(status)
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
end function ioserver_init
end interface
