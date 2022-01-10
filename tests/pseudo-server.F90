! Copyright (C) 2022  Environnement et Changement climatique Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

program pseudoserver
  use ISO_C_BINDING
  implicit none
  interface
    function IOserver_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
    implicit none
    integer, intent(IN) :: comm
    logical :: status
    end function IOserver_Commisnull
  end interface
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node

  call mpi_init(status)
  nio_node = -1
  status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'S', C_NULL_FUNPTR)
  print *,'in pseudo IO server'
  if(IOSERVER_Commisnull(model))    print *,'model    communicator is NULL'
  if(IOSERVER_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(IOSERVER_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(IOSERVER_Commisnull(serverio)) print *,'serverio communicator is NULL'
  call mpi_finalize(status)
end program
