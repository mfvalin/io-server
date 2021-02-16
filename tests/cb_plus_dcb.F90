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

program pseudomodelandserver
  use ISO_C_BINDING
  implicit none
  external io_relay
  include 'io-server/ioserver.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node, modelio, me
  integer :: comm, rank, size, nserv, ierr, noops, node_rank, node_size
  logical :: error
  character(len=128) :: arg

  call mpi_init(status)

  call IOSERVER_debug(1)            ! activate debug mode

  ! 3 arguments:
  !   1. number of server processes
  !   2. number of no-op processes (should be on the server)
  !   3. number of io relay processes per model (compute) node
  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg,*)noops
  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg,*) nserv
  nserv = nserv + noops
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)

  call get_local_world(comm, rank, size)
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/2, GBYTE)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then
    read(arg,*) nio_node ! relay processes per node
    call set_IOSERVER_relay(io_relay)
    status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'M')
    call MPI_Comm_rank(model, rank, ierr)
    call MPI_Comm_size(model, size, ierr)
    write(6,*)'in pseudo model, PE',rank+1,' of',size
  else  ! ranks 0, 1, nserv-1 : server
    nio_node = -1
    if(rank < noops) then ! ranks below noops are NO-OP processes
      status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'Z')
    else
      status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'O')
    endif
    call MPI_Comm_rank(serverio, rank, ierr)
    call MPI_Comm_size(serverio, size, ierr)
    write(6,*)'in pseudo IO server, PE',rank+1,' of',size
  endif

  if(IOSERVER_Commisnull(model))          write(6,*)'model    communicator is NULL'
  if(.not. IOSERVER_Commisnull(model))    write(6,*)'model    communicator is DEFINED'
  if(IOSERVER_Commisnull(modelio))        write(6,*)'modelio  communicator is NULL'
  if(.not. IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is DEFINED'
  if(IOSERVER_Commisnull(allio))          write(6,*)'allio    communicator is NULL'
  if(.not. IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is DEFINED'
  if(IOSERVER_Commisnull(nodeio))         write(6,*)'nodeio   communicator is NULL'
  if(.not. IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is DEFINED'
  if(IOSERVER_Commisnull(serverio))       write(6,*)'serverio communicator is NULL'
  if(.not. IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is DEFINED'

777 continue
  call IOSERVER_time_to_quit()
  call mpi_finalize(status)

end program

subroutine io_relay(model, modelio, allio, nodeio, serverio, nodecom)
  use ISO_C_BINDING
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, nodecom, modelio
  include 'io-server/ioserver.inc'
  integer :: rank, size, ierr
  call MPI_Comm_rank(nodeio, rank, ierr)
  call MPI_Comm_size(nodeio, size, ierr)
  write(6,*)'in pseudo io-relay, PE',rank+1,' of',size
  if(IOSERVER_Commisnull(model))    write(6,*)'model    communicator is NULL'
  if(.not. IOSERVER_Commisnull(model))    write(6,*)'model    communicator is DEFINED'
  if(IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is NULL'
  if(.not. IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is DEFINED'
  if(IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is NULL'
  if(.not. IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is DEFINED'
  if(IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is NULL'
  if(.not. IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is DEFINED'
  if(IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is NULL'
  if(.not. IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is DEFINED'
end subroutine io_relay

subroutine get_local_world(comm, rank, size)
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  integer, intent(OUT) :: comm, rank, size
  integer :: ierr
  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)
end subroutine get_local_world
