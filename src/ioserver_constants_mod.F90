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

module ioserver_constants
  use ISO_C_BINDING
  use ioserver_mpi_f08
  implicit none

  integer(C_SIZE_T), parameter :: KBYTE = 1024
  integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
  integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

  integer, parameter :: NO_COLOR              = 0
  integer, parameter :: MODEL_COLOR           = 1
  integer, parameter :: RELAY_COLOR           = 2
  integer, parameter :: SERVER_COLOR          = 4
  integer, parameter :: SERVER_BOUND_COLOR    = 8
  integer, parameter :: MODEL_BOUND_COLOR     = 16
  integer, parameter :: CHANNEL_COLOR         = 32
  integer, parameter :: GRID_PROCESSOR_COLOR  = 64
  integer, parameter :: NODE_COLOR            = 4096
  integer, parameter :: NO_OP_COLOR           = 8192   ! MUST BE THE HIGHEST VALUE

  type :: comm_rank_size
    type(MPI_Comm) :: comm = MPI_Comm(MPI_COMM_NULL)
    integer :: rank = -1
    integer :: size = 0
  contains
    procedure, pass :: is_null => IOserver_is_CRS_null
  end type
  ! type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(MPI_COMM_NULL, -1, 0)

  type, bind(C) :: qualified_address
    type(C_PTR)    :: p                   ! address (C pointer)
    integer(C_INT) :: color               ! PE color (MODEL_COLOR | RELAY_COLOR | NODE_COLOR)
    integer(C_INT) :: rank                ! pe rank in above color
  end type

  interface comm_rank_size
    procedure :: new_comm_rank_size
  end interface comm_rank_size
contains

!> Custom constructor for comm_rank_size (needed for compatibility with ioserver_mpi_f08 module)
function new_comm_rank_size(comm, rank, size)
  implicit none
  integer, intent(in)  :: comm, rank, size
  type(comm_rank_size) :: new_comm_rank_size
  new_comm_rank_size % comm = comm
  new_comm_rank_size % rank = rank
  new_comm_rank_size % size = size
end function new_comm_rank_size

function IOserver_is_CRS_null(crs) result(status)   !  is this a NULL communicator combo ?
  implicit none
  class(comm_rank_size), intent(inout) :: crs
  logical :: status
  status = (crs % rank < 0 .or. crs % size <= 0)
  if (status) crs % comm = MPI_COMM_NULL
end function IOserver_is_CRS_null

function is_color_relay(color)
  implicit none
  integer, intent(in) :: color
  logical :: is_color_relay
  is_color_relay = iand(color, RELAY_COLOR) == RELAY_COLOR
end function is_color_relay

function is_color_server(color)
  implicit none
  integer, intent(in) :: color
  logical :: is_color_server
  is_color_server = iand(color, SERVER_COLOR) == SERVER_COLOR
end function is_color_server

function is_color_model(color)
  implicit none
  integer, intent(in) :: color
  logical :: is_color_model
  is_color_model = iand(color, MODEL_COLOR) == MODEL_COLOR
end function is_color_model

end module ioserver_constants
