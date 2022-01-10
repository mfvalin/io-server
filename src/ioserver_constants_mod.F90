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
  use mpi_f08
  implicit none

  integer(C_SIZE_T), parameter :: KBYTE = 1024
  integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
  integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

  integer, parameter :: NO_COLOR            = 0
  integer, parameter :: MODEL_COLOR         = 1
  integer, parameter :: RELAY_COLOR         = 2
  integer, parameter :: SERVER_COLOR        = 4
  integer, parameter :: SERVER_BOUND_COLOR  = 8
  integer, parameter :: MODEL_BOUND_COLOR   = 16
  ! integer, parameter :: INOUT_COLOR   = INPUT_COLOR + OUTPUT_COLOR
  integer, parameter :: CHANNEL_COLOR       = 32
  integer, parameter :: NODE_COLOR          = 4096
  integer, parameter :: NO_OP_COLOR         = 8192   ! MUST BE THE HIGHEST VALUE

  integer, parameter :: IO_CONTROL   = 1000
  integer, parameter :: IO_BASE      = 1001
  integer, parameter :: IO_RELAY     = 1002
  integer, parameter :: IO_SERVER    = 1003

  type :: comm_rank_size
    type(MPI_Comm) :: comm = MPI_COMM_NULL
    integer :: rank = -1
    integer :: size = 0
  contains
    procedure, pass :: is_null => IOserver_is_CRS_null
  end type
  type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(MPI_COMM_NULL, -1, 0)

  type, bind(C) :: qualified_address
    type(C_PTR)    :: p                   ! address (C pointer)
    integer(C_INT) :: color               ! PE color (MODEL_COLOR | RELAY_COLOR | NODE_COLOR)
    integer(C_INT) :: rank                ! pe rank in above color
  end type

contains

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
