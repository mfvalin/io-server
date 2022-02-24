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

module ioserver_mpi_f08
  !> Replacement (hopefully temporary) of the mpi_f08 module, which is not supported by all compilers, some of which are
  !> needed to compile GEM.

  implicit none
  include 'mpif.h'

  private

  public :: MPI_COMM_NULL, MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED
  public :: MPI_ADDRESS_KIND, MPI_INFO_NULL, MPI_INTEGER, MPI_INTEGER8, MPI_SUM, MPI_MIN
  public :: MPI_LOCK_SHARED, MPI_MODE_NOCHECK, MPI_LOCK_EXCLUSIVE

  public :: MPI_Wtime

  public :: MPI_Comm, MPI_Win, MPI_Group, MPI_Info, MPI_Status

  type :: MPI_Comm
    integer :: mpi_val
  contains
    procedure :: assign_int
    procedure :: assign_int8
    procedure :: assign_comm
    GENERIC :: ASSIGNMENT(=) => assign_int, assign_comm

    procedure :: is_not_equal_int
    generic:: operator(.ne.) => is_not_equal_int
  end type MPI_Comm

  type :: MPI_Win
    integer :: mpi_val
  end type MPI_Win

  type :: MPI_Group
    integer :: mpi_val
  end type MPI_Group

  type :: MPI_Info
    integer :: mpi_val
  contains
    procedure :: mpi_info_assign_int
    generic :: assignment(=) => mpi_info_assign_int
  end type MPI_Info

  type :: MPI_Status
    integer :: mpi_val
  end type MPI_Status

contains

subroutine assign_int(this, communicator)
  implicit none
  class(MPI_Comm), intent(inout) :: this
  integer,         intent(in)    :: communicator
  this % mpi_val = communicator
end subroutine assign_int

subroutine assign_int8(this, communicator)
  implicit none
  class(MPI_Comm), intent(inout) :: this
  integer(kind=8), intent(in)    :: communicator
  this % mpi_val = communicator
end subroutine assign_int8

subroutine assign_comm(this, comm)
  implicit none
  class(MPI_Comm), intent(inout) :: this
  class(MPI_Comm), intent(in)    :: comm
  this % mpi_val = comm % mpi_val
end subroutine assign_comm

function is_not_equal_int(this, val) result(is_not_equal)
  implicit none
  class(MPI_Comm), intent(in) :: this
  integer, intent(in) :: val
  logical :: is_not_equal
  is_not_equal = (this % mpi_val .ne. val)
end function is_not_equal_int

subroutine mpi_info_assign_int(this, val)
  implicit none
  class(MPI_Info), intent(inout) :: this
  integer, intent(in) :: val
  this % mpi_val = val
end subroutine mpi_info_assign_int

end module ioserver_mpi_f08
