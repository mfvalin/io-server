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


module ioserver_mpi_f08_internal
  implicit none
  include 'mpif.h'
  private

  public :: MPI_COMM_NULL, MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED
  public :: MPI_ADDRESS_KIND, MPI_INFO_NULL, MPI_INTEGER, MPI_INTEGER8, MPI_SUM, MPI_MIN
  public :: MPI_LOCK_SHARED, MPI_MODE_NOCHECK, MPI_LOCK_EXCLUSIVE

  public :: MPI_Wtime

  public :: MPI_Comm, MPI_Win, MPI_Group, MPI_Info, MPI_Status, MPI_Datatype

  public :: MPI_Init_ioserver, MPI_Comm_rank_ioserver, MPI_Comm_size_ioserver, MPI_Comm_split_type_ioserver, MPI_Send_ioserver

  type :: MPI_Comm
    integer :: mpi_val
  contains
    procedure :: assign_int
    procedure :: assign_int8
    procedure :: assign_comm
    GENERIC :: ASSIGNMENT(=) => assign_int, assign_comm

    procedure :: is_not_equal
    generic:: operator(.ne.) => is_not_equal
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

  type :: MPI_Datatype
    integer :: mpi_val
  end type MPI_Datatype

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
  this % mpi_val = int(communicator, kind=4)
end subroutine assign_int8

subroutine assign_comm(this, comm)
  implicit none
  class(MPI_Comm), intent(inout) :: this
  class(MPI_Comm), intent(in)    :: comm
  this % mpi_val = comm % mpi_val
end subroutine assign_comm

function is_not_equal(this, other)
  implicit none
  class(MPI_Comm), intent(in) :: this
  class(MPI_Comm), intent(in) :: other
  logical :: is_not_equal
  is_not_equal = (this % mpi_val .ne. other % mpi_val)
end function is_not_equal

subroutine mpi_info_assign_int(this, val)
  implicit none
  class(MPI_Info), intent(inout) :: this
  integer, intent(in) :: val
  this % mpi_val = val
end subroutine mpi_info_assign_int

subroutine MPI_Init_ioserver()
  implicit none
  integer :: error
  call MPI_Init(error)
end subroutine MPI_Init_ioserver

subroutine MPI_Comm_rank_ioserver(comm, rank)
  implicit none
  type(MPI_Comm), intent(in)  :: comm
  integer,        intent(out) :: rank
  integer :: error
  call  MPI_Comm_rank(comm % mpi_val, rank, error)
end subroutine MPI_Comm_rank_ioserver

subroutine MPI_Comm_size_ioserver(comm, size)
  implicit none
  type(MPI_Comm), intent(in)  :: comm
  integer,        intent(out) :: size
  integer :: error
  call  MPI_Comm_size(comm % mpi_val, size, error)
end subroutine MPI_Comm_size_ioserver

subroutine MPI_Comm_split_type_ioserver(comm, split_type, key, info, new_comm)
  implicit none
  type(MPI_Comm), intent(in)  :: comm
  integer,        intent(in)  :: split_type, key
  type(MPI_Info), intent(in)  :: info
  type(MPI_Comm), intent(out) :: new_comm
  integer :: error
  call MPI_Comm_split_type(comm % mpi_val, split_type, key, info % mpi_val, new_comm % mpi_val, error)
end subroutine MPI_Comm_split_type_ioserver

subroutine MPI_Send_ioserver(buf, count, datatype, dest, tag, comm)
  implicit none
  type(*), dimension(:), intent(in) :: buf
  integer, intent(in) :: count, dest, tag
  type(MPI_Datatype), intent(in) :: datatype
  type(MPI_Comm), intent(in) :: comm
  integer :: error

  call MPI_Send(buf, count, datatype, dest, tag, comm, error)
end subroutine MPI_Send_ioserver


! MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
!     TYPE(*), DIMENSION(..), INTENT(IN) :: buf
!     INTEGER, INTENT(IN) :: count, dest, tag
!     TYPE(MPI_Datatype), INTENT(IN) :: datatype
!     TYPE(MPI_Comm), INTENT(IN) :: comm
!     INTEGER, OPTIONAL, INTENT(OUT) :: ierror

! void FPMPI(send,SEND)(void *buf, int *count, int *datatype, int *dest, int *tag,

! void FPMPI(irecv,IRECV)(void *buf, int *count, int *datatype, int *source, int *tag,
! void FPMPI(type_size,TYPE_SIZE)(int *datatype, int *dsize, int *ierr);
! void FPMPI(init_thread, INIT_THREAD)(int *required, int *provided, int *ierr);
! void FPMPI(recv,RECV)(void *buf, int *count, int *datatype, int *source, int *tag,
! void FPMPI(isend,ISEND)(void *buf, int *count, int *datatype, int *dest, int *tag,
! void FPMPI(reduce,REDUCE)(void *sendbuf, void *recvbuf, int *count,
! void FPMPI(allreduce,ALLREDUCE)(void *sendbuf, void *recvbuf, int *count,
! void FPMPI(bcast,BCAST)(void* buffer, int *count, int *datatype, int *root, int *comm, int *ierr);
! void FPMPI(barrier,BARRIER)( int *comm, int *ierr);
! void FPMPI(testall,TESTALL)(int *count,int *array_of_requests,int *flag,int *array_of_statuses, int *ierr);
! void FPMPI(wait,WAIT)(int *array_of_requests,int *array_of_statuses, int *ierr);
! void FPMPI(waitall,WAITALL)(int *count,int *array_of_requests,int *array_of_statuses, int *ierr);
! void FPMPI(sendrecv,SENDRECV)(void *sendbuf, int *sendcount, int *sendtype,
! void FPMPI(scatter,SCATTER)(void *sendbuf, int *sendcnt, int *sendtype, 
! void FPMPI(scatterv,SCATTERV)(void *sendbuf, int *sendcnts, int *displs, int *sendtype, 
! void FPMPI(gather,GATHER)(void *sendbuf, int *sendcnt, int *sendtype, 
! void FPMPI(gatherv,GATHERV)(void *sendbuf, int *sendcnt, int *sendtype, void *recvbuf, 
! void FPMPI(alltoall,ALLTOALL)(void *sendbuf, int *sendcount, int *sendtype, 
! void FPMPI(alltoallv,ALLTOALLV)(void *sendbuf, int *sendcnts, int *sdispls, int *sendtype, 
! void FPMPI(get, GET)(void *origin_buf, int *origin_count, int *origin_datatype, int *target_rank, MPI_Aint *target_disp,
! void FPMPI(put, PUT)(const void *origin_buf, int *origin_count, int *origin_datatype, int *target_rank,
! void FPMPI(accumulate, ACCUMULATE)(const void *origin_addr, int *origin_count, int *origin_datatype,
! void FPMPI(get_accumulate, GET_ACCUMULATE)(const void *origin_addr, int *origin_count, int *origin_datatype,
! void FPMPI(win_fence, WIN_FENCE)(int *assert, int *window, int *ierr);
! void FPMPI(win_start, WIN_START)(int *group, int *assert, int *window, int *ierr);
! void FPMPI(win_complete, WIN_COMPLETE)(int *window, int *ierr);
! void FPMPI(win_post, WIN_POST)(int *group, int *assert, int *window, int *ierr);
! void FPMPI(win_wait, WIN_WAIT)(int *window, int *ierr);
! void FPMPI(win_lock, WIN_LOCK)(int *lock_type, int *rank, int *assert, int *window, int *ierr);
! void FPMPI(win_lock_all, WIN_LOCK_ALL)(int *assert, int *window, int *ierr);
! void FPMPI(win_unlock, WIN_UNLOCK)(int *rank, int *window, int *ierr);
! void FPMPI(win_unlock_all, WIN_UNLOCK_ALL)(int *window, int *ierr);
! void FPMPI(win_flush, WIN_FLUSH)(int *rank, int *window, int *ierr);
! void FPMPI(win_flush_local, WIN_FLUSH_LOCAL)(int *rank, int *window, int *ierr);
! void FPMPI(win_flush_all, WIN_FLUSH_ALL)(int *window, int *ierr);
! void FPMPI(win_flush_local_all, WIN_FLUSH_LOCAL_ALL)(int *window, int *ierr);
end module ioserver_mpi_f08_internal

module ioserver_mpi_f08_do_not_use
  !> Replacement (hopefully temporary) of the mpi_f08 module, which is not supported by all compilers, some of which are
  !> needed to compile GEM.
  use ioserver_mpi_f08_internal,                            &
      MPI_Init            => MPI_Init_ioserver,             &
      MPI_Comm_rank       => MPI_Comm_rank_ioserver,        &
      MPI_Comm_size       => MPI_Comm_size_ioserver,        &
      MPI_Comm_split_type => MPI_Comm_split_type_ioserver,  &
      MPI_Send            => MPI_Send_ioserver,             &
      MPI_COMM_WORLD_mpi  => MPI_COMM_WORLD,                &
      MPI_COMM_NULL_mpi   => MPI_COMM_NULL
  implicit none

  private

  type(MPI_Comm), parameter :: MPI_COMM_WORLD = MPI_Comm(MPI_COMM_WORLD_mpi)
  type(MPI_Comm), parameter :: MPI_COMM_NULL  = MPI_Comm(MPI_COMM_NULL_mpi)

  public :: MPI_COMM_NULL, MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED
  public :: MPI_ADDRESS_KIND, MPI_INFO_NULL, MPI_INTEGER, MPI_INTEGER8, MPI_SUM, MPI_MIN
  public :: MPI_LOCK_SHARED, MPI_MODE_NOCHECK, MPI_LOCK_EXCLUSIVE

  public :: MPI_Wtime

  public :: MPI_Comm, MPI_Win, MPI_Group, MPI_Info, MPI_Status
  public :: MPI_Init, MPI_Comm_rank, MPI_Comm_size

contains
end module ioserver_mpi_f08_do_not_use

