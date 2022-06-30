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


module dcb_edge_case_1_mod
  use ISO_C_BINDING
  use ioserver_mpi
  use distributed_circular_buffer_module

  public

  integer(C_SIZE_T), parameter :: NUM_CB_BYTES = 200 * 8

contains

  subroutine channel_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    logical :: success

    success = dcb % start_listening()
    if (.not. success) error stop 1
  end subroutine channel_process

  subroutine server_bound_server_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    integer(C_INT64_T) :: capacity 
    integer(C_INT64_T), dimension(:), allocatable :: msg
    logical :: success

    capacity = dcb % get_capacity(0, CB_KIND_INTEGER_8)
    allocate(msg(capacity))
    ! print *, '(Consumer) capacity: ', capacity
    !------------------------
    call dcb % full_barrier()
    !------------------------
    success = dcb % get_elems(0, msg, capacity, CB_KIND_INTEGER_8, .true.)
    if (.not. success) then
      print *, 'AAAhhh error with get()'
      error stop 1
    end if
    !------------------------
    call dcb % full_barrier()
    !------------------------
    success = dcb % get_elems(0, msg, 1_8, CB_KIND_INTEGER_8, .true.)

    ! call dcb % print(.true.)
    
  end subroutine server_bound_server_process

  subroutine server_bound_client_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    integer(C_INT64_T), dimension(:), allocatable :: msg
    integer(C_INT64_T) :: capacity, num_spaces
    logical :: success
    integer :: i

    capacity = dcb % get_capacity(CB_KIND_INTEGER_8)

    if (capacity < 0) then
      print *, 'AAAhhh capacity is bad!'
      error stop 1
    end if

    ! print *, '(Producer) capacity: ', capacity
    allocate(msg(capacity))

    ! print *, 'producer id: ', dcb % get_producer_id()

    msg(:) = 1
    msg(capacity) = 123

    num_spaces = dcb % get_num_spaces(CB_KIND_INTEGER_8, .false.)
    if (num_spaces .ne. capacity) then
      print *, 'Buffer is empty, but num spaces != capacity!!!!'
      error stop 1
    end if

    ! Fill the buffer so that the internal pointers (indices) reach exactly their upper limit
    do i = 1, int(capacity, 4)
      success = dcb % put_elems(msg(i), 1_8, CB_KIND_INTEGER_8, .true.)
      num_spaces = dcb % get_num_spaces(CB_KIND_INTEGER_8, .false.)
      if (num_spaces .ne. capacity - i) then
        print *, 'Wrong number of spaces left...', num_spaces, capacity - i
        call dcb % print(0_8)
        error stop 1
      end if
    end do
    !------------------------
    call dcb % full_barrier()
    !------------------------
    success = dcb % put_elems(321, 1_8, CB_KIND_INTEGER_8, .true.)
    !------------------------
    call dcb % full_barrier()
    !------------------------

  end subroutine server_bound_client_process

end module dcb_edge_case_1_mod

program dcb_edge_case_1
  use dcb_edge_case_1_mod
  use distributed_circular_buffer_module
  use ioserver_mpi
  implicit none

  type(distributed_circular_buffer) :: dcb
  integer :: global_rank, global_size
  integer :: server_comm, producer_comm
  logical :: success
  integer :: channel_id, server_consumer_id, server_bound_client_id
  integer :: ierr

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, global_size, ierr)

  if (global_size .ne. 3) then
    print *, 'Need exactly 3 processes for this test'
    error stop 1
  end if

  ! Create the communicators (needed for the server only)
  if (global_rank == 0) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, server_comm, ierr)
    success = dcb % create_bytes(MPI_COMM_WORLD, server_comm, DCB_SERVER_BOUND_TYPE, NUM_CB_BYTES, 0_8)
  else if (global_rank == 1) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, server_comm, ierr)
    success = dcb % create_bytes(MPI_COMM_WORLD, server_comm, DCB_CHANNEL_TYPE, 0_8, 0_8)
  else if (global_rank == 2) then
    call MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, producer_comm, ierr)
    success = dcb % create_bytes(MPI_COMM_WORLD, MPI_COMM_NULL, DCB_SERVER_BOUND_TYPE, 0_8, 0_8)
  else
    print *, 'Error'
    error stop 1
  end if

  if (.not. success) then
    print *, 'ERROR, could not create DCB!'
    error stop 1
  end if

  server_consumer_id = dcb % get_server_bound_server_id()
  server_bound_client_id = dcb % get_server_bound_client_id()
  channel_id  = dcb % get_channel_id()

  ! Run appropriate code, according to role
  if (server_consumer_id >= 0) then
    call server_bound_server_process(dcb)
  else if (server_bound_client_id >= 0) then
    call server_bound_client_process(dcb)
  else if (channel_id >= 0) then
    call channel_process(dcb)
  else
    error stop 1
  end if

  print *, 'Got here! (before deleting the DCB)', server_consumer_id, server_bound_client_id, channel_id
  call dcb % delete()
  ! print *, 'Got here! (after deleting the DCB)', server_consumer_id, server_bound_client_id, channel_id

  call MPI_Finalize(ierr)

end program dcb_edge_case_1
