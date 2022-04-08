
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
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

module dcb_timeout_module
  use ISO_C_BINDING
  use ioserver_mpi
  use distributed_circular_buffer_module
  use rpn_extra_module
  implicit none

  public

  integer(C_SIZE_T),  parameter :: NUM_CB_BYTES = 200 * 8
  integer(C_INT64_T), parameter :: TEST_VAL     = 543212345

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
    logical :: success
    integer(C_INT64_T) :: result
    !------------------------
    call dcb % full_barrier()
    !------------------------

    success = dcb % peek_elems(0, result, 1_8, CB_KIND_INTEGER_8, timeout_ms = 0)
    success = dcb % get_elems(0, result, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms = 4) .or. success
    if (success) then
      print *, 'ERROR: Get operation should have timed out!'
      error stop 1
    end if

    success = dcb % get_elems(0, result, 1_8, CB_KIND_INTEGER_8, .true.)
    if (.not. success .or. result .ne. TEST_VAL) then
      print *, 'ERROR: Unable to retrieve correct test value!'
    end if
    !------------------------
    call dcb % full_barrier()
    !------------------------
    !------------------------
    call dcb % full_barrier()
    !------------------------
    call sleep_us(100000)
    success = dcb % get_elems(0, result, 1_8, CB_KIND_INTEGER_8, .true.)
  end subroutine server_bound_server_process

  subroutine server_bound_client_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb
    logical :: success
    integer :: i

    !------------------------
    call dcb % full_barrier()
    !------------------------

    call sleep_us(100000)
    success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true.)

    !------------------------
    call dcb % full_barrier()
    !------------------------

    success = .true.
    do i = 1, dcb % get_capacity(CB_KIND_INTEGER_8)
      success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .and. success
    end do
    if (.not. success) then
      print *, 'ERROR: Unable to fill CB'
      error stop 1
    end if

    !------------------------
    call dcb % full_barrier()
    !------------------------

    success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1)
    success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .or. success
    success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .or. success
    if (success) then
      print *, 'ERROR: Should not be able to put anything in the buffer anymore'
      error stop 1
    end if

    success = dcb % put_elems(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms = -1)
    if (.not. success) then
      print *, 'ERROR: Should have been able to put data by now!'
      error stop 1
    end if
  end subroutine server_bound_client_process
end module dcb_timeout_module

program dcb_timeout
  use dcb_timeout_module
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

end program dcb_timeout