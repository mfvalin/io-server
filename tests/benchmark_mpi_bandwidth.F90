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

module mpi_bw_mod
  use ISO_C_BINDING
  use ioserver_mpi
  implicit none

  integer, parameter :: MAX_MESSAGE_SIZE = 500000
  integer(MPI_ADDRESS_KIND), parameter :: MESSAGE_WINDOW_SIZE = MAX_MESSAGE_SIZE * 4
  integer,                   parameter :: MAX_MESSAGE_SIZE_BYTES = MAX_MESSAGE_SIZE * 4
  integer, parameter :: MAX_NUM_MESSAGES = 1000
  integer, parameter :: MAX_NUM_CHANNELS = 12

  integer :: LOCK_TYPE = MPI_LOCK_EXCLUSIVE
  integer :: LOCK_ASSERT = 0

contains

  function am_server_node(node_comm, node_rank)
    implicit none

    integer, intent(out) :: node_comm
    integer, intent(out) :: node_rank
    logical :: am_server_node

    integer :: global_rank,  node_root_global_rank
    integer :: ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm, ierr)
    call MPI_Comm_rank(node_comm, node_rank, ierr)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm, ierr)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.
  end function am_server_node

  function compute_data_element(sender_id, elem_id) result(data_element)
    implicit none
    integer, intent(in) :: sender_id, elem_id
    integer :: data_element

    data_element = sender_id * MAX_MESSAGE_SIZE + elem_id
  end function compute_data_element

  subroutine allocate_shared_mem_array(comm, num_relays, array_size, array)
    use test_helper_module
    implicit none
    integer, intent(in) :: comm
    integer, intent(in) :: num_relays, array_size
    integer, pointer, dimension(:,:), intent(out) :: array

    integer :: rank, size
    integer :: ierr

    type(C_PTR)        :: mem
    integer(C_INT)     :: shmid
    integer(C_INT64_T) :: total_size

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, size, ierr)

    if (rank == 0) then
      total_size = num_relays * array_size
      mem = memory_allocate_shared(shmid, total_size)
    end if

    call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, comm, ierr)
    if (rank .ne. 0) then
      mem = memory_address_from_id(shmid)
    end if

    call c_f_pointer(mem, array, shape = [array_size / 4, num_relays])

  end subroutine allocate_shared_mem_array

  function print_stats(time, num_relays, channel_count, msg_size, msg_count) result(gb_per_s)
    use ISO_C_BINDING
    implicit none
    real(C_DOUBLE), intent(in) :: time
    integer, intent(in)        :: num_relays, channel_count
    integer, intent(in)        :: msg_size, msg_count

    real :: gb_per_s

    real(8) :: total_data, total_data_kb, total_data_mb, total_data_gb
    real(8) :: total_per_s, mb_per_s
    real(8) :: time_s, time_ms

    if (channel_count < 0) then
    end if

    time_ms = time
    time_s = time_ms / 1000.0

    total_data = msg_count * real(msg_size) * num_relays * 4.0
    total_data_kb = total_data / 1024.0
    total_data_mb = total_data_kb / 1024.0
    total_data_gb = total_data_mb / 1024.0

    total_per_s = total_data / time_s
    mb_per_s = total_per_s / (1024.0 * 1024.0)
    gb_per_s = real(mb_per_s / 1024.0, 4)

    ! print *, ''
    ! print '(A20)', 'Results'
    ! print '(A20, I8, A20, I8, A20, I8)', 'Num channels:', channel_count, 'Num relays:', num_relays, 'Num messages:', msg_count
    ! print '(A20, I8)', 'Message size:', msg_size
    ! if (total_data_mb < 1.0) then
    !   print '(A20, F8.3, A3)', 'Total data:', total_data_kb, ' kB'
    ! else if (total_data_gb < 1.0) then
    !   print '(A20, F8.3, A3)', 'Total data:', total_data_mb, ' MB'
    ! else
    !   print '(A20, F8.3, A3)', 'Total data:', total_data_gb, ' GB'
    ! end if
    ! if (time_s < 1.0) then
    !   print '(A20, F7.2, A3)', 'Total time:', time_ms, ' ms'
    ! else
    !   print '(A20, F7.2, A2)', 'Total time:', time_s, ' s'
    ! end if
    ! if (gb_per_s < 1.0) then
    !   print '(A20, F7.2, A5)', 'Rate:', mb_per_s, ' MB/s'
    ! else
    !   print '(A20, F7.2, A5)', 'Rate:', gb_per_s, ' GB/s'
    ! end if
  end function print_stats

  function run_test(msg_comm, msg_size, msg_count, window, target_channel, num_channels, offset, msg_buffer, &
                    on_server, channel_buffer) result(gb_per_s)
    use ioserver_timer_module
    implicit none

    integer, intent(in) :: msg_comm
    integer, intent(in) :: msg_size, msg_count
    integer, intent(in) :: window
    integer, intent(in) :: target_channel, num_channels
    integer(MPI_ADDRESS_KIND), intent(in) :: offset
    integer, dimension(MAX_MESSAGE_SIZE), intent(inout) :: msg_buffer
    logical, intent(in) :: on_server
    integer, dimension(:,:), intent(inout) :: channel_buffer

    real :: gb_per_s

    integer :: i, j, ierr
    type(ioserver_timer) :: timer
    real(C_DOUBLE) :: total_time
    integer :: rank, size
    integer :: num_relays

    call MPI_Comm_rank(msg_comm, rank, ierr)
    call MPI_Comm_size(msg_comm, size, ierr)

    num_relays = size - MAX_NUM_CHANNELS

    if (rank == 0) then
      call timer % create()
    end if

    ! Just a test message to verify that the data is sent properly
    if (.not. on_server) then
      call relay_process(msg_comm, msg_size, 1, window, target_channel, offset, msg_buffer)
    end if

    call MPI_Barrier(msg_comm, ierr)

    ! Verify that everyone on the server can read the correct sent data
    if (on_server) then
      do i = 1, num_relays
        do j = 1, msg_size
          if (channel_buffer(j, i) .ne. compute_data_element(i + MAX_NUM_CHANNELS - 1, j)) then
            print *, 'AHHHHh got the wrong data!', channel_buffer(j, i), compute_data_element(i + MAX_NUM_CHANNELS - 1, j)
            print *, 'for relay, channel ', i, rank
            error stop 1
          end if
        end do
      end do
    end if

    call MPI_Barrier(msg_comm, ierr)

    if (rank == 0) call timer % start()

    if (.not. on_server) then
      call relay_process(msg_comm, msg_size, msg_count, window, target_channel, offset, msg_buffer)
    end if

    call MPI_Barrier(msg_comm, ierr)

    if (rank == 0) then
      call timer % stop()
      total_time = timer % get_time_ms()
      gb_per_s = print_stats(total_time, num_relays, num_channels, msg_size, msg_count)
    end if

  end function run_test

  subroutine run_test_set(msg_comm, msg_count, window, offset, msg_buffer, &
                        global_rank, num_relays, on_server, channel_buffer, message_sizes, num_msg_sizes,  &
                        channel_counts, num_channel_counts)
    implicit none
    integer, intent(in) :: msg_comm
    integer, intent(in) :: msg_count
    integer, intent(in) :: window
    integer(MPI_ADDRESS_KIND), intent(in) :: offset
    integer, dimension(MAX_MESSAGE_SIZE), intent(inout) :: msg_buffer
    integer, intent(in) :: global_rank, num_relays
    logical, intent(in) :: on_server
    integer, dimension(:,:), intent(inout) :: channel_buffer
    integer, intent(in) :: num_msg_sizes
    integer, dimension(num_msg_sizes), intent(in) :: message_sizes
    integer, intent(in) :: num_channel_counts
    integer, dimension(num_channel_counts), intent(in) :: channel_counts

    integer, parameter :: MAX_TOTAL_DATA = 50000000

    integer :: rank
    integer :: i, i_num_channel, ierr
    integer :: num_channels, num_messages, target_channel

    real, dimension(num_msg_sizes) :: rates

    if (global_rank < 0) print *, num_relays, msg_count

    call MPI_Comm_rank(msg_comm, rank, ierr)

    if (rank == 0) then
      print '(A22, I8, I8, I8, I8, I8, I8)', '# channel \ msg sizes', message_sizes(:)
    end if

    do i_num_channel = 1, num_channel_counts
      num_channels = channel_counts(i_num_channel)
      do i = 1, num_msg_sizes
        num_messages   = MAX_TOTAL_DATA / message_sizes(i)
        target_channel = mod(rank, num_channels)
        rates(i) = run_test(msg_comm, message_sizes(i), num_messages, window, target_channel, num_channels, offset, &
                            msg_buffer, on_server, channel_buffer)
      end do

      if (rank == 0) then
        print '(I22, F8.2, F8.2, F8.2, F8.2, F8.2, F8.2)', num_channels, rates(:)
      end if
    end do

  end subroutine run_test_set

  subroutine relay_process(msg_comm, msg_size, msg_count, window, target_channel, offset, msg_buffer)
    implicit none

    integer, intent(in) :: msg_comm
    integer,        intent(in) :: msg_size, msg_count
    integer,  intent(in) :: window
    integer,        intent(in) :: target_channel
    integer(MPI_ADDRESS_KIND), intent(in) :: offset
    integer, dimension(MAX_MESSAGE_SIZE), intent(inout) :: msg_buffer
    integer :: i, rank, ierr

    if (msg_count == 1) then
      call MPI_Comm_rank(msg_comm, rank, ierr)
      ! print *, 'Sending message from rank ', rank
      do i = 1, msg_size
        msg_buffer(i) = compute_data_element(rank, i)
      end do
    end if

    do i = 1, msg_count
      call MPI_Win_lock(LOCK_TYPE, target_channel, LOCK_ASSERT, window, ierr)
      call MPI_Put(msg_buffer, msg_size, MPI_INTEGER, target_channel, offset, MAX_MESSAGE_SIZE, MPI_INTEGER, window, ierr)
      call MPI_Win_unlock(target_channel, window, ierr)
    end do

  end subroutine relay_process

end module mpi_bw_mod

program mpi_bandwidth
  use mpi_bw_mod
  use ioserver_timer_module
  use ISO_C_BINDING
  implicit none

  integer :: global_rank, node_rank
  integer :: node_comm, msg_comm
  logical :: on_server

  integer :: msg_comm_size, msg_rank
  integer :: window
  integer :: total_num_relays, target_channel
  ! integer, allocatable, dimension(:, :) :: channel_buffer
  integer, pointer, dimension(:, :) :: shared_buffer

  integer(MPI_ADDRESS_KIND) :: offset, window_size
  integer, dimension(:), allocatable :: relay_buffer

  ! type(ioserver_timer) :: small_timer, medium_timer, large_timer
  ! real(C_DOUBLE) :: total_time

  integer, parameter :: NUM_MESSAGE_SIZES = 6
  integer, dimension(NUM_MESSAGE_SIZES) :: message_sizes
  ! real, dimension(NUM_MESSAGE_SIZES)    :: rates
  integer :: num_messages !, num_channels
  ! integer :: i, i_num_channel
  integer :: ierr

  integer, parameter :: NUM_CHANNEL_COUNTS = 8
  integer, dimension(NUM_CHANNEL_COUNTS) :: channel_counts

  allocate(relay_buffer(MAX_MESSAGE_SIZE))

  message_sizes(1) = 500
  message_sizes(2) = 5000
  message_sizes(3) = 20000
  message_sizes(4) = 50000
  message_sizes(5) = 200000
  message_sizes(6) = 500000

  channel_counts(1) = 1
  channel_counts(2) = 2
  channel_counts(3) = 3
  channel_counts(4) = 4
  channel_counts(5) = 6
  channel_counts(6) = 8
  channel_counts(7) = 10
  channel_counts(8) = 12

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)

  on_server = am_server_node(node_comm, node_rank)

  if (on_server .and. node_rank < MAX_NUM_CHANNELS) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, msg_comm, ierr)
  else if (.not. on_server .and. node_rank == 0) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank + 1000000, msg_comm, ierr)
  else
    call  MPI_Comm_split(MPI_COMM_WORLD, 1, 0, msg_comm, ierr)
    goto 777
  end if

  call MPI_Comm_size(msg_comm, msg_comm_size, ierr)
  call MPI_Comm_rank(msg_comm, msg_rank, ierr)
  total_num_relays = msg_comm_size - MAX_NUM_CHANNELS

  ! Allocate shared memory on the server
  if (on_server) then
    call allocate_shared_mem_array(node_comm, total_num_relays, MAX_MESSAGE_SIZE_BYTES, shared_buffer)
    if (msg_rank == 0) then
      shared_buffer(:, :) = -1
    end if
  end if

  if (on_server) then
    ! allocate(channel_buffer(MAX_MESSAGE_SIZE, total_num_relays))

    ! print *, 'Allocating (channel) window: ', MESSAGE_WINDOW_SIZE * num_target_relays, msg_rank
    ! call MPI_Win_create(channel_buffer, MESSAGE_WINDOW_SIZE * total_num_relays, 4, MPI_INFO_NULL, msg_comm, window, ierror)
    call MPI_Win_create(shared_buffer, MESSAGE_WINDOW_SIZE * total_num_relays, 4, MPI_INFO_NULL, msg_comm, window, ierr)
    ! print *, 'Channel window created', msg_rank

  else
    target_channel = mod(msg_rank, MAX_NUM_CHANNELS)
    ! print *, 'Allocating window, target_channel, msg_rank', target_channel, msg_rank
    window_size = 0
    offset = (msg_rank - MAX_NUM_CHANNELS) * MAX_MESSAGE_SIZE
    print *, 'msg rank, offset', msg_rank, offset
    call MPI_Win_create(relay_buffer, window_size, 4, MPI_INFO_NULL, msg_comm, window, ierr)
    ! print *, 'Relay window created'

  end if

  call MPI_Barrier(msg_comm, ierr)

  if (global_rank == 0) then
    print *, total_num_relays, 'relays'
  end if

  LOCK_TYPE = MPI_LOCK_EXCLUSIVE
  LOCK_ASSERT = 0
  if (global_rank == 0) print *, 'MPI_LOCK_EXCLUSIVE, assert = 0'
  call run_test_set(msg_comm, num_messages, window, offset, &
                    relay_buffer, global_rank, total_num_relays, on_server, shared_buffer, message_sizes, &
                    NUM_MESSAGE_SIZES, channel_counts, num_channel_counts)

  LOCK_TYPE = MPI_LOCK_SHARED
  LOCK_ASSERT = 0
  if (global_rank == 0) print *, 'MPI_LOCK_SHARED, assert = 0'
  call run_test_set(msg_comm, num_messages, window, offset, &
                    relay_buffer, global_rank, total_num_relays, on_server, shared_buffer, message_sizes, &
                    NUM_MESSAGE_SIZES, channel_counts, num_channel_counts)

  LOCK_TYPE = MPI_LOCK_SHARED
  LOCK_ASSERT = MPI_MODE_NOCHECK
  if (global_rank == 0) print *, 'MPI_LOCK_SHARED, assert = MPI_MODE_NOCHECK'
  call run_test_set(msg_comm, num_messages, window, offset, &
                    relay_buffer, global_rank, total_num_relays, on_server, shared_buffer, message_sizes, &
                    NUM_MESSAGE_SIZES, channel_counts, num_channel_counts)

  LOCK_TYPE = MPI_LOCK_EXCLUSIVE
  LOCK_ASSERT = MPI_MODE_NOCHECK
  if (global_rank == 0) print *, 'MPI_LOCK_EXCLUSIVE, assert = MPI_MODE_NOCHECK'
  call run_test_set(msg_comm, num_messages, window, offset, &
                    relay_buffer, global_rank, total_num_relays, on_server, shared_buffer, message_sizes, &
                    NUM_MESSAGE_SIZES, channel_counts, num_channel_counts)

  ! if (global_rank == 0) then
  !   print '(A22, I8, I8, I8, I8, I8, I8)', '# channel \ msg sizes', message_sizes(:)
  ! end if

  ! do i_num_channel = 1, NUM_CHANNEL_COUNTS
  !   num_channels = channel_counts(i_num_channel)
  !   do i = 1, NUM_MESSAGE_SIZES
  !     num_messages   = MAX_TOTAL_DATA / message_sizes(i)
  !     target_channel = mod(msg_rank, num_channels)
  !     rates(i) = run_test(msg_comm, message_sizes(i), num_messages, window, target_channel, num_channels, offset, &
  !                         relay_buffer, global_rank, total_num_relays, on_server, shared_buffer)
  !   end do

  !   if (global_rank == 0) then
  !     print '(I22, F8.2, F8.2, F8.2, F8.2, F8.2, F8.2)', num_channels, rates(:)
  !   end if
  ! end do

777 continue
  call MPI_Finalize(ierr)
  deallocate(relay_buffer)

end program mpi_bandwidth



