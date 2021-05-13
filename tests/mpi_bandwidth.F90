
module mpi_bw_mod

  implicit none
  include 'mpif.h'

  integer(MPI_ADDRESS_KIND), parameter :: MAX_MESSAGE_SIZE = 500000
  integer(MPI_ADDRESS_KIND), parameter :: MESSAGE_WINDOW_SIZE = MAX_MESSAGE_SIZE * 4
  integer, parameter :: MAX_NUM_MESSAGES = 1000
  integer, parameter :: MAX_NUM_CHANNELS = 12

contains

  function am_server_node(node_comm, node_rank)
    implicit none
    ! include 'mpif.h'

    integer, intent(out) :: node_comm, node_rank
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

end module mpi_bw_mod

program mpi_bandwidth
  use mpi_bw_mod
  use io_timer_module
  use ISO_C_BINDING
  implicit none
  ! include 'mpif.h'

  interface
    function run_test(msg_comm, msg_size, msg_count, window, target_channel, num_channels, offset, msg_buffer, global_rank, num_relays, on_server) result(gb_per_s)
      use io_timer_module
      use ISO_C_BINDING
      use mpi_bw_mod
      implicit none

      integer, intent(in) :: msg_comm
      integer, intent(in) :: msg_size, msg_count
      integer, intent(in) :: window
      integer, intent(in) :: target_channel, num_channels
      integer(MPI_ADDRESS_KIND), intent(in) :: offset
      integer, dimension(MAX_MESSAGE_SIZE), intent(in) :: msg_buffer
      integer, intent(in) :: global_rank, num_relays
      logical, intent(in) :: on_server

      real :: gb_per_s
    end function run_test
  end interface

  integer :: ierror

  integer :: global_rank
  integer :: node_rank, node_comm, msg_comm
  logical :: on_server

  integer :: msg_comm_size, msg_rank
  integer :: window
  integer :: total_num_relays, num_target_relays, target_channel
  integer, allocatable, dimension(:, :) :: channel_buffer

  integer(MPI_ADDRESS_KIND) :: offset, window_size
  integer, dimension(MAX_MESSAGE_SIZE) :: relay_buffer

  type(io_timer) :: small_timer, medium_timer, large_timer
  real(C_DOUBLE) :: total_time

  integer, parameter :: MAX_TOTAL_DATA = 100000000
  integer, parameter :: NUM_MESSAGE_SIZES = 4
  integer, dimension(NUM_MESSAGE_SIZES) :: message_sizes
  real, dimension(NUM_MESSAGE_SIZES)    :: rates
  integer :: num_messages
  integer :: i, i_num_channel

  message_sizes(1) = 500
  message_sizes(2) = 5000
  message_sizes(3) = 50000
  message_sizes(4) = 500000

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierror)

  on_server = am_server_node(node_comm, node_rank)

  if (on_server .and. node_rank < MAX_NUM_CHANNELS) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, msg_comm, ierror)
  else if (.not. on_server .and. node_rank == 0) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank + 1000000, msg_comm, ierror)
  else
    call  MPI_Comm_split(MPI_COMM_WORLD, 1, 0, msg_comm, ierror)
    goto 777
  end if

  call MPI_Comm_size(msg_comm, msg_comm_size, ierror)
  call MPI_Comm_rank(msg_comm, msg_rank, ierror)
  total_num_relays = msg_comm_size - MAX_NUM_CHANNELS

  if (on_server) then
    num_target_relays = total_num_relays ! / NUM_CHANNELS
    ! if (msg_rank < mod(total_num_relays, NUM_CHANNELS)) num_target_relays = num_target_relays + 1
    allocate(channel_buffer(MAX_MESSAGE_SIZE, num_target_relays))

    ! print *, 'Allocating (channel) window: ', MESSAGE_WINDOW_SIZE * num_target_relays, msg_rank
    call MPI_Win_create(channel_buffer, MESSAGE_WINDOW_SIZE * num_target_relays, 4, MPI_INFO_NULL, msg_comm, window, ierror)
    ! print *, 'Channel window created', msg_rank

  else
    target_channel = mod(msg_rank, MAX_NUM_CHANNELS)
    ! print *, 'Allocating window, target_channel, msg_rank', target_channel, msg_rank
    window_size = 0
    ! offset = (msg_rank / NUM_CHANNELS - 1) * MAX_MESSAGE_SIZE
    offset = (msg_rank - MAX_NUM_CHANNELS) * MAX_MESSAGE_SIZE
    call MPI_Win_create(relay_buffer, window_size, 4, MPI_INFO_NULL, msg_comm, window, ierror)
    ! print *, 'Relay window created'

  end if

  call MPI_Barrier(msg_comm, ierror)

  if (global_rank == 0) then
    print '(A22, I8, I8, I8, I8)', '# channel \ msg sizes', message_sizes(1:4)
  end if
  do i_num_channel = 1, MAX_NUM_CHANNELS
    do i = 1, NUM_MESSAGE_SIZES
      num_messages   = MAX_TOTAL_DATA / message_sizes(i)
      target_channel = mod(msg_rank, i_num_channel)
      rates(i) = run_test(msg_comm, message_sizes(i), num_messages, window, target_channel, i_num_channel, offset, relay_buffer, global_rank, total_num_relays, on_server)
    end do

    if (global_rank == 0) then
      print '(I22, F8.2, F8.2, F8.2, F8.2)', i_num_channel, rates(1:4)
    end if
  end do

777 continue
  call MPI_Finalize(ierror)

end program mpi_bandwidth

function run_test(msg_comm, msg_size, msg_count, window, target_channel, num_channels, offset, msg_buffer, global_rank, num_relays, on_server) result(gb_per_s)
  use io_timer_module
  use ISO_C_BINDING
  use mpi_bw_mod
  implicit none

  interface
    function print_stats(time, num_relays, channel_count, msg_size, msg_count) result(gb_per_s)
      use ISO_C_BINDING
      implicit none
      real(C_DOUBLE), intent(in) :: time
      integer, intent(in)        :: num_relays, channel_count
      integer, intent(in)        :: msg_size, msg_count
      real :: gb_per_s
    end function print_stats
  end interface

  integer, intent(in) :: msg_comm
  integer, intent(in) :: msg_size, msg_count
  integer, intent(in) :: window
  integer, intent(in) :: target_channel, num_channels
  integer, intent(in) :: offset
  integer, dimension(MAX_MESSAGE_SIZE), intent(in) :: msg_buffer
  integer, intent(in) :: global_rank, num_relays
  logical, intent(in) :: on_server

  real :: gb_per_s

  integer :: ierror
  type(io_timer) :: timer
  real(C_DOUBLE) :: total_time

  if (global_rank == 0) then
    call timer % create()
  end if

  call MPI_Barrier(msg_comm, ierror)

  if (global_rank == 0) call timer % start()

  if (.not. on_server) then
    call relay_process(msg_comm, msg_size, msg_count, window, target_channel, offset, msg_buffer)
  end if

  call MPI_Barrier(msg_comm, ierror)

  if (global_rank == 0) then
    call timer % stop()
    total_time = timer % get_time_ms()
    gb_per_s = print_stats(total_time, num_relays, num_channels, msg_size, msg_count)
  end if

end function run_test

subroutine relay_process(msg_comm, msg_size, msg_count, window, target_channel, offset, msg_buffer)
  use mpi_bw_mod
  implicit none

  integer, intent(in) :: msg_comm
  integer, intent(in) :: msg_size, msg_count
  integer, intent(in) :: window
  integer, intent(in) :: target_channel
  integer, intent(in) :: offset
  integer, dimension(MAX_MESSAGE_SIZE), intent(in) :: msg_buffer
  integer :: ierror
  integer :: i

  do i = 1, msg_count
    call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, target_channel, 0, window, ierror)
    call MPI_Put(msg_buffer, msg_size, MPI_INTEGER, target_channel, offset, MAX_MESSAGE_SIZE, MPI_INTEGER, window, ierror)
    call MPI_Win_unlock(target_channel, window, ierror)
  end do

end subroutine relay_process

function print_stats(time, num_relays, channel_count, msg_size, msg_count) result(gb_per_s)
  use ISO_C_BINDING
  implicit none
  real(C_DOUBLE), intent(in) :: time
  integer, intent(in)        :: num_relays, channel_count
  integer, intent(in)        :: msg_size, msg_count

  real :: total_data, total_data_kb, total_data_mb, total_data_gb
  real :: total_per_s, mb_per_s, gb_per_s
  real :: time_s, time_ms

  time_ms = time
  time_s = time_ms / 1000.0

  total_data = msg_count * real(msg_size) * num_relays * 4.0
  total_data_kb = total_data / 1024.0
  total_data_mb = total_data_kb / 1024.0
  total_data_gb = total_data_mb / 1024.0

  total_per_s = total_data / time_s
  mb_per_s = total_per_s / (1024.0 * 1024.0)
  gb_per_s = mb_per_s / 1024.0

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
