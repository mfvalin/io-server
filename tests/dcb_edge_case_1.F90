
module dcb_edge_case_1_mod
  use ISO_C_BINDING
  use mpi_f08
  use distributed_circular_buffer_module

  public

  integer, parameter :: NUM_CB_ELEMENTS = 200

contains

  subroutine channel_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    integer :: return_value

    return_value = dcb % start_listening()
    if (return_value .ne. 0) error stop 1
  end subroutine channel_process

  subroutine consumer_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    integer :: capacity 
    integer, dimension(:), allocatable :: msg
    integer :: num

    capacity = dcb % get_capacity(0)
    allocate(msg(capacity))
    ! print *, '(Consumer) capacity: ', capacity
    !------------------------
    call dcb % full_barrier()
    !------------------------
    num = dcb % get(0, msg, capacity, .true.)
    if (num < 0) then
      print *, 'AAAhhh error with get()'
      error stop 1
    end if
    !------------------------
    call dcb % full_barrier()
    !------------------------
    num = dcb % get(0, msg, 1, .true.)

    call dcb % print(.true.)
    
  end subroutine consumer_process

  subroutine producer_process(dcb)
    implicit none
    type(distributed_circular_buffer), intent(inout) :: dcb

    integer, dimension(:), allocatable :: msg
    integer :: capacity
    integer :: num_spaces
    integer :: i

    capacity = dcb % get_capacity()
    ! print *, '(Producer) capacity: ', capacity
    allocate(msg(capacity))

    ! print *, 'producer id: ', dcb % get_producer_id()

    msg(:) = 1
    msg(capacity) = 123

    num_spaces = dcb % get_num_spaces(.false.)
    
    if (num_spaces .ne. capacity) then
      print *, 'Buffer is empty, but num spaces != capacity!!!!'
      error stop 1
    end if

    ! Fill the buffer so that the internal pointers (indices) reach exactly their upper limit
    do i = 1, capacity
      num_spaces = dcb % put(msg(i), 1, .true.)
      if (num_spaces .ne. capacity - i) then
        print *, 'Wrong number of spaces left...', num_spaces, capacity - i
        call dcb % print(.false.)
        error stop 1
      end if
    end do
    !------------------------
    call dcb % full_barrier()
    !------------------------
    num_spaces = dcb % put(321, 1, .true.)
    !------------------------
    call dcb % full_barrier()
    !------------------------

  end subroutine producer_process

end module dcb_edge_case_1_mod

program dcb_edge_case_1
  use dcb_edge_case_1_mod
  use distributed_circular_buffer_module
  use mpi_f08
  implicit none

  type(distributed_circular_buffer) :: dcb
  integer :: global_rank, global_size
  type(MPI_Comm) :: server_comm, producer_comm
  logical :: success
  integer :: channel_id, consumer_id, producer_id

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
  call MPI_Comm_size(MPI_COMM_WORLD, global_size)

  if (global_size .ne. 3) then
    print *, 'Need exactly 3 processes for this test'
    error stop 1
  end if

  ! Create the communicators (needed for the server only)
  if (global_rank == 0 .or. global_rank == 1) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, global_rank, server_comm)
    success = dcb % create(MPI_COMM_WORLD, server_comm, 1, 1, NUM_CB_ELEMENTS)
  else if (global_rank == 2) then
    call MPI_Comm_split(MPI_COMM_WORLD, 1, global_rank, producer_comm)
    success = dcb % create(MPI_COMM_WORLD, MPI_COMM_NULL, 0, 0, 0)
  else
    print *, 'Error'
    error stop 1
  end if

  if (.not. success) error stop 1

  consumer_id = dcb % get_consumer_id()
  producer_id = dcb % get_producer_id()
  channel_id  = dcb % get_channel_id()

  ! Run appropriate code, according to role
  if (consumer_id >= 0) then
    call consumer_process(dcb)
  else if (producer_id >= 0) then
    call producer_process(dcb)
  else if (channel_id >= 0) then
    call channel_process(dcb)
  else
    error stop 1
  end if

  call dcb % delete()

  call MPI_Finalize()

end program dcb_edge_case_1
