! Copyright (C) 2021  Environnement et Changement climatique Canada
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

module cb_plus_dcb_parameters
  use ISO_C_BINDING
  use mpi_f08
  implicit none

  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 128

  integer(C_SIZE_T), parameter :: CB_SIZE_BYTES             = 1040 * 4;
  integer,           parameter :: CB_MESSAGE_SIZE_INT       = 100              ! Size of each data batch put in a CB in number of integers
  integer,           parameter :: CB_TOTAL_DATA_TO_SEND_INT = 10000000         ! How much total data to send (for each CB, in integers)
  integer,           parameter :: MAX_DCB_MESSAGE_SIZE_INT  = CB_MESSAGE_SIZE_INT * 500        ! Size of each data batch put in the DCB
  integer(C_SIZE_T), parameter :: DCB_SIZE_BYTES            = MAX_DCB_MESSAGE_SIZE_INT * 4 * 5 ! Number of bytes in each buffer of the DCB

  logical :: CHECK_CB_MESSAGES
  logical :: CHECK_DCB_MESSAGES

  integer :: num_channels

contains

  function compute_data_point(global_rank, index) result(data_point)
    implicit none
    integer, intent(in) :: global_rank
    integer, intent(in) :: index
    integer :: data_point

    data_point = mod(global_rank, 20000) * 100000 + mod(index, 100000)
  end function compute_data_point

  function am_server_node(node_rank)
    implicit none

    integer, intent(out) :: node_rank
    logical :: am_server_node

    type(MPI_Comm) :: node_comm
    integer :: global_rank, node_root_global_rank

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm)
    call MPI_Comm_rank(node_comm, node_rank)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.
  end function am_server_node
end module cb_plus_dcb_parameters

#include <serializer.hf>

program pseudomodelandserver
  use ISO_C_BINDING
  use shmem_arena_mod
  use cb_plus_dcb_parameters
  implicit none
  external io_relay_process
  external io_server_process
  integer :: status, input
  integer :: me, nio_node
  type(MPI_Comm) :: comm
  integer :: rank, size, nserv
  character(len=128) :: arg
  type(shmem_arena) :: ma
  type(comm_rank_size) :: fullnode_crs, local_crs

  ! integer, parameter :: NUM_NODES = 3
  logical :: server_node
  integer :: node_rank

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

!  call IOSERVER_debug(1)            ! activate debug mode

  ! arg = '0'
  ! if(COMMAND_ARGUMENT_COUNT() >= 5) call GET_COMMAND_ARGUMENT(5, arg)
  ! read(arg,*)noops

  ! Arguments
  ! 1. Check messages or not
  ! 2. Number of consumer processes
  ! 3. Number of channel processes
  ! 4. Number of relay processes per node

  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)
  read(arg,*) nserv
  nserv = nserv ! + noops

  arg = '1'
  if (COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg, *) num_channels
  nserv = nserv + num_channels

  CHECK_CB_MESSAGES = .false.
  CHECK_DCB_MESSAGES = .false.
  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg, *) input
  if (input > 0) then
    CHECK_CB_MESSAGES = .true.
    CHECK_DCB_MESSAGES = .true.
  end if

  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 4) call GET_COMMAND_ARGUMENT(4, arg)
  read(arg,*) nio_node                    ! number of relay processes per node

  call get_local_world(comm, rank, size)
  me = ma % setid(rank)

  server_node = am_server_node(node_rank)

  ! if(rank >= nserv) then
  ! if(mod(rank, NUM_NODES) .ne. 0) then
  if (.not. server_node) then
    ! =============================================================================================
    !                                 compute or IO relay Processes
    ! =============================================================================================
    call set_IOSERVER_relay(io_relay_process)
    !  no return from ioserver_int_init in the case of IO relay processes when io_relay_fn is defined
    !  compute processes will return from call
    status = ioserver_init(nio_node, 'M')
    ! =============================================================================================
    !                                 compute Processes
    ! =============================================================================================
    !  from this point on, this is a model compute process

    call model_process()
    write(6,*)'END: compute, PE',rank+1,' of',size

  else            ! ranks 0, 1,..., nserv-1 :
    ! =============================================================================================
    !                                server and no-op processes
    ! =============================================================================================
    nio_node = -1
    ! if(rank < noops) then          ! ranks below noops are NO-OP processes
    ! if(rank/NUM_NODES >= nserv) then          ! ranks below noops are NO-OP processes
    if (node_rank >= nserv) then
      ! =============================================================================================
      !                                no-op processes
      ! =============================================================================================
      ! no return from ioserver_int_init in the case of NO-OP processes
      status = ioserver_init(nio_node, 'Z')
    else
      ! =============================================================================================
      !                                server processes (usually on another node)
      ! =============================================================================================
      call set_IOserver_server(io_server_process)
      status = ioserver_init(nio_node, 'O')   ! this function should not return as set_IOserver_server is set
      ! io_server_out may or may not return from call
!      call io_server_out()
!      noderank = server_crs % rank
!      nodesize = server_crs % size
    endif

  endif

  call ioserver_set_time_to_quit()
!  write(6,*)'FINAL: serv node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program


subroutine io_server_process()
  use cb_plus_dcb_parameters
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use io_server_mod
  implicit none

  type(MPI_Comm) :: global_comm
  integer :: global_rank, global_size
  type(distributed_circular_buffer) :: data_buffer

  call get_local_world(global_comm, global_rank, global_size)
  call io_server_mod_init()

  ! write(6, *) 'Server process! PE', server_crs % rank + 1, ' of', server_crs % size, ' global:', global_rank + 1

  ! Create the DCB used for this test
  data_buffer = IOserver_get_dcb()

  ! Choose what to do based on whether we are a consumer or a channel process
  if (data_buffer % get_consumer_id() >= 0) then
    call consumer_process(data_buffer)
  else
    write(6, *) 'We have a problem'
    error stop 1
  end if

end subroutine io_server_process

subroutine io_server_out()
end subroutine io_server_out

subroutine channel_process(data_buffer)
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use ISO_C_BINDING
  implicit none
  type(distributed_circular_buffer), intent(inout) :: data_buffer
  integer(C_INT) :: result

  ! The channel doesn't do anything other than listening
  result = data_buffer % start_listening()

  if (result .ne. 0) then
    write (6, *) 'The receiver loop did not terminate properly!!!'
    write (6, *) 'Terminating with error from SERVER (channel) process'
    error stop 1
  end if

end subroutine channel_process

subroutine consumer_process(data_buffer)
  use cb_plus_dcb_parameters
  use distributed_circular_buffer_module
  implicit none

  interface
    function c_gethostid() result(h) bind(C,name='gethostid') ! SMP host identifier
      import :: C_LONG
      integer(C_LONG) :: h
    end function c_gethostid
  end interface

  type(distributed_circular_buffer), intent(inout) :: data_buffer

  integer, parameter :: WRITE_BUFFER_SIZE = 50000

  integer :: consumer_id, num_producers, num_consumers
  integer :: i_producer, i_data_check
  integer :: message_size
  integer, dimension(:), allocatable :: message, expected_message
  integer, dimension(:), allocatable :: file_write_buffer
  integer :: file_write_position, write_size
  logical :: finished, success
  integer :: num_errors = 0

  logical, allocatable :: producer_activated(:)

  integer           :: file_unit
  character(len=14) :: file_name

  allocate(message(MAX_DCB_MESSAGE_SIZE_INT))
  allocate(expected_message(MAX_DCB_MESSAGE_SIZE_INT))
  allocate(file_write_buffer(WRITE_BUFFER_SIZE))

  consumer_id   = data_buffer % get_consumer_id()
  num_consumers = data_buffer % get_num_consumers()
  num_producers = data_buffer % get_num_producers()

  allocate(producer_activated(num_producers))

  file_write_position = 1

  write(file_name,'(A6, I4.4, A4)') 'SERVER', consumer_id, '.out'
  open(newunit = file_unit, file = file_name, status = 'replace', form = 'unformatted')

  ! Should receive one test signal
  do i_producer = consumer_id, num_producers - 1, num_consumers
    success = data_buffer % get_elems(i_producer, message, 2_8, CB_KIND_INTEGER_4, .true.)
    if (.not. success) then
      print *, 'Could not even receive test signal!'
      error stop 1
    end if
    ! write (6, *) 'Received HI from relay #, local ID #, global rank #', i_producer, message(1), message(2)
    producer_activated(i_producer + 1) = .false.
    if (message(1) == 0) producer_activated(i_producer + 1) = .true.
  end do

  ! Now, we repeatedly loop through all buffers this consumer is responsible for
  ! If the buffer is empty, just go on to the next
  ! If the buffer has something, the first value is the size of the data block to read, so we read it
  ! If the first value to read is 0, it means this buffer is done and won't send anything anymore
  ! When all buffer have a first value of 0, the entire test is finished
  finished = .false.
  do while (.not. finished)
    finished = .true.
    do i_producer = consumer_id, num_producers - 1, num_consumers

      ! Skip relays that do not produce data
      if (.not. producer_activated(i_producer + 1)) cycle

      ! The buffer is empty, so it has not finished sending stuff. Just move on to the next
      if (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) == 0) then
        finished = .false.
        cycle
      end if

      success = data_buffer % peek_elems(i_producer, message_size, 1_8, CB_KIND_INTEGER_4)

      if (message_size > 0)  then
        ! There is something in the buffer!
        finished = .false.
        success = data_buffer % get_elems(i_producer, message, INT(message_size + 1, C_SIZE_T), CB_KIND_INTEGER_4, .true.)


        write_size = message_size / 4
        if (file_write_position + write_size >= WRITE_BUFFER_SIZE) then
          write(file_unit) file_write_buffer(1:file_write_position)
          file_write_position = 1
        end if

        file_write_buffer(file_write_position:file_write_position+write_size) = message(1:write_size)
        file_write_position = file_write_position + write_size
      else if (message_size == 0) then
        ! The buffer will not send anything more
      else
        ! This should not happen
        write (6, *) 'message_size: ', message_size
        num_errors = num_errors + 1
      end if

      if (CHECK_DCB_MESSAGES) then
        expected_message(1:3) = message(1:3)
        do i_data_check = 1, message_size - 2
           expected_message(i_data_check + 3) = compute_data_point(message(2), message(3) + i_data_check)
        end do
        if (.not. all(expected_message(1:message_size + 1) == message(1:message_size + 1))) then
          num_errors = num_errors + 1
        end if
      end if

    end do
  end do

  write(file_unit) file_write_buffer(1:file_write_position)
  close(file_unit)
  file_write_position = 1

  ! Final check on the buffers' content
  do i_producer = consumer_id, num_producers - 1, num_consumers
    if (producer_activated(i_producer + 1) .and. (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) .ne. 1)) then
      num_errors = num_errors + 1
      write (6, *) 'ERROR: buffer should be empty at the end of test'
    end if
  end do

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from SERVER (consumer) process'
    error stop 1
  end if

  deallocate(message)
  deallocate(expected_message)
  deallocate(file_write_buffer)

end subroutine consumer_process

subroutine model_pseudo_write(cb, array, n)
  use data_serialize
  use circular_buffer_module
  implicit none
  type(circular_buffer), intent(inout) :: cb
  integer, intent(in) :: n
  integer, dimension(n), intent(inout) :: array

  type(jar) :: parcel
  integer :: status
  logical :: success
  integer, dimension(:), pointer :: blind_array

  status = parcel % shape(array(2:n), n-1)
  JAR_DATA(parcel, blind_array)
  success = cb % put(n-1, 1_8, CB_KIND_INTEGER_4, .false.)
  success = cb % put(blind_array, INT(n-1, C_SIZE_T), CB_KIND_INTEGER_4, .true.)
  ! status = cb % atomic_put(array(1:n), n, .true.)
end subroutine model_pseudo_write

subroutine model_process()
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module
  use rpn_extra_module, only: sleep_us
  use io_common_mod
  use ioserver_internal_mod, only: IOserver_get_relay_shmem
  implicit none

  type(MPI_Comm)        :: global_comm
  integer               :: global_rank, global_size
  type(comm_rank_size)  :: node_crs, local_compute_crs
  integer               :: local_compute_id
  integer               :: num_errors, ierr
  type(circular_buffer) :: data_buffer
  logical               :: success
  character(len=8)      :: compute_name
  integer               :: bsize, bflags
  type(C_PTR)           :: tmp_ptr
  type(C_PTR) :: node_shmem

  integer, dimension(CB_MESSAGE_SIZE_INT) :: message

  call get_local_world(global_comm, global_rank, global_size)

  num_errors        = 0
  node_crs          = IOserver_get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  local_compute_crs = IOserver_get_crs(NODE_COLOR + MODEL_COLOR)
  local_compute_id  = local_compute_crs % rank

  node_shmem = IOserver_get_relay_shmem()
  tmp_ptr = ma % clone(node_shmem)

  ! write(6, *) 'Model process! PE', node_crs % rank + 1, ' of', node_crs % size, ' global:', global_rank + 1

  ! Create a single CB in our allocated space in shared memory
  write(compute_name,'(A4,I4.4)') "MCIO", local_compute_id
  tmp_ptr = ma % getblock(bsize, bflags, compute_name)
  success = data_buffer % create_bytes(tmp_ptr, CB_SIZE_BYTES)
  if (.not. success) then
    num_errors = num_errors + 1
    write (6, *) 'AAAAHHHhhhh could not create CB from model PE ', node_crs % rank
    error stop 1
  end if

  ! NODE barrier to signal RELAY processes that the CB (from each MODEL) is ready
  call MPI_Barrier(node_crs % comm, ierr)

  ! Send a first test signal
  message(1) = global_rank
  success = data_buffer % put(message, 1_8, CB_KIND_INTEGER_4, .true.)

  ! Now send a bunch of messages
  ! First item  = number of elements in message
  ! Second item = global rank of the sender
  ! Third item  = position at which we are in the message sequence (how many data elements sent so far)
  message(1) = CB_MESSAGE_SIZE_INT
  message(2) = global_rank
  block
    integer :: i, j
    do i = 0, CB_TOTAL_DATA_TO_SEND_INT, CB_MESSAGE_SIZE_INT - 3
      message(3) = i ! Where we at
      ! Prepare the data to send
      do j = 1, CB_MESSAGE_SIZE_INT - 3
        message(j + 3) = compute_data_point(global_rank, i + j)
      end do
      ! Do the sending
      call model_pseudo_write(data_buffer, message(:), CB_MESSAGE_SIZE_INT)
      ! num_spaces = data_buffer % atomic_put(message, CB_MESSAGE_SIZE, .true.)
!      call sleep_us(50)
    end do
    ! We done. Send the stop signal
    message(1) = 0
    success = data_buffer % put(message, 1_8, CB_KIND_INTEGER_4, .true.)
  end block

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from MODEL process'
    error stop 1
  end if

  call MPI_Barrier(node_crs % comm, ierr) ! Sync with relays and avoid scrambling output

end subroutine model_process


subroutine io_relay_process()
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : circular_buffer
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use io_relay_mod
  implicit none

  type(MPI_Comm)       :: global_comm
  integer              :: global_rank, global_size
  type(comm_rank_size) :: local_relay_crs, local_model_crs
  integer              :: num_local_compute, local_relay_id, num_local_relays, producer_id
  integer              :: ierr, i_compute, index, num_errors
  character(len=8)     :: compute_name
  integer              :: bsize, bflags
  type(C_PTR)          :: tmp_ptr

  type(circular_buffer), dimension(MAX_NUM_WORKER_PER_NODE) :: local_data_buffers

  logical :: success = .false.
  type(distributed_circular_buffer) :: data_buffer

  call io_relay_mod_init()
  call get_local_world(global_comm, global_rank, global_size)

  local_relay_crs = IOserver_get_crs(RELAY_COLOR + NODE_COLOR)
  local_model_crs = IOserver_get_crs(MODEL_COLOR + NODE_COLOR)

  local_relay_id    = local_relay_crs % rank
  num_local_relays  = local_relay_crs % size
  num_local_compute = nodecom_crs % size - local_relay_crs % size
  num_errors        = 0

  ! write(6, *) 'Relay process! PE', nodecom_crs % rank + 1, ' of', nodecom_crs % size, ' global:', global_rank + 1

  ! Create the DCB used to communicate with the server
  data_buffer = IOserver_get_dcb()

  producer_id = data_buffer % get_producer_id()

  ! NODE barrier to allow MODEL processes to initialize their CBs
  call MPI_Barrier(nodecom_crs % comm, ierr)

  ! Recover all CBs that are stored on this node and check that they are valid (even if we don't necessarily access all of them)
  index = 1
  do i_compute = 0, num_local_compute - 1
    write(compute_name,'(A4,I4.4)') "MCIO", i_compute

    tmp_ptr = ma % getblock(bsize, bflags, compute_name)
    success = local_data_buffers(index) % create_bytes(tmp_ptr)

    if (success) then
      index = index + 1
    else
      num_errors = num_errors + 1
      print *, 'AAAAhhhh invalid buffer given...', i_compute
      error stop 1
    end if
  end do

  block
    
    integer :: i_data_check
    integer, dimension(CB_MESSAGE_SIZE_INT) :: cb_message, expected_message
    integer, dimension(:), allocatable :: dcb_message
    integer :: current_message_size, model_message_size
    logical :: finished = .false.

    allocate(dcb_message(MAX_DCB_MESSAGE_SIZE_INT))

    ! Say hi to the consumer processes
    dcb_message(1) = local_relay_id
    dcb_message(2) = global_rank
    success = data_buffer % put_elems(dcb_message, 2_8, CB_KIND_INTEGER_4, .true.)

    if (local_relay_id == 0) then ! only 1 relay per node sends output

      ! Get the initial test signal from each CB this relay is responsible for
      do i_compute = local_relay_id + 1, num_local_compute !, num_local_relays
        cb_message(:) = -1
        success = local_data_buffers(i_compute) % get(cb_message, 1_8, CB_KIND_INTEGER_4, .true.)
      end do

      ! The main loop
      expected_message(:) = -1
      current_message_size = 0
      do while (.not. finished)
        finished = .true.
        do i_compute = local_relay_id + 1, num_local_compute !, num_local_relays

          ! The buffer is empty, so it has not finished sending stuff. Move on to the next CB
          if (local_data_buffers(i_compute) % get_num_elements(CB_KIND_INTEGER_4) == 0) then
            finished = .false.
            cycle
          end if

          success = local_data_buffers(i_compute) % peek(model_message_size, 1_8, CB_KIND_INTEGER_4)
          if (model_message_size > 0) then
            ! There is something in the buffer!
            finished = .false.

            ! Read the content of the CB
            success = local_data_buffers(i_compute) % get(cb_message, INT(model_message_size + 1, C_SIZE_T), CB_KIND_INTEGER_4, .true.)

            ! If the DCB message buffer is too full to contain that new package, flush it now
            if (current_message_size + model_message_size + 1 > MAX_DCB_MESSAGE_SIZE_INT) then
              success = data_buffer % put_elems(dcb_message, INT(current_message_size, C_SIZE_T), CB_KIND_INTEGER_4, .true.)
              current_message_size = 0
            end if

            ! Copy the CB message to the DCB message buffer
            dcb_message(current_message_size + 1: current_message_size + model_message_size + 1) = cb_message(1:model_message_size + 1)
            current_message_size = current_message_size + model_message_size + 1

            if (CHECK_CB_MESSAGES) then
              expected_message(1:3) = cb_message(1:3)
              do i_data_check = 1, model_message_size - 2
                expected_message(i_data_check + 3) = compute_data_point(cb_message(2), cb_message(3) + i_data_check)
              end do
              if (.not. all(expected_message(1:model_message_size + 1) == cb_message(1:model_message_size + 1))) then
                num_errors = num_errors + 1
              end if
            end if
          end if
        end do
      end do

      dcb_message(current_message_size + 1) = 0 ! Put a 'stop' signal at the end of the DCB message buffer
      ! Send the stop signal along with the remaining data
      success = data_buffer % put_elems(dcb_message, INT(current_message_size + 1, C_SIZE_T), CB_KIND_INTEGER_4, .true.)
    end if

    deallocate(dcb_message)
  end block

  if (local_relay_id == 0) then
    do i_compute = 1, num_local_compute
      call local_data_buffers(i_compute) % print_stats(producer_id * 100 + i_compute - 1, i_compute == 1)
    end do
  end if

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from RELAY process'
    error stop 1
  end if

  call MPI_Barrier(nodecom_crs % comm, ierr) ! Sync with models and avoid scrambling output

end subroutine io_relay_process


subroutine get_local_world(comm, rank, size)
  use ISO_C_BINDING
  use mpi_f08
  implicit none
  type(MPI_Comm), intent(OUT) :: comm
  integer,        intent(OUT) :: rank, size

  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank)
  call MPI_Comm_size(comm, size)
end subroutine get_local_world

