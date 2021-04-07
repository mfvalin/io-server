! Copyright (C) 2021  Environnement Canada
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
  implicit none

  ! integer(kind = MPI_ADDRESS_KIND), parameter :: CB_WINDOW_SIZE  = 10000
  integer, parameter :: NUM_CB_ELEMENTS = 128
  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 128
  integer, parameter :: CB_MESSAGE_SIZE = 100
  integer, parameter :: CB_TOTAL_DATA_TO_SEND = 100000
  integer, parameter :: MAX_DCB_MESSAGE_SIZE = CB_MESSAGE_SIZE * 21
  integer, parameter :: NUM_DCB_ELEMENTS = MAX_DCB_MESSAGE_SIZE * 5
  ! integer, parameter :: NUM_DCB_ELEMENTS = 200

  logical, parameter :: CHECK_CB_MESSAGES = .false.

contains

  function compute_data_point(global_rank, index) result(data_point)
    implicit none
    integer, intent(in) :: global_rank
    integer, intent(in) :: index
    integer :: data_point

    data_point = mod(global_rank, 20000) * 100000 + index
  end function compute_data_point

end module cb_plus_dcb_parameters


program pseudomodelandserver
  use ISO_C_BINDING
  use ioserver_functions
  use memory_arena_mod
  implicit none
  external io_relay_process
  external io_server_process
  integer :: status
!  integer :: model, allio, relay, server, nio_node, modelio, nodecom, me
  integer :: me, nio_node
  integer :: comm, rank, size, nserv, noops
  logical :: error
  character(len=128) :: arg
  type(memory_arena) :: ma
!  character(len=8) :: blockname
!  type(comm_rank_size) :: local_crs, model_crs, relay_crs, fullnode_crs
!  type(comm_rank_size) :: modelio_crs, allio_crs, server_crs, nodecom_crs
  type(comm_rank_size) :: fullnode_crs, local_crs

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

!  call IOSERVER_debug(1)            ! activate debug mode

  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg,*)noops
  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg,*) nserv
  nserv = nserv + noops
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)

  call get_local_world(comm, rank, size)
  me = ma % setid(rank)
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/4, GBYTE/2)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then
    ! =============================================================================================
    !                                 compute or IO relay Processes
    ! =============================================================================================
    read(arg,*) nio_node                    ! number of relay processes per node
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
    if(rank < noops) then          ! ranks below noops are NO-OP processes
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
777 continue
  call ioserver_set_time_to_quit()
!  write(6,*)'FINAL: serv node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program


subroutine io_server_process()
  use cb_plus_dcb_parameters
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use ioserver_functions
  implicit none

  type(comm_rank_size) :: server_crs, allio_crs
  integer :: global_comm, global_rank, global_size
  type(distributed_circular_buffer) :: data_buffer
  logical :: success

  call get_local_world(global_comm, global_rank, global_size)

  allio_crs    = IOserver_get_crs(RELAY_COLOR + SERVER_COLOR)
  server_crs   = IOserver_get_crs(SERVER_COLOR)

  write(6, *) 'Server process! PE', server_crs % rank + 1, ' of', server_crs % size, ' global:', global_rank + 1

  success = data_buffer % create(allio_crs % comm, server_crs % comm, allio_crs % size - server_crs % size, server_crs % size / 2, NUM_DCB_ELEMENTS)
  write(6, *) 'Server process successfully created DCB'

  if (data_buffer % get_consumer_id() >= 0) then
    call consumer_process(data_buffer)
  else if (data_buffer % get_channel_id() >= 0) then
    call channel_process(data_buffer)
  else
    write(6, *) 'We have a problem'
    error stop 1
  end if

  ! write(6, *) 'Server, deleting DCB', server_rank + 1
  call data_buffer % delete()

end subroutine io_server_process

subroutine io_server_out()
end subroutine io_server_out

subroutine channel_process(data_buffer)
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use ISO_C_BINDING
  implicit none
  type(distributed_circular_buffer), intent(inout) :: data_buffer
  integer(C_INT) :: result

  write(6, *) 'I am a receiver'
  result = data_buffer % start_listening()

  if (result .ne. 0) then
    write(6, *) 'The receiver loop did not terminate properly!!!'
  end if

  write(6, *) 'Channel done!'

end subroutine channel_process

subroutine consumer_process(data_buffer)
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none

  type(distributed_circular_buffer), intent(inout) :: data_buffer

  integer :: consumer_id, num_producers, num_consumers
  integer :: i_producer
  integer :: num_elements, num_iter
  integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: message
  logical :: finished

  write(6, *) 'I am a consumer'

  consumer_id   = data_buffer % get_consumer_id()
  num_consumers = data_buffer % get_num_consumers()
  num_producers = data_buffer % get_num_producers()

  do i_producer = consumer_id, num_producers - 1, num_consumers
    num_elements = data_buffer % get(i_producer, message, 2, .true.)
    write (6, *) 'Received HI from relay #, local ID #, global rank #', i_producer, message(1), message(2)
  end do

  finished = .false.
  num_iter = 0
  do while (.not. finished)
    finished = .true.
    do i_producer = consumer_id, num_producers - 1, num_consumers
      num_elements = data_buffer % get(i_producer, message, 1, .true.)

      if (message(1) > 0)  then
        finished = .false.
        num_elements = data_buffer % get(i_producer, message(2:), message(1) * (CB_MESSAGE_SIZE + 1), .true.)
      else
        write (6, *) 'Message(1): ', message(1)
      end if

    end do
    num_iter = num_iter + 1

    if (mod(num_iter, 100) == 0) write (6, *) 'Checked many times: ', num_iter
  end do

  write (6, *) 'Consumer done!'

end subroutine consumer_process


subroutine model_process()
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  use io_common_mod
  implicit none

  integer :: global_comm, global_rank, global_size
  type(comm_rank_size) :: node_crs, local_compute_crs
  integer :: local_compute_id
  integer :: num_errors, ierr
  type(circular_buffer) :: data_buffer
  logical :: success
  integer :: num_spaces
  character(len=8)     :: compute_name
  integer              :: bsize, bflags
  type(C_PTR)          :: tmp_ptr

  integer(DATA_ELEMENT), dimension(CB_MESSAGE_SIZE) :: message

  call get_local_world(global_comm, global_rank, global_size)

  num_errors        = 0
  node_crs          = IOserver_get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  local_compute_crs = IOserver_get_crs(NODE_COLOR + MODEL_COLOR)
  local_compute_id  = local_compute_crs % rank

  call IOSERVER_get_winmem(p_base, p_relay, p_server)
  tmp_ptr = ma % clone(p_relay)
  write(compute_name,'(A4,I4.4)') "MCIO", local_compute_id

  write(6, *) 'Model process! PE', node_crs % rank + 1, ' of', node_crs % size, ' global:', global_rank + 1

  tmp_ptr = ma % getblock(bsize, bflags, compute_name)
  success = data_buffer % create(tmp_ptr, NUM_CB_ELEMENTS)
  if (.not. success) then
    num_errors = num_errors + 1
    write (6, *) 'AAAAHHHhhhh could not create CB from model PE ', node_crs % rank
  end if

  call MPI_Barrier(node_crs % comm, ierr)
  
  ! write(6, *) '(model)', node_rank, ' Sending flag to rank', 0
  ! call MPI_Send(worker_flag, 1, MPI_INTEGER, 0, 0, node_comm, ierr)

  message(1) = global_rank
  num_spaces = data_buffer % atomic_put(message, 1, .true.)

  block
    integer :: i, j
    do i = 1, CB_TOTAL_DATA_TO_SEND, CB_MESSAGE_SIZE
      do j = 1, CB_MESSAGE_SIZE
        message(j) = compute_data_point(global_rank, i + j)
      end do
      num_spaces = data_buffer % atomic_put(message, CB_MESSAGE_SIZE, .true.)
    end do
  end block

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from MODEL process'
    error stop num_errors
  end if

  write (6, *) 'Model done!'

end subroutine model_process


subroutine io_relay_process()
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use io_relay_mod
  implicit none

  integer              :: global_comm, global_rank, global_size
  type(comm_rank_size) :: local_relay_crs
  integer              :: num_local_compute, local_relay_id, num_local_relays
  integer              :: ierr, i_compute, index, num_errors
  character(len=8)     :: compute_name
  integer              :: bsize, bflags
  type(C_PTR)          :: tmp_ptr

  type(circular_buffer), dimension(MAX_NUM_WORKER_PER_NODE) :: local_data_buffers
  integer,               dimension(MAX_NUM_WORKER_PER_NODE) :: worker_global_ranks

  logical :: success = .false.
  type(distributed_circular_buffer) :: data_buffer

  call io_relay_mod_init()
  call get_local_world(global_comm, global_rank, global_size)

  local_relay_crs   = IOserver_get_crs(RELAY_COLOR + NODE_COLOR)
  local_relay_id    = local_relay_crs % rank
  num_local_relays  = local_relay_crs % size
  num_local_compute = modelio_crs % size - relay_crs % size
  num_errors        = 0

  write(6, *) 'Relay process! PE', nodecom_crs % rank + 1, ' of', nodecom_crs % size, ' global:', global_rank + 1

  success = data_buffer % create(allio_crs % comm, MPI_COMM_NULL, -1, -1, -1)
  write(6, *) 'Relay process successfully created DCB'

  if (.not. success) then
    write(6, *) 'Unable to create DCB on relay process!!'
  end if

  write (6, *) 'Barrier on node com (relay)'
  call MPI_Barrier(nodecom_crs % comm, ierr)


  index = 1
  do i_compute = 0, num_local_compute - 1
    write(compute_name,'(A4,I4.4)') "MCIO", i_compute

    tmp_ptr = ma % getblock(bsize, bflags, compute_name)
    success = local_data_buffers(index) % create(tmp_ptr)

    if (success) then
      index = index + 1
    else
      num_errors = num_errors + 1
      print *, 'AAAAhhhh invalid buffer given...', i_compute
    end if
  end do

  block
    
    integer :: i_data_outer, i_worker_local, i_data_check
    integer(DATA_ELEMENT), dimension(CB_MESSAGE_SIZE) :: cb_message, expected_message
    integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: dcb_message
    integer :: dcb_message_size, msg_start
    integer :: num_elements, num_spaces

    do i_compute = local_relay_id + 1, num_local_compute, num_local_relays
      cb_message(:) = -1
      write(6, *) 'Reading data from compute ', i_compute - 1
      num_elements = local_data_buffers(i_compute) % atomic_get(cb_message, 1, .true.)
      write(6, *) 'Read data from compute ', i_compute - 1
      ! write(6, *) 'Relay ID, Buffer ID, Global rank:', local_relay_id, i_worker, message(1)
      worker_global_ranks(i_compute) = cb_message(1)
    end do

    ! Say hi to the consumer processes
    dcb_message(1) = local_relay_id
    dcb_message(2) = global_rank
    num_spaces = data_buffer % put(dcb_message, 2, .true.)

    ! Set up message header
    dcb_message_size = 1 + num_local_compute + num_local_compute * CB_MESSAGE_SIZE

    if (dcb_message_size > MAX_DCB_MESSAGE_SIZE) then
      write(6, *) 'DCB message size is larger than MAX_DCB_MESSAGE_SIZE !!!'
    end if

    dcb_message(1) = num_local_compute

    do i_data_outer = 1, CB_TOTAL_DATA_TO_SEND, CB_MESSAGE_SIZE
      i_worker_local = 1
      do i_compute = local_relay_id + 1, num_local_compute, num_local_relays
        dcb_message(i_worker_local + 1) = worker_global_ranks(i_compute)
        num_elements = local_data_buffers(i_compute) % atomic_get(cb_message, CB_MESSAGE_SIZE, .true.)


        msg_start = 1 + num_local_compute + (i_worker_local - 1) * CB_MESSAGE_SIZE
        dcb_message(msg_start:msg_start + CB_MESSAGE_SIZE - 1) = cb_message(:)

        if (CHECK_CB_MESSAGES) then
          do i_data_check = 1, CB_MESSAGE_SIZE
            expected_message(i_data_check) = compute_data_point(worker_global_ranks(i_compute), i_data_outer + i_data_check)
          end do
          if (.not. all(expected_message == cb_message)) then
            num_errors = num_errors + 1
   !         if (i_compute < 5) then
   !           write(6, *) 'GOT WRONG MESSAGE FROM WORKER # with global rank #', i_compute, worker_global_ranks(i_compute)
   !           write(6, *) cb_message
   !           write(6, *) 'Expected'
   !           write(6, *) expected_message
   !         end if
          end if
        end if

        i_worker_local = i_worker_local + 1
      end do

      write (6, *) 'Sending: ', dcb_message(1:4), dcb_message_size
      num_spaces = data_buffer % put(dcb_message, dcb_message_size, .true.)
    end do

    dcb_message(1:11) = 0
    num_spaces = data_buffer % put(dcb_message, 10, .true.)

  end block

  ! write (6,*) 'Relay, deleting DCB', local_relay_id
  call data_buffer % delete()

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from RELAY process'
    error stop num_errors
  end if

  write (6, *) 'Relay done!'

end subroutine io_relay_process


subroutine get_local_world(comm, rank, size)
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  integer, intent(OUT) :: comm, rank, size
  integer :: ierr

  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)
end subroutine get_local_world
