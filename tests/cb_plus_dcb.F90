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
  integer, parameter :: NUM_CB_ELEMENTS = 1000
  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 128
  integer, parameter :: CB_MESSAGE_SIZE = 100
  integer, parameter :: CB_TOTAL_DATA_TO_SEND = 1000
  integer, parameter :: MAX_DCB_MESSAGE_SIZE = CB_MESSAGE_SIZE * 21
  integer, parameter :: NUM_DCB_ELEMENTS = MAX_DCB_MESSAGE_SIZE * 5
  ! integer, parameter :: NUM_DCB_ELEMENTS = 200

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
  implicit none
  external io_relay_process
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node, modelio, me
  integer :: comm, rank, size
  integer :: node_comm, node_rank, node_size
  logical :: is_server_node
  integer :: nserv, ierr
  logical :: error
  character(len=128) :: arg

  call mpi_init(status)

  ! call IOSERVER_debug(1)            ! activate debug mode

  ! 3 arguments:
  !   1. number of server processes
  !   2. number of no-op processes (should be on the server)
  !   3. number of io relay processes per model (compute) node

  ! arg = '0'
  ! if(COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  ! read(arg,*)noops

  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg,*) nserv
  if (nserv < 2) nserv = 2

  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)

  call get_local_world(comm, rank, size, node_comm, node_rank, node_size, is_server_node)
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/2, GBYTE)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif


  if (.not. is_server_node) then
    read(arg, *) nio_node ! relay processes per node
    if (nio_node < 2) nio_node = 2
    call set_IOSERVER_relay(io_relay_process)
    status = ioserver_int_init(model, modelio, allio, nodeio, serverio, node_comm, nio_node, 'M') ! Will not return for relay processes

    call model_process(node_comm, rank)

  else  ! ranks 0, 1, nserv-1 : server
    nio_node = -1
    if(node_rank < nserv) then
      status = ioserver_int_init(model, modelio, allio, nodeio, serverio, node_comm, nio_node, 'O')
    else
      status = ioserver_int_init(model, modelio, allio, nodeio, serverio, node_comm, nio_node, 'Z') ! This call will never return
    endif

    call server_process(serverio, allio, rank)

  endif

777 continue
  call ioserver_set_time_to_quit()
  call mpi_finalize(status)

end program


subroutine server_process(server_comm, server_relay_comm, global_rank)
  use cb_plus_dcb_parameters
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none
  integer, intent(IN) :: server_comm, server_relay_comm
  integer, intent(IN) :: global_rank

  integer :: server_rank, server_size, ierr
  integer :: global_io_rank, global_io_size
  type(distributed_circular_buffer) :: data_buffer
  logical :: success

  call MPI_Comm_rank(server_comm, server_rank, ierr)
  call MPI_Comm_size(server_comm, server_size, ierr)
  call MPI_Comm_rank(server_relay_comm, global_io_rank, ierr)
  call MPI_Comm_size(server_relay_comm, global_io_size, ierr)

  write(6, *) 'Server process! PE', server_rank + 1, ' of', server_size, ' global:', global_rank + 1

  success = data_buffer % create(server_relay_comm, server_comm, global_io_size - server_size, server_size / 2, NUM_DCB_ELEMENTS)

  if (data_buffer % get_consumer_id() >= 0) then
    call consumer_process(data_buffer)
  else if (data_buffer % get_receiver_id() >= 0) then
    call receiver_process(data_buffer)
  else
    write(6, *) 'We have a problem'
  end if

  ! write(6, *) 'Server, deleting DCB', server_rank + 1
  call data_buffer % delete()

end subroutine server_process

subroutine receiver_process(data_buffer)
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use ISO_C_BINDING
  implicit none
  type(distributed_circular_buffer), intent(inout) :: data_buffer
  integer(C_INT) :: result

  ! write(6, *) 'I am a receiver'
  result = data_buffer % start_receiving()

  if (result .ne. 0) then
    write(6, *) 'The receiver loop did not terminate properly!!!'
  end if

end subroutine receiver_process

subroutine consumer_process(data_buffer)
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none

  type(distributed_circular_buffer), intent(inout) :: data_buffer

  integer :: consumer_id, num_producers, num_consumers
  integer :: i_producer
  integer :: num_elements
  integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: message
  logical :: finished

  ! write(6, *) 'I am a consumer'

  consumer_id   = data_buffer % get_consumer_id()
  num_consumers = data_buffer % get_num_consumers()
  num_producers = data_buffer % get_num_producers()

  do i_producer = consumer_id, num_producers - 1, num_consumers
    num_elements = data_buffer % get(i_producer, message, 2)
    write (6, *) 'Received HI from relay #, local ID #, global rank #', i_producer, message(1), message(2)
  end do

  finished = .false.
  do while (.not. finished)
    finished = .true.
    do i_producer = consumer_id, num_producers - 1, num_consumers
      num_elements = data_buffer % get(i_producer, message, 1)

      if (message(1) > 0)  then
        finished = .false.
        num_elements = data_buffer % get(i_producer, message(2:), message(1) * (CB_MESSAGE_SIZE + 1))
      end if

    end do
  end do

end subroutine consumer_process


subroutine model_process(node_comm, global_rank)
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: node_comm
  integer, intent(IN) :: global_rank

  integer :: node_rank, node_size, ierr
  integer :: window, disp_unit, target_disp_unit
  integer :: target_proc
  integer(DATA_ELEMENT) :: dummy_element
  integer(MPI_ADDRESS_KIND) :: base_mem_ptr, target_mem_ptr, target_size
  type(C_PTR) :: shmem_ptr
  type(circular_buffer) :: data_buffer
  logical :: success
  integer :: worker_flag = 1
  integer :: num_spaces

  integer(MPI_ADDRESS_KIND), parameter :: cb_window_size = NUM_CB_ELEMENTS * 8
  integer(DATA_ELEMENT), dimension(CB_MESSAGE_SIZE) :: message
  integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: dcb_message

  call MPI_Comm_rank(node_comm, node_rank, ierr)
  call MPI_Comm_size(node_comm, node_size, ierr)

  ! write(6, *) 'Model process! PE', node_rank + 1, ' of', node_size, ' global:', global_rank + 1

  ! Allocate MPI window in shared memory
  disp_unit = C_SIZEOF(dummy_element)
  call MPI_Win_allocate_shared(cb_window_size, disp_unit, MPI_INFO_NULL, node_comm, base_mem_ptr, window, ierr)

  shmem_ptr = transfer(base_mem_ptr, C_NULL_PTR)
  success = data_buffer % create(shmem_ptr, NUM_CB_ELEMENTS)
  if (.not. success) write (6, *) 'AAAAHHHhhhh could not create CB from model PE ', node_rank

  call MPI_Barrier(node_comm, ierr)
  
  ! write(6, *) '(model)', node_rank, ' Sending flag to rank', 0
  ! call MPI_Send(worker_flag, 1, MPI_INTEGER, 0, 0, node_comm, ierr)

  message(1) = global_rank
  num_spaces = data_buffer % atomic_put(message, 1)

  block
    integer :: i, j
    do i = 1, CB_TOTAL_DATA_TO_SEND, CB_MESSAGE_SIZE
      do j = 1, CB_MESSAGE_SIZE
        message(j) = compute_data_point(global_rank, i + j)
      end do
      num_spaces = data_buffer % atomic_put(message, CB_MESSAGE_SIZE)
    end do
  end block

end subroutine model_process


subroutine io_relay_process(model, modelio, allio, nodeio, serverio, nodecom)
  use ISO_C_BINDING
  use cb_plus_dcb_parameters
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, nodecom, modelio
  include 'mpif.h'

  integer :: rank, size, ierr, i_loop, index
  integer :: node_rank, node_size
  integer :: relay_rank, relay_size
  integer :: global_rank

  integer :: window, disp_unit, source_disp_unit
  integer :: worker_flag = -1, source_proc
  integer :: recv_status
  integer(DATA_ELEMENT) :: dummy_element
  integer(MPI_ADDRESS_KIND) :: base_mem_ptr, source_mem_ptr, source_size
  type(C_PTR) :: shmem_ptr
  type(circular_buffer), dimension(MAX_NUM_WORKER_PER_NODE) :: data_buffers
  integer, dimension(MAX_NUM_WORKER_PER_NODE) :: worker_global_ranks

  integer(MPI_ADDRESS_KIND) :: zero_window_size = 1
  logical :: success = .false.
  integer :: num_workers = -1, num_relays = -1, local_num_workers
  integer :: local_relay_id
  integer :: num_errors = 0
  type(distributed_circular_buffer) :: data_buffer

  call MPI_Comm_rank(nodeio, rank, ierr)
  call MPI_Comm_size(nodeio, size, ierr)

  call MPI_Comm_rank(MPI_COMM_WORLD, global_rank, ierr)
  call MPI_Comm_rank(nodecom, node_rank, ierr)
  call MPI_Comm_size(nodecom, node_size, ierr)

  call MPI_Comm_rank(modelio, relay_rank, ierr)
  call MPI_Comm_size(modelio, relay_size, ierr)

  ! write(6, *) 'Relay process! PE', node_rank + 1, ' of', node_size, ' global:', global_rank + 1

  disp_unit = C_SIZEOF(dummy_element)
  call MPI_Win_allocate_shared(zero_window_size, disp_unit, MPI_INFO_NULL, nodecom, base_mem_ptr, window, ierr)

  success = data_buffer % create(allio, MPI_COMM_NULL, -1, -1, -1)
  success = data_buffer % is_valid()

  if (.not. success) then
    write(6, *) 'Unable to create DCB on relay process!!'
  end if

  call MPI_Barrier(nodecom, ierr)

  index = 1
  num_workers = 0
  do i_loop = 0, node_size - 1
    if (i_loop .ne. node_rank) then
      source_proc = i_loop
      call MPI_Win_shared_query(window, source_proc, source_size, source_disp_unit, source_mem_ptr, ierr)
      shmem_ptr = transfer(source_mem_ptr, C_NULL_PTR)
      success = data_buffers(index) % create(shmem_ptr)
      success = data_buffers(index) % is_valid()

      if (success) then
        index = index + 1
        num_workers = num_workers + 1
      else
        ! print *, 'AAAAhhhh invalid buffer given...', i_loop
      end if
    end if 
  end do

  num_relays = node_size - num_workers
  local_relay_id = node_rank
  if (local_relay_id >= num_workers) local_relay_id = local_relay_id - num_workers
  local_num_workers = (num_workers - local_relay_id - 1) / num_relays

  ! write (6, *) 'Relay ID, Local relay ID, num_relays, num_workers, local_num_workers', &
  !  node_rank, local_relay_id, num_relays, num_workers, local_num_workers

  block
    
    integer :: i_worker, i_data_outer, i_data_check, i_worker_local
    integer(DATA_ELEMENT), dimension(CB_MESSAGE_SIZE) :: cb_message, expected_message
    integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: dcb_message
    integer :: dcb_message_size, msg_start
    integer :: num_elements, num_spaces

    do i_worker = local_relay_id + 1, num_workers, num_relays
      cb_message(:) = -1
      num_elements = data_buffers(i_worker) % atomic_get(cb_message, 1)
      ! write(6, *) 'Relay ID, Buffer ID, Global rank:', local_relay_id, i_worker, message(1)
      worker_global_ranks(i_worker) = cb_message(1)
    end do

    ! Say hi to the consumer processes
    dcb_message(1) = local_relay_id
    dcb_message(2) = global_rank
    num_spaces = data_buffer % put(dcb_message, 2)

    ! Set up message header
    dcb_message_size = 1 + local_num_workers + local_num_workers * CB_MESSAGE_SIZE

    if (dcb_message_size > MAX_DCB_MESSAGE_SIZE) then
      write(6, *) 'DCB message size is larger than MAX_DCB_MESSAGE_SIZE !!!'
    end if

    dcb_message(1) = local_num_workers

    do i_data_outer = 1, CB_TOTAL_DATA_TO_SEND, CB_MESSAGE_SIZE
      i_worker_local = 1
      do i_worker = local_relay_id + 1, num_workers, num_relays
        dcb_message(i_worker_local + 1) = worker_global_ranks(i_worker)
        num_elements = data_buffers(i_worker) % atomic_get(cb_message, CB_MESSAGE_SIZE)


        msg_start = 1 + local_num_workers + (i_worker_local - 1) * CB_MESSAGE_SIZE
        dcb_message(msg_start:msg_start + CB_MESSAGE_SIZE) = cb_message(:)

        ! do i_data_check = 1, CB_MESSAGE_SIZE
        !   expected_message(i_data_check) = compute_data_point(worker_global_ranks(i_worker), i_data_outer + i_data_check)
        ! end do
        ! if (.not. all(expected_message == message)) then
        !   num_errors = num_errors + 1
        !   if (i_worker < 5) then
        !     write(6, *) 'GOT WRONG MESSAGE FROM WORKER # with global rank #', i_worker, worker_global_ranks(i_worker)
        !     write(6, *) message
        !     write(6, *) 'Expected'
        !     write(6, *) expected_message
        !   end if
        ! end if

        i_worker_local = i_worker_local + 1
      end do

      num_spaces = data_buffer % put(dcb_message, dcb_message_size)
    end do

    dcb_message(1:11) = 0
    num_spaces = data_buffer % put(dcb_message, 10)

  end block

  ! write (6,*) 'Relay, deleting DCB', local_relay_id
  call data_buffer % delete()

end subroutine io_relay_process


subroutine get_local_world(comm, rank, size, node_comm, node_rank, node_size, is_server_node)
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  integer, intent(OUT) :: comm, rank, size
  integer, intent(OUT) :: node_comm, node_rank, node_size
  logical, intent(OUT) :: is_server_node

  integer :: ierr
  integer :: node_signal

  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)

  call MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, node_comm, ierr)
  call MPI_Comm_rank(node_comm, node_rank, ierr)
  call MPI_Comm_size(node_comm, node_size, ierr)

  is_server_node = .false.

  node_signal = -1
  if (node_rank == 0) node_signal = rank
  call MPI_Bcast(node_signal, 1, MPI_INTEGER, 0, node_comm, ierr)

  if (node_signal == 0) is_server_node = .true.

  ! print *, 'rank', rank, ' is_server_node =', is_server_node, ' node rank', node_rank

end subroutine get_local_world
