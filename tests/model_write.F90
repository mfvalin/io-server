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

module model_write_parameters
  use ISO_C_BINDING
  use ioserver_constants
  use ioserver_internal_mod
  implicit none

  save

  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 128

  integer,           parameter :: CB_MESSAGE_SIZE_INT       = 500                      ! Size of each data batch put in a CB
  ! integer, parameter :: CB_TOTAL_DATA_TO_SEND = 10000000                 ! How much total data to send (for each CB)
  integer,           parameter :: CB_TOTAL_DATA_TO_SEND_INT = 2000                     ! How much total data to send (for each CB)
  integer,           parameter :: MAX_DCB_MESSAGE_SIZE_INT  = CB_MESSAGE_SIZE_INT * 500        ! Size of each data batch put in the DCB
  integer(C_SIZE_T), parameter :: DCB_SIZE_BYTES            = MAX_DCB_MESSAGE_SIZE_INT * 4 * 5 ! Number of elements in each buffer of the DCB

  logical :: CHECK_CB_MESSAGES
  logical :: CHECK_DCB_MESSAGES

  integer :: num_channels

  type(ioserver_context) :: context

contains

  function compute_data_point(compute_rank, tag, index) result(data_point)
    implicit none
    integer, intent(in) :: compute_rank
    integer, intent(in) :: tag
    integer, intent(in) :: index
    integer :: data_point

    data_point = mod(compute_rank, 1000) * 1000000 + mod(tag, 1000) * 1000 + mod(index, 800) + tag / 1000
  end function compute_data_point

  function am_server_node(node_rank, single_node)
    use mpi_f08
    implicit none

    integer, intent(out) :: node_rank
    logical, intent(out) :: single_node
    logical :: am_server_node

    type(MPI_Comm) :: node_comm
    integer :: global_rank, node_root_global_rank
    integer :: node_size, global_size

    call MPI_Comm_rank(MPI_COMM_WORLD, global_rank)
    call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, global_rank, MPI_INFO_NULL, node_comm)
    call MPI_Comm_rank(node_comm, node_rank)

    node_root_global_rank = -1
    if (node_rank == 0) node_root_global_rank = global_rank

    call MPI_Bcast(node_root_global_rank, 1, MPI_INTEGER, 0, node_comm)

    am_server_node = .false.
    if (node_root_global_rank == 0) am_server_node = .true.

    call MPI_Comm_size(MPI_COMM_WORLD, global_size)
    call MPI_Comm_size(node_comm, node_size)

    single_node = .false.
    if (global_size == node_size) single_node = .true.
  end function am_server_node
end module model_write_parameters

#include <serializer.hf>

program pseudomodelandserver
  use ISO_C_BINDING
  use mpi_f08

  use ioserver_constants
  use model_write_parameters
  implicit none
  external io_relay_process
  external io_server_process
  integer :: status, input
  integer :: nio_node
  logical :: success

  type(MPI_Comm) :: comm
  integer :: rank, size, nserv !, noops
  character(len=128) :: arg
  type(comm_rank_size) :: fullnode_crs, local_crs

  ! integer, parameter :: NUM_NODES = 3
  logical :: server_node, single_node
  integer :: node_rank

  logical :: debug_mode

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

  debug_mode = .false.
  debug_mode = .true. ! activate debug mode

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

  server_node = am_server_node(node_rank, single_node)

  ! if(rank >= nserv) then
  ! if(mod(rank, NUM_NODES) .ne. 0) then
  if (.not. server_node) then
    ! =============================================================================================
    !                                 compute or IO relay Processes
    ! =============================================================================================
    ! call set_IOSERVER_relay(io_relay_process)
    call context % set_relay_fn(io_relay_process)
    !  no return from ioserver_int_init in the case of IO relay processes when io_relay_fn is defined
    !  compute processes will return from call
    ! status = ioserver_init(nio_node, 'M')
    success = context % init(nio_node, 'M', debug_mode)
    if (.not. success) then
      print *, 'Could not initialize io-server MODEL process'
      error stop 1
    end if
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
    ! nio_node = -1
    if (node_rank < nserv) then
      if (node_rank < nserv - num_channels) then
        ! call set_IOserver_server(io_server_process)
        call context % set_server_fn(io_server_process)
        ! status = ioserver_init(nio_node, 'O')   ! this function should not return as set_IOserver_server is set
        success = context % init(nio_node, 'O', debug_mode)
      else
        ! status = ioserver_init(nio_node, 'C') ! This should never return
        success = context % init(nio_node, 'C', debug_mode)
      end if
      if (.not. success) then
        print *, 'Could not initialize io-server SERVER process'
        error stop 1
      end if
    else 
      if (single_node) then
        !--------------------------------------------------------------
        ! Relay + model processes, when eeeveryone in on the same node
        ! call set_IOSERVER_relay(io_relay_process)
        call context % set_relay_fn(io_relay_process)
        !  no return from ioserver_int_init in the case of IO relay processes when io_relay_fn is defined
        !  compute processes will return from call
        ! status = ioserver_init(nio_node, 'M')
        success = context % init(nio_node, 'M', debug_mode)
        if (.not. success) then
          print *, 'Could not initialize io-server MODEL process (single node)'
          error stop 1
        end if
        call model_process()
        write(6,*)'END: compute, PE',rank+1,' of',size
      else
        !-----------------
        ! no-op processes
        ! status = ioserver_init(nio_node, 'Z')
        success = context % init(nio_node, 'Z', debug_mode)
        if (.not. success) then
          print *, 'Could not initialize io-server NO-OP process'
          error stop 1
        end if
      end if
    end if

  endif

  ! call ioserver_set_time_to_quit()
!  write(6,*)'FINAL: serv node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program

subroutine io_server_process()
  use mpi_f08
  use model_write_parameters
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  ! use io_server_mod
  implicit none

  type(comm_rank_size) :: consumer_crs
  type(MPI_Comm) :: global_comm !, consumer_comm
  integer :: global_rank, global_size
  type(distributed_circular_buffer) :: data_buffer
  ! logical :: success
  ! integer :: num_producers

  call get_local_world(global_comm, global_rank, global_size)
  ! call io_server_mod_init()


  ! write(6, *) 'SERVER process! PE', server_crs % rank + 1, ' of', server_crs % size, ' global:', global_rank + 1

  ! Create the DCB used for this test
  data_buffer = context % get_dcb()

  consumer_crs = context % get_crs(SERVER_COLOR + NODE_COLOR)

  ! Choose what to do based on whether we are a consumer or a channel process
  if (data_buffer % get_consumer_id() >= 0) then
    call consumer_process(data_buffer, consumer_crs % comm)
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

subroutine process_message()
end subroutine process_message

function receive_message(dcb, producer_id) result(finished)
  use distributed_circular_buffer_module
  use ioserver_functions
  use ioserver_message_module
  use model_write_parameters
  implicit none
  type(distributed_circular_buffer), intent(inout) :: dcb
  integer, intent(in) :: producer_id
  logical :: finished

  type(message_header) :: header
  type(model_record)   :: record
  logical :: success
  integer :: i_data_check
  integer :: message_size, end_cap
  integer(C_INT64_T) :: num_elements, num_data
  
  character(len=1), dimension(:), pointer :: filename
  integer, dimension(:), allocatable, save :: model_data
  integer, dimension(:), allocatable, save :: expected_data

  if (.not. allocated(model_data)) allocate(model_data(MAX_DCB_MESSAGE_SIZE_INT))
  if (.not. allocated(expected_data)) allocate(expected_data(CB_MESSAGE_SIZE_INT))

  print *, 'Receiving a message'
  finished = .false.

  success = dcb % peek_elems(producer_id, message_size, 1_8, CB_KIND_INTEGER_4)

  if (.not. success) then
    print *, 'Error after peeking into DCB'
    error stop 1
  end if

  if (message_size > MAX_DCB_MESSAGE_SIZE_INT) then
    print *, 'Message is larger than what we can deal with. That is problematic.'
    print *, 'Message size: ', message_size
    num_elements = dcb % get_capacity(producer_id, CB_KIND_INTEGER_4)
    print *, 'capacity     = ', num_elements
    print *, 'producer id  = ', producer_id
    error stop 1
  end if

  success = dcb % get_elems(producer_id, header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)

  if (.not. success) then
    print *, 'ERROR getting message header from DCB'
    error stop 1
  end if

  ! print '(A, I8, A, I3, A, I3, A, I8, A, I5)', &
  !   'Got header: len ', header % length, ', cmd ', header % command, ', stream ', header % stream, ', tag ', header % tag, ', rank ', header % sender_global_rank

  !-------
  ! Data
  if (header % command == MSG_COMMAND_DATA) then
    print *, 'Got DATA message'
    success = dcb % get_elems(producer_id, record, model_record_size_int(), CB_KIND_INTEGER_4, .true.)
    if (.not. success) then
      print *, 'Error reading record'
      error stop 1
    end if

    ! TODO manage compression + other metadata

    num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size (tkr) into account
    success = dcb % get_elems(producer_id, model_data, num_data, CB_KIND_INTEGER_4, .true.)

    if (CHECK_DCB_MESSAGES) then
      do i_data_check = 1, int(num_data, 4)
          expected_data(i_data_check) = compute_data_point(header % sender_global_rank, record % tag, i_data_check)
      end do
      if (.not. all(expected_data(1:num_data) == model_data(1:num_data))) then
        print *, 'Expected: ', expected_data(1:num_data)
        print *, 'Received: ', model_data(1:num_data)
        error stop 1
      end if
    end if

  !---------------
  ! Open a file
  else if (header % command == MSG_COMMAND_OPEN_FILE) then
    allocate(filename(header % length))
    print *, 'Got OPEN message'
    success = dcb % get_elems(producer_id, filename, INT(header % length, kind=8), CB_KIND_CHAR, .true.)
    print *, 'Opening a file named ', filename

  !----------------
  ! Misc. message
  else if (header % command == MSG_COMMAND_DUMMY) then
    print *, 'Got a DUMMY message!'

  !-----------------------------------
  ! Stop receiving from this producer
  else if (header % command == MSG_COMMAND_STOP) then
    print *, 'Got a STOP message'
    finished = .true.

  !------------
  ! Big no-no
  else
    print *, 'ERROR Unhandled message type!', header % command
    error stop 1
  end if

  success = dcb % get_elems(producer_id, end_cap, 1_8, CB_KIND_INTEGER_4, .true.)
  if (end_cap .ne. header % length) then
    print *, 'Discrepancy between message length and end cap', header % length, end_cap
    error stop 1
  end if

end function receive_message

subroutine consumer_process(data_buffer, consumer_comm)
  use mpi_f08
  use model_write_parameters
  use distributed_circular_buffer_module
  use ioserver_functions
  use data_serialize
  implicit none

  interface
    function c_gethostid() result(h) bind(C,name='gethostid') ! SMP host identifier
      import :: C_LONG
      integer(C_LONG) :: h
    end function c_gethostid
    function receive_message(dcb, producer_id) result(finished)
      import :: distributed_circular_buffer
      implicit none
      type(distributed_circular_buffer), intent(inout) :: dcb
      integer, intent(in) :: producer_id
      logical :: finished
    end function receive_message
  end interface

  type(distributed_circular_buffer), intent(inout) :: data_buffer
  type(MPI_Comm), intent(in) :: consumer_comm

  integer, parameter :: WRITE_BUFFER_SIZE = 50000

  integer :: consumer_id, num_producers, num_consumers, producer_id
  integer :: i_producer
  integer :: message_size
  integer, dimension(:), allocatable :: message, expected_message
  integer, dimension(:), allocatable :: file_write_buffer
  integer :: file_write_position, write_size
  logical :: finished, producer_finished
  integer :: num_errors
  integer :: num_active_producers

  ! type(grid_assembly) :: partial_grids

  integer, allocatable :: active_producers(:)

  integer           :: file_unit
  character(len=14) :: file_name

  allocate(message(MAX_DCB_MESSAGE_SIZE_INT))
  allocate(expected_message(MAX_DCB_MESSAGE_SIZE_INT))
  allocate(file_write_buffer(WRITE_BUFFER_SIZE))

  num_errors = 0

  consumer_id   = data_buffer % get_consumer_id()
  num_consumers = data_buffer % get_num_consumers()
  num_producers = data_buffer % get_num_producers()

  allocate(active_producers(num_producers))
  active_producers(:) = -1

  file_write_position = 1

  write(file_name,'(A6, I4.4, A4)') 'SERVER', consumer_id, '.out'
  open(newunit = file_unit, file = file_name, status = 'replace', form = 'unformatted')

  num_active_producers = 0
  if (consumer_id == 0) then
    ! Should receive one test signal
    do i_producer = 0, num_producers - 1
      finished = receive_message(data_buffer, i_producer)
      print *, 'Received acknowledge message from producer ', i_producer, finished
      if (.not. finished) then
        num_active_producers = num_active_producers + 1
        active_producers(num_active_producers) = i_producer
      end if
    end do
  end if

  call MPI_Bcast(num_active_producers, 1, MPI_INTEGER, 0, consumer_comm)
  call MPI_Bcast(active_producers, num_active_producers, MPI_INTEGER, 0, consumer_comm)

  ! Now, we repeatedly loop through all buffers this consumer is responsible for
  ! If the buffer is empty, just go on to the next
  ! If the buffer has something, the first value is the size of the data block to read, so we read it
  ! If the first value to read is 0, it means this buffer is done and won't send anything anymore
  ! When all buffer have a first value of 0, the entire test is finished
  finished = .false.
  do while (.not. finished)
    finished = .true.
    do i_producer = consumer_id + 1, num_active_producers, num_consumers

      producer_id = active_producers(i_producer)
      if (producer_id < 0) cycle

      ! The buffer is empty, so it has not finished sending stuff. Just move on to the next
      if (data_buffer % get_num_elements(producer_id, CB_KIND_INTEGER_4) == 0) then
        finished = .false.
        cycle
      end if

      producer_finished = receive_message(data_buffer, producer_id)
      if (producer_finished) then
        active_producers(i_producer) = -1
      else
        finished = .false.
      end if

      if (message_size > 0)  then

        ! call partial_grids % put_data(record, 1, message(old_record_length + 1:))

        write_size = message_size / 4
        if (file_write_position + write_size >= WRITE_BUFFER_SIZE) then
          ! write(file_unit) file_write_buffer(1:file_write_position)
          file_write_position = 1
        end if

        file_write_buffer(file_write_position:file_write_position+write_size) = message(1:write_size)
        file_write_position = file_write_position + write_size
      else if (message_size == 0) then
        ! The buffer will not send anything more
        print *, 'Got STOP signal from producer ', producer_id
      else
        ! This should not happen
        write (6, *) 'message_size: ', message_size
        write (6, *) 'This line should not ever be printed'
        num_errors = num_errors + 1
        error stop 1
      end if


    end do
  end do

  print *, 'Server done receiving', consumer_id

  write(file_unit) file_write_buffer(1:file_write_position)
  close(file_unit)
  file_write_position = 1

  ! Final check on the buffers' content
  do i_producer = consumer_id + 1, num_active_producers - 1, num_consumers
    if (data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4) .ne. 0) then
      num_errors = num_errors + 1 
      write (6, *) 'ERROR: buffer should be empty at the end of test', data_buffer % get_num_elements(i_producer, CB_KIND_INTEGER_4), i_producer
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

subroutine model_process()
  use ISO_C_BINDING
  use mpi_f08

  use circular_buffer_module
  use ioserver_file_module
  use ioserver_message_module
  use model_write_parameters
  use rpn_extra_module, only: sleep_us
  use shmem_heap
  implicit none

  type(comm_rank_size)  :: node_crs, local_compute_crs, all_crs, model_crs
  integer               :: local_compute_id, global_rank
  integer               :: num_errors
  type(circular_buffer) :: data_buffer
  logical               :: success

  integer :: f_status
  type(server_file) :: results_file
  type(heap)        :: node_heap
  integer(kind=4), dimension(:), pointer :: msg_array
  type(block_meta) :: msg_array_info
  type(subgrid) :: my_grid
  type(grid) :: input_grid, output_grid

  type(C_PTR) :: p
  integer(C_INT), dimension(MAX_ARRAY_RANK) :: d
  integer(C_INT) :: tkr
  integer(C_SIZE_T) :: o

  node_heap = context % get_local_heap()

  results_file = context % open_file('model_write_results_')
  if (.not. results_file % is_open()) then
    print *, 'Unable to open model file!!!!'
    error stop 1
  end if

  model_crs = context % get_crs(MODEL_COLOR)

  num_errors        = 0
  node_crs          = context % get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  local_compute_crs = context % get_crs(NODE_COLOR + MODEL_COLOR)
  local_compute_id  = local_compute_crs % rank

  all_crs = context % get_crs(NO_COLOR)
  global_rank = all_crs % rank

  ! print *, 'MODEL, local compute id: ', local_compute_id

  data_buffer = context % get_server_bound_cb()
  if (.not. data_buffer % is_valid()) then
    print *, 'ERROR: CB received from context is not valid!'
    error stop 1
  end if
  ! if (data_buffer % get_capacity(CB_KIND_INTEGER_4) .ne. data_buffer % get_num_spaces(CB_KIND_INTEGER_4)) then
  !   print *, 'AAAAaaaahhhhh local CB is NOT EMPTY. Aaaaahhh', data_buffer % get_capacity(CB_KIND_INTEGER_4), data_buffer % get_num_spaces(CB_KIND_INTEGER_4)
  !   error stop 1
  ! end if

  ! NODE barrier to signal RELAY processes that the CB (from each MODEL) is ready
  call MPI_Barrier(node_crs % comm)

  ! Send a first test signal
  ! success = data_buffer % put(local_compute_id, 1_8, CB_KIND_INTEGER_4, .true.)

  ! Init area info
  my_grid % i0 = local_compute_id * CB_MESSAGE_SIZE_INT + 1
  my_grid % ni = CB_MESSAGE_SIZE_INT
  my_grid % j0 = 1 ! cause ARRAYS START AT 1
  my_grid % nj = 1
  my_grid % nk = 1
  my_grid % nv = 1

  input_grid % id = 1
  input_grid % size_i = CB_MESSAGE_SIZE_INT * model_crs % size
  input_grid % size_j = 1

  output_grid % id = 1

  ! call sleep_us(5000)
  block
    integer :: i, j
    do i = 0, CB_TOTAL_DATA_TO_SEND_INT / CB_MESSAGE_SIZE_INT
      ! Get memory and put data in it
      msg_array_info = node_heap % allocate(msg_array, [CB_MESSAGE_SIZE_INT])
      call block_meta_internals(msg_array_info, p, d, tkr, o)
      do while (.not. c_associated(p))
        ! print*, 'Could not allocate!!!!! Trying again soon'
        call sleep_us(10)
        msg_array_info = node_heap % allocate(msg_array, [CB_MESSAGE_SIZE_INT])
        call block_meta_internals(msg_array_info, p, d, tkr, o)
        ! error stop 2
      end do

      ! Using i + 2 b/c tag is incremented when opening a file
      do j = 1, CB_MESSAGE_SIZE_INT
        msg_array(j) = compute_data_point(global_rank, i + 2, j)
      end do

      ! if (local_compute_id == 0) then
      !   print *, 'Sending', msg_array(1:3), transfer(p, tmp)
      ! end if
      ! Write the data to a file (i.e. send it to the server to do that for us)
      f_status = results_file % write(msg_array_info, my_grid, input_grid, output_grid)

      if (f_status .ne. 0) then
        print *, 'ERROR while trying to do a WRITE', f_status
        error stop 1
      end if

      call sleep_us(50)
    end do

    ! We done. Send the stop signal
    success = data_buffer % put(0, 1_8, CB_KIND_INTEGER_4, .true.)
  end block

  f_status = results_file % close() ! TODO implement that function so that it can send the stop signal
  if (f_status .ne. 0) then
    print *, 'Unable to close model file!!!!'
    error stop 1
  end if

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from MODEL process'
    error stop 1
  end if

  call MPI_Barrier(node_crs % comm) ! Sync with relays and avoid scrambling output

end subroutine model_process

subroutine io_relay_process()
  use ISO_C_BINDING
  use mpi_f08

  use model_write_parameters
  use circular_buffer_module
  use distributed_circular_buffer_module
  ! use io_relay_mod
  use ioserver_memory_mod
  use data_serialize
  implicit none

  type(MPI_Comm)       :: global_comm
  integer              :: global_rank, global_size
  type(comm_rank_size) :: local_relay_crs, local_model_crs, node_crs
  integer              :: num_local_compute, local_relay_id, num_local_relays, producer_id
  integer              :: i_compute, index, num_errors
  integer :: h_status

  logical :: success
  type(distributed_circular_buffer) :: data_buffer
  type(circular_buffer), dimension(:), pointer :: cb_list
  type(heap), dimension(:), pointer :: heap_list

  ! call io_relay_mod_init()
  call get_local_world(global_comm, global_rank, global_size)

  cb_list => context % get_server_bound_cb_list()
  heap_list => context % get_heap_list()

  node_crs        = context % get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
  local_relay_crs = context % get_crs(RELAY_COLOR + NODE_COLOR)
  local_model_crs = context % get_crs(MODEL_COLOR + NODE_COLOR)

  local_relay_id    = local_relay_crs % rank
  num_local_relays  = local_relay_crs % size
  num_local_compute = local_model_crs % size
  num_errors        = 0

  ! print *, 'RELAY, local relay id: ', local_relay_id

  ! Create the DCB used to communicate with the server
  data_buffer = context % get_dcb()

  producer_id = data_buffer % get_producer_id()

  ! NODE barrier to allow MODEL processes to initialize their CBs
  call MPI_Barrier(node_crs % comm)

  ! Recover all CBs that are stored on this node and check that they are valid (even if we don't necessarily access all of them)
  index = 1

  block
    use ioserver_message_module
    
    integer :: i_data_check
    integer :: total_message_size, model_message_size, end_cap, content_size
    logical :: finished = .false.
    integer, dimension(CB_MESSAGE_SIZE_INT) :: cb_message, expected_message
    ! integer, dimension(:), allocatable      :: dcb_message

    integer, dimension(:), pointer :: f_data
    type(C_PTR) :: c_data
    integer :: num_data

    type(model_record)   :: record
    type(message_header) :: header
    type(jar)            :: dcb_message_jar
    integer              :: jar_ok, num_jar_elem
    integer, dimension(:), pointer :: dcb_message

    ! allocate(dcb_message(MAX_DCB_MESSAGE_SIZE_INT))

    jar_ok = dcb_message_jar % new(MAX_DCB_MESSAGE_SIZE_INT)
    if (jar_ok .ne. 0) then
      print *, 'Could not create jar to contain DCB message...'
      error stop 1
    end if

    dcb_message => dcb_message_jar % raw_array()

    ! Say hi to the consumer processes
    header % length  = 0
    if (local_relay_id == 0) then
      header % command = MSG_COMMAND_DUMMY
    else
      header % command = MSG_COMMAND_STOP
    end if
    success = data_buffer % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)
    success = data_buffer % put_elems(0, 1_8, CB_KIND_INTEGER_4, .true.) .and. success ! Append size

    if (.not. success) then
      print *, 'ERROR saying HI to the consumer...'
      error stop 1
    end if

    if (local_relay_id == 0) then ! only 1 relay per node sends output

      ! Get the initial test signal from each CB this relay is responsible for
      ! do i_compute = 0, num_local_compute - 1 !, num_local_relays
      !   cb_message(:) = -1
      !   success = cb_list(i_compute) % get(cb_message, 1_8, CB_KIND_INTEGER_4, .true.)
      !   if (.not. success) then
      !     print *, 'ERROR in relay. atomic_get returned a negative value'
      !     error stop 1
      !   end if
      ! end do

      ! The main loop
      call dcb_message_jar % reset()
      ! print *, 'Jar high = ', dcb_message_jar % high()
      expected_message(:) = -1
      do while (.not. finished)
        finished = .true.
        do i_compute = 0, num_local_compute - 1

          ! The buffer is empty, so it has not finished sending stuff. Move on to the next CB
          if (cb_list(i_compute) % get_num_elements(CB_KIND_INTEGER_4) == 0) then
            finished = .false.
            cycle
          end if

          success = cb_list(i_compute) % peek(model_message_size, 1_8, CB_KIND_INTEGER_4)
          if (model_message_size > 0) then
            ! There is something in the buffer!
            finished = .false.

            ! Read the content of the CB

            success = cb_list(i_compute) % get(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.) ! Header
            if (.not. success) then
              print *, 'ERROR when getting message header from CIO_OUT', i_compute
              error stop 1
            end if

            ! print '(A, I8, A, I3, A, I3, A, I8, A, I5)', &
            !   'Got header: len ', header % length, ', cmd ', header % command, ', stream ', header % stream, ', tag ', header % tag, ', rank ', header % sender_global_rank

            if (header % command == MSG_COMMAND_DATA) then
              success = cb_list(i_compute) % get(record, model_record_size_int(), CB_KIND_INTEGER_4, .false.) ! Record 
              success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.) ! End cap

              if (.not. success) then
                print *, 'ERROR Could not get record from data message'
                error stop 1
              end if

              if (record % record_length .ne. end_cap .or. (record % record_length .ne. header % length)) then
                print *, 'We have a problem with message size (end cap does not match)'
                error stop 1
              end if

              ! TODO take compression meta and other meta stuff into account

              c_data = context % ptr_translate_from(record % data, MODEL_COLOR, i_compute)
              num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size into account!!!!
              call c_f_pointer(c_data, f_data, [num_data])
              
              ! print *, 'f_data: ', f_data, num_data
              ! print '(A12, I4, I5, I3, I4, I4, I4, I4, I4, I4, I4, I3, I3, I4, I4)', 'Record info: ', &
              !   record % record_length, record % tag, record % stream, &
              !   record % ni, record % nj, record % gnignj, &
              !   record % gin, record % gout, record % i0, record % j0, &
              !   record % nk, record % nvar, record % csize, record % msize

              if (CHECK_CB_MESSAGES) then
                do i_data_check = 1, num_data
                  expected_message(i_data_check) = compute_data_point(header % sender_global_rank, record % tag, i_data_check)
                end do
                ! if (i_compute == 0) then
                !   print *, 'Got ', f_data(1:3), transfer(record % data, tmp)
                ! end if
                if (.not. all(expected_message(1:num_data) == f_data(:))) then
                  num_errors = num_errors + 1
                  print *, 'Expected: ', expected_message(1:num_data / 100 + 3)
                  print *, 'Received: ', f_data(:num_data / 100 + 3)
                  print *, 'i_compute, tag: ', i_compute, record % tag
                  error stop 1
                end if
              end if

              record % record_length = record % record_length + num_data
              header % length = record % record_length
              total_message_size = INT(message_header_size_int(), kind=4) + record % record_length + 1

            else
              content_size = header % length
              if (header % command == MSG_COMMAND_OPEN_FILE) content_size = num_char_to_num_int(header % length)

              total_message_size = INT(message_header_size_int(), kind=4) + content_size + 1
            end if

            ! If the DCB message buffer is too full to contain that new package, flush it now
            if (dcb_message_jar % high() > MAX_DCB_MESSAGE_SIZE_INT) then
              print *, 'Sending data ', dcb_message_jar % high()
              success = data_buffer % put_elems(dcb_message, INT(dcb_message_jar % high(), C_SIZE_T), CB_KIND_INTEGER_4, .true.)
              call dcb_message_jar % reset()

              if (.not. success) then
                print *, 'ERROR sending message from relay to server!'
                error stop 1
              end if
            end if

            ! Copy the message header
            num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header)
            ! print *, '(header) Jar high = ', dcb_message_jar % high()

            if (header % command == MSG_COMMAND_DATA) then
              num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, record)
              ! print *, '(record) Jar high = ', dcb_message_jar % high()

              !TODO compression and other meta


              ! The data
              num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, f_data(:))
              ! print *, '(data)   Jar high = ', dcb_message_jar % high()

              ! End cap
              num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, record % record_length)
              ! print *, '(r len)  Jar high = ', dcb_message_jar % high()

              f_data(2) = -1
              h_status = heap_list(i_compute) % free(c_data)
              if (h_status .ne. 0) then
                print*, 'Unable to free heap data (from RELAY)'
                error stop 1
              end if

            else
              success = cb_list(i_compute) % get(cb_message, int(content_size, kind=8), CB_KIND_INTEGER_4, .true.)
              success = cb_list(i_compute) % get(end_cap, 1_8, CB_KIND_INTEGER_4, .true.) ! End cap
              num_jar_elem = JAR_PUT_ITEMS(dcb_message_jar, cb_message(1:content_size))
              ! print *, '(cmd d)  Jar high = ', dcb_message_jar % high()
              num_jar_elem = JAR_PUT_ITEM(dcb_message_jar, header % length)
              ! print *, '(cmdlen) Jar high = ', dcb_message_jar % high()
            end if
          end if
        end do
      end do

      ! Send the remaining data
      print *, 'Sending remaining data: ', dcb_message_jar % high()
      success = data_buffer % put_elems(dcb_message, INT(dcb_message_jar % high(), C_SIZE_T), CB_KIND_INTEGER_4, .true.)

      if (.not. success) then
        print *, 'ERROR sending remaining data!'
        error stop 1
      end if

      ! Send a stop signal
      print *, 'Relay sending STOP signal'
      header % length = 0
      header % command = MSG_COMMAND_STOP
      success = data_buffer % put_elems(header, message_header_size_int(), CB_KIND_INTEGER_4, .true.)
      success = data_buffer % put_elems(header % length, 1_8, CB_KIND_INTEGER_4, .true.) .and. success

      if (.not. success) then
        print *, 'Could not send a stop signal!!!'
      end if
    end if

    ! deallocate(dcb_message)
  end block


  if (local_relay_id == 0) then
    call heap_list(0) % dumpinfo()
    do i_compute = 0, num_local_compute - 1
      call cb_list(i_compute) % print_stats(producer_id * 100 + i_compute, i_compute == 0)
    end do
  end if

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from RELAY process'
    error stop 1
  end if

  call MPI_Barrier(node_crs % comm) ! Sync with models and avoid scrambling output

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

