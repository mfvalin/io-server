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
  use ioserver_functions
  implicit none

  integer, parameter :: MAX_NUM_WORKER_PER_NODE = 128

  integer, parameter :: CB_MESSAGE_SIZE       = 500                      ! Size of each data batch put in a CB
  ! integer, parameter :: CB_TOTAL_DATA_TO_SEND = 10000000                 ! How much total data to send (for each CB)
  integer, parameter :: CB_TOTAL_DATA_TO_SEND = 2000                 ! How much total data to send (for each CB)
  integer, parameter :: MAX_DCB_MESSAGE_SIZE  = CB_MESSAGE_SIZE * 500    ! Size of each data batch put in the DCB
  integer, parameter :: NUM_DCB_ELEMENTS      = MAX_DCB_MESSAGE_SIZE * 5 ! Number of elements in each buffer of the DCB

  integer, parameter :: MAX_ASSEMBLY_LINES = 20

  logical :: CHECK_CB_MESSAGES
  logical :: CHECK_DCB_MESSAGES

  integer :: num_channels

  type, private :: grid_assembly_line
    integer :: tag = -1
    integer :: file_unit
    integer, dimension(:, :), allocatable :: data
    integer(kind=8) :: missing_data = -1
  end type

  type, public :: grid_assembly
    type(grid_assembly_line), dimension(MAX_ASSEMBLY_LINES) :: lines

    contains
    procedure :: put_data => grid_assembly_put_data
  end type

contains

  function grid_assembly_put_data(this, record, entry_size, subgrid_data) result(status)
    implicit none
    class(grid_assembly), intent(inout) :: this
    class(model_record),  intent(in)    :: record
    ! integer, intent(in) :: tag
    ! integer, intent(in) :: start_i, start_j
    ! integer, intent(in) :: num_i, num_j
    ! integer, intent(in) :: total_size_i, total_size_j
    integer, intent(in) :: entry_size
    integer, intent(in), dimension(record % ni, record % nj * entry_size) :: subgrid_data

    integer :: status

    integer :: i_line, line_id, free_line_id
    integer :: i0, i1, j0, j1

    status       = -1
    line_id      = -1
    free_line_id = -1
    do i_line = 1, MAX_ASSEMBLY_LINES
      if (this % lines(i_line) % tag == record % tag) then
        line_id = i_line
        exit
      else if (this % lines(i_line) % tag == -1 .and. free_line_id == -1) then
        free_line_id = i_line
      end if
    end do
    
    if (line_id == -1) then
      if (free_line_id .ne. -1) then
        line_id = free_line_id
        print *, 'Starting assembly for a new grid! Tag = ', record % tag, line_id, free_line_id
        this % lines(line_id) % tag = record % tag
        this % lines(line_id) % file_unit = record % stream
        allocate(this % lines(line_id) % data(record % grid_size_i, record % grid_size_j * entry_size))
        this % lines(line_id) % missing_data = record % grid_size_i * record % grid_size_j * entry_size
      else
        print *, 'We have reached the maximum number of grids being assembled! Quitting.'
        error stop 1
        return
      end if
    end if

    i0 = record % i0
    i1 = i0 + record % ni - 1
    j0 = record % j0 * entry_size
    j1 = j0 + (record % nj - 1) * entry_size
    ! print *, 'si, sj, ni, nj', start_i, start_j, num_i, num_j
    ! print *, 'Writing to ', i0, i1, j0, j1, entry_size
    this % lines(line_id) % data(i0:i1, j0:j1) = subgrid_data(:,:)
    this % lines(line_id) % missing_data = this % lines(line_id) % missing_data - record % ni * record % nj * entry_size

    if (this % lines(line_id) % missing_data == 0) then
      print *, 'Completed a grid! Gotta write it down now'
      status = line_id
    else
      status = 0
    end if
  end function grid_assembly_put_data

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

  use ioserver_functions
  use memory_arena_mod
  use model_write_parameters
  implicit none
  external io_relay_process
  external io_server_process
  integer :: status, input
  integer :: me, nio_node

  type(MPI_Comm) :: comm
  integer :: rank, size, nserv, noops
  logical :: error
  character(len=128) :: arg
  type(memory_arena) :: ma
  type(comm_rank_size) :: fullnode_crs, local_crs

  integer, parameter :: NUM_NODES = 3
  logical :: server_node, single_node
  integer :: node_rank

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

  ! call IOSERVER_debug(1)            ! activate debug mode

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
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/4, GBYTE/2)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  server_node = am_server_node(node_rank, single_node)

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
    ! nio_node = -1
    if (node_rank < nserv) then
      call set_IOserver_server(io_server_process)
      status = ioserver_init(nio_node, 'O')   ! this function should not return as set_IOserver_server is set
    else 
      if (single_node) then
        !--------------------------------------------------------------
        ! Relay + model processes, when eeeveryone in on the same node
        call set_IOSERVER_relay(io_relay_process)
        !  no return from ioserver_int_init in the case of IO relay processes when io_relay_fn is defined
        !  compute processes will return from call
        status = ioserver_init(nio_node, 'M')
        call model_process()
        write(6,*)'END: compute, PE',rank+1,' of',size
      else
        !-----------------
        ! no-op processes
        status = ioserver_init(nio_node, 'Z')
      end if
    end if

  endif
777 continue
  call ioserver_set_time_to_quit()
!  write(6,*)'FINAL: serv node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program


subroutine io_server_process()
  use mpi_f08
  use model_write_parameters
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use io_server_mod
  implicit none

  type(MPI_Comm) :: global_comm, consumer_comm
  integer :: global_rank, global_size
  type(distributed_circular_buffer) :: data_buffer
  logical :: success
  integer :: num_producers

  call get_local_world(global_comm, global_rank, global_size)
  call io_server_mod_init()

  ! write(6, *) 'SERVER process! PE', server_crs % rank + 1, ' of', server_crs % size, ' global:', global_rank + 1

  ! Create the DCB used for this test
  num_producers = allio_crs % size - server_crs % size
  success = data_buffer % create(allio_crs % comm, server_crs % comm, num_producers, num_channels, NUM_DCB_ELEMENTS)

  if (.not. success) then
    write(6, *) 'Unable to create DCB (from SERVER process)'
    error stop 1
  end if

  ! Choose what to do based on whether we are a consumer or a channel process
  if (data_buffer % get_consumer_id() >= 0) then
    call MPI_Comm_split(server_crs % comm, 0, data_buffer % get_consumer_id(), consumer_comm)
    call consumer_process(data_buffer, consumer_comm)
  else if (data_buffer % get_channel_id() >= 0) then
    call MPI_Comm_split(server_crs % comm, 1, data_buffer % get_channel_id(), consumer_comm)
    call channel_process(data_buffer)
  else
    write(6, *) 'We have a problem'
    error stop 1
  end if

  call data_buffer % delete()

  call MPI_Barrier(allio_crs % comm) ! To avoid scrambling printed stats
  call MPI_Barrier(allio_crs % comm) ! To avoid scrambling printed stats

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

subroutine consumer_process(data_buffer, consumer_comm)
  use mpi_f08
  use model_write_parameters
  use circular_buffer_module, only : DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use ioserver_functions
  use data_serialize
  implicit none

  interface
    function c_gethostid() result(h) bind(C,name='gethostid') ! SMP host identifier
      import :: C_LONG
      integer(C_LONG) :: h
    end function c_gethostid
  end interface

  type(distributed_circular_buffer), intent(inout) :: data_buffer
  type(MPI_Comm), intent(in) :: consumer_comm

  integer, parameter :: WRITE_BUFFER_SIZE = 50000

  integer :: consumer_id, num_producers, num_consumers, producer_id
  integer :: i_producer, i_data_check, i_print
  integer :: num_elements, message_size
  integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: message, expected_message
  integer(DATA_ELEMENT), dimension(WRITE_BUFFER_SIZE) :: file_write_buffer
  integer :: file_write_position, write_size
  logical :: finished
  integer :: num_errors
  integer :: num_active_producers
  integer :: num_data, data_start
  type(model_record) :: record
  integer :: jar_status, jar_num_elem, old_record_length
  integer :: dummy_integer
  integer :: assembly_status

  type(grid_assembly) :: partial_grids

  integer, allocatable :: active_producers(:)

  integer           :: file_unit
  character(len=14) :: file_name

  JAR_DECLARE(data_jar)

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
      num_elements = data_buffer % get(i_producer, message, 2, .true.)
      write (6, *) 'Received HI from relay #', i_producer, message(1), message(2)
      if (message(1) == 0) then
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

      ! The buffer is empty, so it has not finished sending stuff. Just move on to the next
      if (data_buffer % get_num_elements(producer_id) == 0) then
        finished = .false.
        cycle
      end if

      num_elements = data_buffer % peek(producer_id, message_size, 1)

      if (num_elements < 0) then
        print *, 'Error after peeking into DCB'
        error stop 1
      end if

      if (message_size > MAX_DCB_MESSAGE_SIZE) then
        print *, 'Message is larger than what we can deal with. That is problematic.'
        print *, 'Message size: ', message_size
        print *, 'num_elements = ', num_elements
        num_elements = data_buffer % get_capacity(producer_id)
        print *, 'capacity     = ', num_elements
        print *, 'producer id  = ', producer_id
        num_elements = data_buffer % peek(producer_id, record, storage_size(record) / storage_size(dummy_integer))
        print '(A12, I5, I6, I4, I5, I5, I5, I5, I5, I5, I5, I5, I4, I4, I6, I5, I5)', 'Record info: ', &
          record % record_length, record % tag, record % stream, &
          record % ni, record % nj, record % gnignj, &
          record % grid_size_i, record % grid_size_j, record % output_grid, record % i0, record % j0, &
          record % nk, record % nvar, record % type_kind_rank, &
          record % csize, record % msize
        error stop 1
      end if

      if (message_size > 0)  then
        ! There is something in the buffer!
        finished = .false.
        num_elements = data_buffer % get(producer_id, message, message_size, .true.)

        if (num_elements < 0) then
          print *, 'Error after reading data from DCB'
          error stop 1
        end if

        jar_status   = JAR_FREE(data_jar)
        jar_status   = data_jar % shape(message, message_size)
        jar_num_elem = JAR_GET_ITEM(data_jar, record)
        jar_num_elem = JAR_GET_ITEM(data_jar, old_record_length)

        num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size (tkr) into account

        ! print '(A12, I5, I6, I4, I5, I5, I5, I5, I5, I5, I5, I5, I4, I4, I6, I5, I5)', 'Record info: ', &
        !   record % record_length, record % tag, record % stream, &
        !   record % ni, record % nj, record % gnignj, &
        !   record % grid_size_i, record % grid_size_j, record % output_grid, record % i0, record % j0, &
        !   record % nk, record % nvar, record % type_kind_rank, &
        !   record % csize, record % msize

        ! Sanity check
        if (num_data .ne. record % record_length - old_record_length - 1) then
          print *, 'Record length is not equal to old length + num data. Aborting.'
          print *, old_record_length, num_data, record % record_length
          error stop 1
        end if

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
      else
        ! This should not happen
        write (6, *) 'message_size: ', message_size
        write (6, *) 'This line should not ever be printed'
        num_errors = num_errors + 1
        error stop 1
      end if

      if (CHECK_DCB_MESSAGES) then
        jar_status   = JAR_FREE(data_jar)
        jar_status   = data_jar % shape(message, message_size)
        jar_num_elem = JAR_GET_ITEM(data_jar, record)

        num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size into account!!!!
        data_start = record % record_length - num_data + 1
        expected_message(1) = message(data_start)
        do i_data_check = 2, num_data
           expected_message(i_data_check) = compute_data_point(message(data_start), record % tag, i_data_check)
        end do
        if (.not. all(expected_message(1:num_data) == message(data_start:data_start + num_data))) then
          num_errors = num_errors + 1
          print *, 'Expected: ', expected_message(1:num_data)
          print *, 'Received: ', message(data_start:data_start + num_data)
          error stop 1
        end if
      end if

    end do
  end do

  write(file_unit) file_write_buffer(1:file_write_position)
  close(file_unit)
  file_write_position = 1

  ! Final check on the buffers' content
  do i_producer = consumer_id + 1, num_active_producers, num_consumers
    producer_id = active_producers(i_producer)
    if (data_buffer % get_num_elements(producer_id) .ne. 1) then
      num_errors = num_errors + 1
      write (6, *) 'ERROR: buffer should be empty at the end of test'
    end if
  end do

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from SERVER (consumer) process'
    error stop 1
  end if

end subroutine consumer_process

subroutine model_process()
  use ISO_C_BINDING
  use mpi_f08

  use model_write_parameters
  use circular_buffer_module
  use rpn_extra_module, only: sleep_us
  use io_common_mod
  implicit none

  type(comm_rank_size)  :: node_crs, local_compute_crs
  integer               :: local_compute_id
  integer               :: num_errors
  type(circular_buffer) :: data_buffer
  integer               :: num_spaces

  integer :: f_status, h_status
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

  node_heap = ioserver_heap(0)

  f_status = results_file % open('model_write_results')
  if (f_status .ne. 0) then
    print *, 'Unable to open model file!!!!'
    error stop 1
  end if

  model_crs = IOserver_get_crs(MODEL_COLOR)

  num_errors        = 0
  node_crs          = IOserver_get_crs(NODE_COLOR + MODEL_COLOR + RELAY_COLOR)
  local_compute_crs = IOserver_get_crs(NODE_COLOR + MODEL_COLOR)
  local_compute_id  = local_compute_crs % rank

  ! print *, 'MODEL, local compute id: ', local_compute_id

  data_buffer = IOserver_get_cio_out()
  if (data_buffer % get_capacity() .ne. data_buffer % get_num_spaces()) then
    print *, 'AAAAaaaahhhhh local CB is NOT EMPTY. Aaaaahhh'
    error stop 1
  end if

  ! NODE barrier to signal RELAY processes that the CB (from each MODEL) is ready
  call MPI_Barrier(node_crs % comm)

  ! Send a first test signal
  num_spaces = data_buffer % atomic_put(local_compute_id, 1, .true.)

  ! Init area info
  my_grid % i0 = local_compute_id * CB_MESSAGE_SIZE + 1
  my_grid % ni = CB_MESSAGE_SIZE
  my_grid % j0 = 1 ! cause ARRAYS START AT 1
  my_grid % nj = 1
  my_grid % nk = 1
  my_grid % nv = 1

  input_grid % id = 1
  input_grid % size_i = CB_MESSAGE_SIZE * model_crs % size
  input_grid % size_j = 1

  output_grid % id = 1

  ! call sleep_us(5000)
  block
    integer :: i, j
    integer(C_INTPTR_T) :: tmp
    do i = 0, CB_TOTAL_DATA_TO_SEND / CB_MESSAGE_SIZE
      ! Get memory and put data in it
      msg_array_info = node_heap % allocate(msg_array, [CB_MESSAGE_SIZE])
      call block_meta_internals(msg_array_info, p, d, tkr, o)
      do while (.not. c_associated(p))
        ! print*, 'Could not allocate!!!!! Trying again soon'
        call sleep_us(10)
        msg_array_info = node_heap % allocate(msg_array, [CB_MESSAGE_SIZE])
        call block_meta_internals(msg_array_info, p, d, tkr, o)
        ! error stop 2
      end do

      msg_array(1) = local_compute_id
      do j = 2, CB_MESSAGE_SIZE
        msg_array(j) = compute_data_point(local_compute_id, i + 2, j)
      end do

      ! if (local_compute_id == 0) then
      !   print *, 'Sending', msg_array(1:3), transfer(p, tmp)
      ! end if
      ! Write the data to a file (i.e. send it to the server to do that for us)
      f_status = results_file % write(msg_array_info, my_grid, input_grid, output_grid)

      if (f_status .ne. 0) then
        print *, 'ERROR while trying to do a WRITE'
        error stop 1
      end if

      call sleep_us(50)
    end do

    ! We done. Send the stop signal
    num_spaces = data_buffer % atomic_put(0, 1, .true.)
  end block

  f_status = results_file % close()
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
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  use distributed_circular_buffer_module, only : distributed_circular_buffer
  use io_relay_mod
  use ioserver_memory_mod
  use data_serialize
  implicit none

  type(MPI_Comm)       :: global_comm
  integer              :: global_rank, global_size
  type(comm_rank_size) :: local_relay_crs, local_model_crs
  integer              :: num_local_compute, local_relay_id, num_local_relays, producer_id
  integer              :: i_compute, index, num_errors
  character(len=8)     :: compute_name
  integer              :: bsize, bflags
  type(C_PTR)          :: tmp_ptr
  integer :: h_status

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

  ! print *, 'RELAY, local relay id: ', local_relay_id

  ! Create the DCB used to communicate with the server
  success = data_buffer % create(allio_crs % comm, MPI_COMM_NULL, -1, -1, -1)
  if (.not. success) then
    write(6, *) 'Unable to create DCB from RELAY process'
    error stop 1
  end if

  producer_id = data_buffer % get_producer_id()

  ! NODE barrier to allow MODEL processes to initialize their CBs
  call MPI_Barrier(nodecom_crs % comm)

  ! Recover all CBs that are stored on this node and check that they are valid (even if we don't necessarily access all of them)
  index = 1

  block
    
    integer :: i_data_check
    integer(DATA_ELEMENT), dimension(CB_MESSAGE_SIZE) :: cb_message, expected_message
    integer(DATA_ELEMENT), dimension(MAX_DCB_MESSAGE_SIZE) :: dcb_message
    integer :: current_message_size, model_message_size, blind_message_size
    integer :: num_elements, num_spaces
    logical :: finished = .false.

    integer, dimension(:), pointer :: f_data
    type(C_PTR) :: c_data
    integer :: num_data

    type(model_record) :: record
    integer :: jar_status, jar_num_elem, record_size
    JAR_DECLARE(data_jar)

    integer(C_INTPTR_T) :: tmp

    ! Say hi to the consumer processes
    dcb_message(1) = local_relay_id
    dcb_message(2) = global_rank
    num_spaces = data_buffer % put(dcb_message, 2, .true.)

    if (local_relay_id == 0) then ! only 1 relay per node sends output

      ! Get the initial test signal from each CB this relay is responsible for
      do i_compute = 0, num_local_compute - 1 !, num_local_relays
        cb_message(:) = -1
        num_elements = c_cio_out(i_compute) % atomic_get(cb_message, 1, .true.)
        if (num_elements < 0) then
          print *, 'ERROR in relay. atomic_get returned a negative value'
          error stop 1
        end if
      end do

      ! The main loop
      expected_message(:) = -1
      current_message_size = 0
      do while (.not. finished)
        finished = .true.
        do i_compute = 0, num_local_compute - 1

          ! The buffer is empty, so it has not finished sending stuff. Move on to the next CB
          if (c_cio_out(i_compute) % get_num_elements() == 0) then
            finished = .false.
            cycle
          end if

          num_elements = c_cio_out(i_compute) % peek(model_message_size, 1)
          if (model_message_size > 0) then
            ! There is something in the buffer!
            finished = .false.

            ! Read the content of the CB
            num_elements = c_cio_out(i_compute) % atomic_get(cb_message, model_message_size + 1, .true.)

            if (num_elements < 0) then
              print *, 'ERROR when getting stuff from CIO_OUT', i_compute
            end if

            jar_status   = JAR_FREE(data_jar)
            jar_status   = data_jar % shape(cb_message, model_message_size + 1)
            jar_num_elem = JAR_GET_ITEM(data_jar, record)

            c_data = ptr_translate_from(record % data, MODEL_COLOR, i_compute)
            num_data = record % ni * record % nj * record % nk * record % nvar ! TODO: take var size into account!!!!
            call c_f_pointer(c_data, f_data, [num_data])
            ! print *, 'f_data: ', f_data, num_data

            ! print '(A12, I4, I5, I3, I4, I4, I4, I4, I4, I4, I4, I3, I3, I4, I4)', 'Record info: ', &
            !   record % record_length, record % tag, record % stream, &
            !   record % ni, record % nj, record % gnignj, &
            !   record % gin, record % gout, record % i0, record % j0, &
            !   record % nk, record % nvar, record % csize, record % msize

            cb_message(1) = record % record_length + num_data + 1

            ! If the DCB message buffer is too full to contain that new package, flush it now
            if (current_message_size + model_message_size + 1 + num_data > MAX_DCB_MESSAGE_SIZE) then
              num_spaces = data_buffer % put(dcb_message, current_message_size, .true.)
              current_message_size = 0
            end if

            ! Copy the CB message header to the DCB message buffer
            dcb_message(current_message_size + 1: current_message_size + model_message_size + 1) = cb_message(1:model_message_size + 1)
            current_message_size = current_message_size + model_message_size + 1

            ! Copy the data to the DCB message buffer
            dcb_message(current_message_size + 1: current_message_size + num_data) = f_data(:)
            current_message_size = current_message_size + num_data

            if (CHECK_CB_MESSAGES) then
              expected_message(1) = i_compute
              do i_data_check = 2, num_data
                expected_message(i_data_check) = compute_data_point(i_compute, record % tag, i_data_check)
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

            f_data(2) = -1
            h_status = c_heaps(i_compute) % free(c_data)
            if (h_status .ne. 0) then
              print*, 'Unable to free heap data (from RELAY)'
            end if

          end if
        end do
      end do

      dcb_message(current_message_size + 1) = 0 ! Put a 'stop' signal at the end of the DCB message buffer
      ! Send the stop signal along with the remaining data
      num_spaces = data_buffer % put(dcb_message, current_message_size + 1, .true.)

    end if
  end block

  call data_buffer % delete()

  call MPI_Barrier(allio_crs % comm) ! To avoid scrambling printed stats

  if (local_relay_id == 0) then
    call c_heaps(0) % dumpinfo()
    do i_compute = 0, num_local_compute - 1
      call c_cio_out(i_compute) % print_stats(producer_id * 100 + i_compute, i_compute == 0)
    end do
  end if

  call MPI_Barrier(allio_crs % comm) ! To avoid scrambling printed stats

  if (num_errors > 0) then
    write (6, *) 'Terminating with error from RELAY process'
    error stop 1
  end if

  call MPI_Barrier(nodecom_crs % comm) ! Sync with models and avoid scrambling output

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

