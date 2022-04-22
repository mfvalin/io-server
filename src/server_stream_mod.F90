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

module server_stream_module
  use iso_c_binding

  use atomic_module
  use circular_buffer_module
  use grid_assembly_module
  use ioserver_message_module
  use ioserver_timer_module
  use heap_module
  use process_command_internal_module
  use rpn_extra_module, only: sleep_us
  use simple_mutex_module
  implicit none
#if ! defined(VERSION)
#define VERSION 10000
#endif

  private

  public :: block_meta_f08, subgrid_t, grid_t, cmeta

  integer, parameter :: STREAM_STATUS_UNINITIALIZED = -1 !< Not even initialized
  integer, parameter :: STREAM_STATUS_INITIALIZED   =  0 !< Initialized but not open
  ! integer, parameter :: STREAM_STATUS_OPEN_NEEDED   =  1 !< Opening has been requested, but not completed yet
  integer, parameter :: STREAM_STATUS_OPEN          =  2 !< Stream is open
  ! integer, parameter :: STREAM_STATUS_CLOSED        =  4 !< Stream is closed (implying it has been opened before)

  integer(C_INT64_T), parameter :: COMMAND_BUFFER_SIZE_BYTES = 5000 !< Size of the buffer used to send commands to the stream owner

  !> Derived type that handles a server stream in shared memory. This is used to reassemble grids that
  !> are transmitted through that stream. Any server process can contribute to the grids, but only the
  !> owner can trigger the processing of a completed grid (interpolation, writing to file, etc.)
  type, public :: shared_server_stream
    private
    integer :: stream_rank        = -1  !< Rank of this stream in the list of streams. Can be reused after a stream is closed
    integer :: stream_id          = -1  !< Stream ID, for internal use. Assigned at object creation, associated with a model stream when opening it
    integer :: owner_id           = -1  !< Who owns (will read/write/process) this stream and its physical file
    integer :: mutex_value        =  0  !< When we need to lock this stream with a mutex. Don't ever touch this value directly (i.e. other than through a mutex object)
    integer :: status             = STREAM_STATUS_UNINITIALIZED !< Status of the stream object
    integer :: current_command_tag = 0  !< Tag of the latest command processed (or being processed) by the owner of this stream
    integer :: command_tag_counter = 0  !< DO NOT TOUCH DIRECTLY. Atomically accessed counter for the latest command put in the command buffer
    integer :: num_opens           = 0  !< How many times this stream rank was opened

    type(circular_buffer) :: command_buffer
    integer(C_INT8_T), dimension(COMMAND_BUFFER_SIZE_BYTES) :: command_buffer_data

    !> Object where grids sent to this stream are assembled
    type(grid_assembly) :: partial_grid_data = grid_assembly(grid_assembly_line())

    contains
    private
    procedure, pass :: open           => shared_server_stream_open
    procedure, pass :: close          => shared_server_stream_close
    procedure, pass :: get_id         => shared_server_stream_get_id

    procedure, pass :: is_open      => shared_server_stream_is_open
    procedure, pass :: is_valid     => shared_server_stream_is_valid
    procedure, pass :: print        => shared_server_stream_print
    procedure, pass, private :: get_owner_id => shared_server_stream_get_owner_id

    procedure, pass :: get_tag_counter => shared_server_stream_get_tag_counter

  end type shared_server_stream

  interface shared_server_stream
    procedure new_shared_server_stream
  end interface shared_server_stream

  !> Local stream object through which a server process can access a specific shared server stream. It's basically an
  !> interface to the underlying shared object, with extra data, that ensures proper synchronization.
  type, public :: local_server_stream
    private
    integer             :: server_id  = -2      !< On which process this local instance is located (might have duplicates, depending on server process types)
    logical             :: is_writer  = .false. !< Whether this local instance is allowed to write to the underlying file
    integer             :: unit       = -1      !< Fortran file unit, when/if open
    logical             :: debug_mode = .false. !< Whether to print some debug messages
    type(simple_mutex)  :: mutex                !< Used to protect access to the underlying 
    type(heap)          :: data_heap            !< Heap where we can get space in shared memory
    type(shared_server_stream), pointer :: shared_instance => NULL() !< Pointer to the underlying shared stream
    type(circular_buffer) :: command_buffer
    type(atomic_int32)  :: command_counter

    contains

    procedure, pass :: init     => local_server_stream_init

    procedure, pass :: is_init  => local_server_stream_is_init
    procedure, pass :: is_owner => local_server_stream_is_owner
    procedure, pass :: is_open  => local_server_stream_is_open
    procedure, pass :: get_id   => local_server_stream_get_id


    procedure, pass :: put_command    => local_server_stream_put_command
    procedure, pass :: process_stream => local_server_stream_process_stream
    procedure, pass :: put_data       => local_server_stream_put_data
    procedure, pass :: print_command_stats => local_server_stream_print_command_stats

    procedure, pass, private :: open  => local_server_stream_open
    procedure, pass, private :: close => local_server_stream_close
  end type local_server_stream

contains

  !> Create a shared server stream object. This ensures that the stream has an ID and an owner.
  !> This sets its status to "initialized" (but it's not open yet!)
  function new_shared_server_stream(stream_rank, owner_id)
    implicit none
    integer, intent(in) :: stream_rank  !< Rank of the stream within the list of streams
    integer, intent(in) :: owner_id     !< Which server process will own this stream
    type(shared_server_stream), target :: new_shared_server_stream
    logical :: success
    ! print '(A, I3, A, I2)', 'Creating stream ', stream_id, ' with owner ', owner_id
    new_shared_server_stream % stream_rank = stream_rank
    new_shared_server_stream % owner_id    = owner_id

    success = new_shared_server_stream % command_buffer % create_bytes(c_loc(new_shared_server_stream % command_buffer_data), COMMAND_BUFFER_SIZE_BYTES)

    new_shared_server_stream % status = STREAM_STATUS_INITIALIZED
  end function new_shared_server_stream

  function shared_server_stream_get_id(this) result(stream_id)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    integer :: stream_id
    stream_id = this % stream_id
  end function shared_server_stream_get_id

  !> Open this shared stream. Basically checks whether it is initialized and not already open.
  function shared_server_stream_open(this, stream_id) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    integer,                     intent(in)    :: stream_id
    logical :: success !< Whether we were able to actually open it (.false. if it wasn't valid or if it was already open)

    success = .false.
    if (this % is_valid()) then
      if (.not. this % is_open()) then
        this % stream_id = stream_id
        this % status    = STREAM_STATUS_OPEN
        this % num_opens = this % num_opens + 1
        success          = .true.
      end if
    end if
  end function shared_server_stream_open

  !> Close this shared stream.
  !> @return .true. if the stream was closed peacefully (i.e. there were no incomplete grids left)
  !> .false. if it wasn't open or if there were incomplete grids
  function shared_server_stream_close(this) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    logical :: success

    integer :: num_grids

    success = .false.

    if (this % is_open()) then
      num_grids = this % partial_grid_data % get_num_grids()
      if (num_grids > 0) then
        print '(A, I3, A)', 'ERROR: There are still ', num_grids, ' in this stream. We should not be closing it.'
        this % status = STREAM_STATUS_UNINITIALIZED
        return
      end if
      ! Close the stream
      this % status = STREAM_STATUS_INITIALIZED
      this % stream_id = -1
      success = .true.
    end if
  end function shared_server_stream_close

  !> Whether this stream has been properly initialized (so whether the constructor has been called on it)
  function shared_server_stream_is_valid(this) result(is_valid)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_valid
    is_valid = (this % status >= 0)
  end function shared_server_stream_is_valid

  !> Whether this stream has been successfully opened
  function shared_server_stream_is_open(this) result(is_open)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_open
    is_open = (this % status == STREAM_STATUS_OPEN)
  end function shared_server_stream_is_open

  !> Retrieve the owner ID of this stream
  function shared_server_stream_get_owner_id(this) result(owner_id)
    implicit none
    class(shared_server_stream), intent(in) :: this
    integer :: owner_id
    owner_id = this % owner_id
  end function shared_server_stream_get_owner_id

  !> Print member values of this stream
  subroutine shared_server_stream_print(this)
    implicit none
    class(shared_server_stream), intent(in) :: this
    print '(A, I4, A, I4, A, I4, A, I4, A, I2)', 'Stream rank: ', this % stream_rank, ', stream ID: ', this % stream_id,  &
          ', status: ', this % status, ', owner ID: ', this % owner_id, ', mutex value: ', this % mutex_value
  end subroutine shared_server_stream_print

  !> Create an atomic counter from the tag counter variable of this shared_server_stream
  function shared_server_stream_get_tag_counter(this) result(tag_counter)
    implicit none
    class(shared_server_stream), intent(in) :: this   !< shared_server_stream instance
    type(atomic_int32) :: tag_counter
    call tag_counter % init_from_int(this % command_tag_counter)
  end function shared_server_stream_get_tag_counter

  !> Initialize this local instance (but *not* the underlying shared memory stream), associate this local instance with its underlying shared stream,
  !> initialize the mutex to point to the shared instance mutex value, associate this local instance with a shared memory heap where the grids will
  !> actually be assembled
  function local_server_stream_init(this, server_id, is_writer, debug_mode, shared_instance, data_heap) result(success)
    implicit none
    class(local_server_stream),          intent(inout) :: this
    integer,                             intent(in)    :: server_id         !< [in] ID of the stream this local instance will access
    logical,                             intent(in)    :: is_writer         !< [in] Whether the caller is allowed to write to the underlying file
    logical,                             intent(in)    :: debug_mode        !< [in] Whether to print debug messages
    type(shared_server_stream), pointer, intent(inout) :: shared_instance   !< [in,out] Pointer to the underlying shared instance
    type(heap),                          intent(inout) :: data_heap         !< [in,out] Heap from which to allocate/access shared memory

    logical :: success

    this % server_id        =  server_id
    this % is_writer        =  is_writer
    this % debug_mode       =  debug_mode
    this % shared_instance  => shared_instance
    this % data_heap        =  data_heap
    call this % mutex % init_from_int(this % shared_instance % mutex_value, this % server_id)
    this % command_counter = this % shared_instance % get_tag_counter()

    success = this % command_buffer % create_bytes(c_loc(this % shared_instance % command_buffer_data))

  end function local_server_stream_init

  !> Check whether this local stream has been initialized (i.e. it has an ID and is associated with a shared instance)
  function local_server_stream_is_init(this) result (is_init)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_init
    is_init = .false.
    if (this % server_id >= 0) then
      is_init = associated(this % shared_instance)
    end if
  end function local_server_stream_is_init
  
  !> Check wether this local instance is the owner of the underlying shared instance
  function local_server_stream_is_owner(this) result(is_owner)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_owner
    is_owner = (this % server_id == this % shared_instance % get_owner_id()) .and. this % is_writer
  end function local_server_stream_is_owner

  function local_server_stream_get_id(this) result(stream_id)
    implicit none
    class(local_server_stream), intent(inout) :: this
    integer :: stream_id
    stream_id = -1
    if (this % is_init()) stream_id = this % shared_instance % get_id()
  end function local_server_stream_get_id

  function local_server_stream_put_command(this, command_content, tag) result(success)
    implicit none
    class(local_server_stream),             intent(inout) :: this
    integer(CB_DATA_ELEMENT), dimension(:), intent(in)    :: command_content
    integer(C_INT),                         intent(in)    :: tag
    logical :: success

    integer(C_INT32_T) :: old_value
    old_value = this % command_counter % read()
    success = .true.
    if (tag > old_value) then
      if (this % command_counter % try_update(old_value, tag)) then
        ! print '(A, I8, A, I8, I8)', 'Enqueuing command, tag ', tag, ', size ', size(command_content), this % command_counter % read()
        success = this % command_buffer % put(command_content, size(command_content, kind=8), CB_DATA_ELEMENT_KIND, .true., thread_safe = .true., timeout_ms = 0)
      end if
    end if
  end function local_server_stream_put_command

  !> Do any task that needs to be done with this stream: open the underlying file, process a completed grid,
  !> close the file.
  function local_server_stream_process_stream(this) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: success

    ! integer :: num_flushed

    integer(CB_DATA_ELEMENT), dimension(500) :: data_buffer
    type(command_record) :: record

    success = .false.

    ! if (.not. this % is_writer) then
    !   print *, 'ERROR: This function should only be called from a "writer" process (not a server receiver)'
    !   return
    ! end if

    if (.not. this % is_owner()) then
      print '(A)', 'ERROR: I am not the owner of that stream'
      return
    end if
    ! if (this % shared_instance % status == 0) return

    success = .true.
    do while (this % command_buffer % get_num_elements(CB_KIND_INTEGER_8) > 0 .and. success)
      ! Extract the command and execute it

      ! print *, 'Getting from command queue, size ', command_record_size_int8()
      success = this % command_buffer % get(record, command_record_size_int8(), CB_KIND_INTEGER_8, .true.)
      if (.not. success) return
      if (record % message_tag > this % shared_instance % current_command_tag) then
        this % shared_instance % current_command_tag = record % message_tag
        ! call print_command_record(record)

        if (record % command_type == MSG_COMMAND_OPEN_STREAM) then
          success = this % open(record % stream_id) .and. success
          if (.not. success) print '(A, I4)', 'ERROR: Failed to open stream, ID ', record % stream_id

        else if (record % command_type == MSG_COMMAND_CLOSE_STREAM) then
          success = this % close() .and. success
          if (.not. success) print '(A, I4)', 'ERROR: Failed to close stream, ID ', record % stream_id

        else if (record % command_type == MSG_COMMAND_SERVER_CMD .or. record % command_type == MSG_COMMAND_DATA) then

          block
            type(jar) :: command_content ! We want a new jar every time
            type(C_PTR) :: grid_data
            type(ioserver_timer) :: timer

            call timer % create()

            ! print *, 'Getting from command queue, size ', record % size_int8
            success = this % command_buffer % get(data_buffer, record % size_int8, CB_KIND_INTEGER_8, .true.) .and. success
            ! call command_content % reset()
            success = command_content % shape_with(data_buffer, int(record % size_int8, kind=4)) .and. success
            if (.not. success) then
              print '(A, I4)', 'ERROR: Unable to extract and package command from command buffer in stream ', record % stream_id
              call print_command_record(record)
              success = .false.
              return
            end if
            
            if (record % command_type == MSG_COMMAND_SERVER_CMD) then
              ! Simply execute the command
              success = process_command(command_content, this % shared_instance % get_id())

            else if (record % command_type == MSG_COMMAND_DATA) then
              ! First get a pointer to the assembled grid that comes with the command. Might have to wait a bit for it
              call timer % start()
              grid_data = this % shared_instance % partial_grid_data % get_completed_line_data(record % message_tag, this % data_heap)
              call timer % stop()
              if (.not. c_associated(grid_data)) then
                print '(A)', 'ERROR: Could not get completed line data!!!'
                success = .false.
                return 
              end if

              ! print '(A, F8.3, A)', 'DEBUG: Waited ', timer % get_time_ms() / 1000.0, ' seconds for grid to be assembled'

              ! Then execute the command on that assembled grid
              success = process_data(grid_data, command_content, this % shared_instance % get_id())

              ! Free the grid data
              success = this % shared_instance % partial_grid_data % free_line(record % message_tag, this % data_heap, this % mutex) .and. success
            end if

            call timer % delete()
          end block

        else
          print '(A, I3, A)', 'ERROR: Unknown message command ', record % command_type, '. Will just stop processing this stream.'
          call print_command_record(record)
          success = .false.
        end if
      else
        ! Command has already been executed, so skip it
        ! Don't forget to discard data from the already-executed command
        if (record % size_int8 > 0) success = this % command_buffer % get(data_buffer, record % size_int8, CB_KIND_INTEGER_8, .true.)
      end if
    end do
  end function local_server_stream_process_stream

  !> Check whether the underlying shared stream is open
  function local_server_stream_is_open(this) result(is_open)
    implicit none
    class(local_server_stream), intent(in) :: this
    logical :: is_open
    is_open = .false.
    if (this % is_init()) then
      is_open = this % shared_instance % is_open()
    end if
  end function local_server_stream_is_open

  !> Put the given grid data into the underlying shared stream. If the stream is not open yet,
  !> we wait a little bit (it should just be a matter of time).
  !> @return The result of the operation on the underlying stream, if it is open, .false. otherwise
  function local_server_stream_put_data(this, record, subgrid_data) result(success)
    implicit none
    class(local_server_stream), intent(inout)  :: this
    type(data_record),          intent(in)     :: record                    !< [in] Record that describes the data to insert
    integer(kind = 8), intent(in), dimension(:), contiguous, pointer :: subgrid_data  !< [in] Pointer to the data itself
    logical :: success

    integer, parameter :: MAX_NUM_ATTEMPTS = 300
    integer, parameter :: WAIT_TIME_US     = 50000
    integer :: i

    ! First attempt
    success = this % shared_instance % partial_grid_data % put_data(record, subgrid_data, this % data_heap, this % mutex)

    ! Try again a few times, after waiting a bit, instead of just crashing right away
    do i = 1, MAX_NUM_ATTEMPTS
      if (.not. success) then
        if (mod(i, 10) == 0) then
          print '(I2, A, I2, A, F5.2, A)', this % server_id, ' WARNING: Could not put the data into the grid for owner ', &
              this % shared_instance % get_owner_id(), '. Trying repeatedly for another ', &
              (MAX_NUM_ATTEMPTS - i) * WAIT_TIME_US / 1000000.0, 's'
        end if

        call sleep_us(WAIT_TIME_US)
        success = this % shared_instance % partial_grid_data % put_data(record, subgrid_data, this % data_heap, this % mutex)
      else
        exit ! We're good, can finish now
      end if
    end do

  end function local_server_stream_put_data

  subroutine local_server_stream_print_command_stats(this, id, with_header)
    implicit none
    class(local_server_stream), intent(in) :: this
    integer, intent(in) :: id
    logical, intent(in) :: with_header
    if (this % is_init()) then
      if (this % shared_instance % num_opens > 0) then
        call this % command_buffer % print_stats(id, with_header)
      end if
    end if
  end subroutine local_server_stream_print_command_stats

  function local_server_stream_open(this, stream_id) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    integer,                    intent(in)    :: stream_id
    logical :: success

    success = .false.
    if (this % is_init() .and. this % is_owner() .and. .not. this % is_open()) then
      success = this % shared_instance % open(stream_id)
    end if
  end function local_server_stream_open

  function local_server_stream_close(this) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: success
    success = .false.
    if (this % is_init() .and. this % is_owner() .and. this % is_open()) then
      success = this % shared_instance % close()
    end if
  end function local_server_stream_close
end module server_stream_module
