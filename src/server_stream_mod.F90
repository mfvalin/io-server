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

  use circular_buffer_module
  use grid_assembly_module
  use ioserver_message_module
  use heap_module
  use process_command_module
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
  integer, parameter :: STREAM_STATUS_OPEN_NEEDED   =  1 !< Opening has been requested, but not completed yet
  integer, parameter :: STREAM_STATUS_OPEN          =  2 !< Stream is open
  integer, parameter :: STREAM_STATUS_CLOSED        =  4 !< Stream is closed (implying it has been opened before)

  integer(C_INT64_T), parameter :: COMMAND_BUFFER_SIZE_BYTES = 50000 !< Size of the buffer used to send commands to the stream owner

  !> Derived type that handles a server stream in shared memory. This is used to reassemble grids that
  !> are transmitted through that stream. Any server process can contribute to the grids, but only the
  !> owner can trigger the processing of a completed grid (interpolation, writing to file, etc.)
  type, public :: shared_server_stream
    private
    integer :: stream_id    = -1     !< Stream ID, for internal use. Assigned at object creation, associated with a model stream when opening it
    integer :: owner_id     = -1     !< Who owns (will read/write/process) this stream and its physical file
    integer :: mutex_value  = 0      !< When we need to lock this stream with a mutex. Don't ever touch this value directly (i.e. other than through a mutex object)
    integer :: status       = STREAM_STATUS_UNINITIALIZED !< Status of the stream object
    logical :: needs_closing = .false. !< Whether we want it to be closed
    character(len=MAX_FILE_NAME_SIZE) :: name
    integer :: name_length  = 0

    type(circular_buffer) :: command_buffer
    integer(C_INT8_T), dimension(COMMAND_BUFFER_SIZE_BYTES) :: command_buffer_data

    !> Object where grids sent to this stream are assembled
    type(grid_assembly) :: partial_grid_data = grid_assembly(grid_assembly_line())

    contains
    private
    procedure, pass :: request_open   => shared_server_stream_request_open
    procedure, pass :: request_close  => shared_server_stream_request_close
    procedure, pass :: close          => shared_server_stream_close
    procedure, pass :: flush_data     => shared_server_stream_flush_data

    procedure, pass :: needs_open   => shared_server_stream_needs_open
    procedure, pass :: needs_close  => shared_server_stream_needs_close
    procedure, pass :: is_open      => shared_server_stream_is_open
    procedure, pass :: is_closed    => shared_server_stream_is_closed
    procedure, pass :: is_valid     => shared_server_stream_is_valid
    procedure, pass :: print        => shared_server_stream_print
    procedure, pass :: is_same_name => shared_server_stream_is_same_name
    procedure, pass :: get_owner_id => shared_server_stream_get_owner_id

    final :: shared_server_stream_finalize

    procedure, nopass, private :: make_full_filename
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

    contains

    procedure, pass :: init     => local_server_stream_init

    procedure, pass :: is_init  => local_server_stream_is_init
    procedure, pass :: is_owner => local_server_stream_is_owner
    procedure, pass :: is_open  => local_server_stream_is_open

    procedure, pass :: put_command    => local_server_stream_put_command
    procedure, pass :: process_stream => local_server_stream_process_stream
    procedure, pass :: request_open   => local_server_stream_request_open
    procedure, pass :: request_close  => local_server_stream_request_close
    procedure, pass :: put_data       => local_server_stream_put_data

    procedure, pass, private :: close => local_server_stream_close
  end type local_server_stream

contains

  !> Create a shared server stream object. This ensures that the stream has an ID and an owner.
  !> This sets its status to "initialized" (but it's not open yet!)
  function new_shared_server_stream(stream_id, owner_id)
    implicit none
    integer, intent(in) :: stream_id   !< ID of the stream. Will be used to associate it with a "model stream"
    integer, intent(in) :: owner_id    !< Which server process will own this stream
    type(shared_server_stream), target :: new_shared_server_stream
    logical :: success
    ! print '(A, I3, A, I2)', 'Creating stream ', stream_id, ' with owner ', owner_id
    new_shared_server_stream % stream_id = stream_id
    new_shared_server_stream % owner_id  = owner_id
    new_shared_server_stream % status    = STREAM_STATUS_INITIALIZED

    success = new_shared_server_stream % command_buffer % create_bytes(c_loc(new_shared_server_stream % command_buffer_data), COMMAND_BUFFER_SIZE_BYTES)
  end function new_shared_server_stream

  !> Trigger the opening process for the shared file. This basically sets the name of the file and
  !> signals that we want to open it.
  function shared_server_stream_request_open(this, file_name, mutex) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    character(len=*),            intent(in)    :: file_name !< [in] Base name of the file to open for that stream
    type(simple_mutex),          intent(inout) :: mutex
    logical :: success

    success = .false.

    if (.not. this % is_valid()) then
      print *, 'ERROR: Requesting opening of an invalid file'
      return
    end if

    ! Try setting the filename
    if (this % status < STREAM_STATUS_OPEN_NEEDED) then
      call mutex % lock()
      if (this % status < STREAM_STATUS_OPEN_NEEDED) then
        this % name = this % make_full_filename(file_name, this % name_length)
        this % status = STREAM_STATUS_OPEN_NEEDED
      end if
      call mutex % unlock()
    end if

    ! Verify that the stream filename is the same as the one requested (in case it was requested multiple times with different ones)
    success = this % is_same_name(file_name)
    if (.not. success) print *, 'ERROR: Requesting opening of a same file with a different name'
  end function shared_server_stream_request_open

  !> Request that this stream be closed by its owner
  function shared_server_stream_request_close(this) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    logical :: success
    success = .false.

    if (this % is_valid()) then
      this % needs_closing = .true.
      success = .true.
    end if
  end function shared_server_stream_request_close

  !> Close this shared stream. Closes the file associated with it
  !> @return .true. if the stream was closed peacefully (i.e. there were no incomplete grids left) or if it was already closed,
  !> .false. if it didn't close or if there were incomplete grids and it was forcefully closed
  function shared_server_stream_close(this, file_unit, data_heap) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    integer,                     intent(in)    :: file_unit     !< [in] Fortran file unit where we write the grids
    type(heap),                  intent(inout) :: data_heap     !< [in,out] Heap from which we can allocate/access shared memory
    logical :: success

    integer :: num_flushed, num_incomplete, old_num_incomplete

    integer, parameter :: WAIT_TIME_US     = 100000
    real,    parameter :: TOTAL_WAIT_TIME_S = 20.0
    integer(kind=8) :: i

    success = .false.

    if (this % is_open()) then
      i = 1
      num_incomplete = this % partial_grid_data % get_num_partial_grids()
      old_num_incomplete = num_incomplete
      do while (real(i * WAIT_TIME_US) / 1000000.0 < TOTAL_WAIT_TIME_S)
        i = i + 1

        ! Process completed grids
        num_flushed = this % partial_grid_data % flush_completed_grids(file_unit, data_heap)

        ! Find out how many grids are started but incomplete
        num_incomplete = this % partial_grid_data % get_num_partial_grids()
        
        if (num_incomplete > 0) then
          ! Still not finished. Gotta wait a bit more before trying to flush again

          if (num_incomplete < old_num_incomplete) then
            ! There's been some progress, so let's reset the wait time
            old_num_incomplete = num_incomplete
            i = 1
          end if

          ! Occasionally print what's holding up the closing process
          if (mod(i, 10_8) == 0) &
            print '(I2, A, I4, A, A, A, F6.2, A)', this % get_owner_id(), ' DEBUG: There are still ', num_incomplete, &
                  ' incomplete grids in file "', this % name(:this % name_length), '", will wait another ', &
                  TOTAL_WAIT_TIME_S - real(i * WAIT_TIME_US) / 1000000.0, ' second(s)'

          call sleep_us(WAIT_TIME_US)
        else
          ! We done!
          exit
        end if
      end do

      ! Close the stream
      this % status = STREAM_STATUS_CLOSED
      this % stream_id = -1
      this % owner_id = -1
      success = (num_incomplete == 0)
    end if
  end function shared_server_stream_close

  !> Flush any grids that are complete within this stream. For now, this just writes them to file
  !> @return How many grids were flushed
  function shared_server_stream_flush_data(this, file_unit, data_heap) result(num_lines_flushed)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    integer,                     intent(in)    :: file_unit
    type(heap),                  intent(inout) :: data_heap !< [in,out] Heap from which we can access shared memory
    integer :: num_lines_flushed

    num_lines_flushed = this % partial_grid_data % flush_completed_grids(file_unit, data_heap)
  end function shared_server_stream_flush_data

  !> Whether this stream has been properly initialized (so whether the constructor has been called on it)
  function shared_server_stream_is_valid(this) result(is_valid)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_valid
    is_valid = (this % status >= 0)
  end function shared_server_stream_is_valid

  !> Whether the opening of this stream has been requested
  function shared_server_stream_needs_open(this) result(needs_open)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: needs_open
    needs_open = (this % status == STREAM_STATUS_OPEN_NEEDED)
  end function shared_server_stream_needs_open

  !> Whether closing this stream was requested
  function shared_server_stream_needs_close(this) result(needs_close)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: needs_close
    needs_close = this % needs_closing
  end function shared_server_stream_needs_close

  !> Whether this stream has been successfully opened
  function shared_server_stream_is_open(this) result(is_open)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_open
    is_open = (this % status == STREAM_STATUS_OPEN)
  end function shared_server_stream_is_open

  !> Whether this stream has been closed
  function shared_server_stream_is_closed(this) result(is_closed)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_closed
    is_closed = (this % status == STREAM_STATUS_CLOSED)
  end function shared_server_stream_is_closed

  !> Create a complete filename from the given base name. This adds a suffix to the base name after removing trailing nulls from it.
  function make_full_filename(filename, name_length) result(full_filename)
    implicit none
    character(len=*), intent(in)  :: filename          !< [in] The base name to give to the file
    integer,          intent(out) :: name_length
    character(len=:), allocatable :: trimmed_filename
    character(len=:), allocatable :: full_filename

    allocate(character(len = len_trim(filename)) :: trimmed_filename)
    trimmed_filename = trim(filename)
    do name_length = 1, len(trimmed_filename)
      if (trimmed_filename(name_length:name_length) == achar(0)) exit
    end do
    
    full_filename =  trimmed_filename(:name_length-1)// '.out'
    name_length = min(name_length + 3, MAX_FILE_NAME_SIZE)
    deallocate(trimmed_filename)
  end function make_full_filename

  !> Check whether this stream has been opened with the given base name
  function shared_server_stream_is_same_name(this, filename) result(is_same_name)
    implicit none
    class(shared_server_stream), intent(in) :: this
    character(len=*),     intent(in) :: filename !< [in] Base name to check against
    integer :: name_length
    logical :: is_same_name

    is_same_name = (this % name(:this % name_length) == this % make_full_filename(filename, name_length))
  end function shared_server_stream_is_same_name

  !> Retrieve the owner ID of this stream
  function shared_server_stream_get_owner_id(this) result(owner_id)
    implicit none
    class(shared_server_stream), intent(in) :: this
    integer :: owner_id
    owner_id = this % owner_id
  end function shared_server_stream_get_owner_id

  !> Check whether the stream is still open, print an error message if it is.
  subroutine shared_server_stream_finalize(this)
    implicit none
    type(shared_server_stream), intent(inout) :: this

    logical :: success
    if (this % is_open()) then
      success = .false.
      print *, 'ERROR: Shared server stream is still open on finalize()'
    else
      success = .true.
    end if
  end subroutine shared_server_stream_finalize

  !> Print member values of this stream
  subroutine shared_server_stream_print(this)
    implicit none
    class(shared_server_stream), intent(in) :: this
    print '(A, I4, I4, I4)', this % name, this % stream_id, this % status, this % owner_id, this % mutex_value
  end subroutine shared_server_stream_print

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

  function local_server_stream_put_command(this, command_content) result(success)
    implicit none
    class(local_server_stream),             intent(inout) :: this
    integer(CB_DATA_ELEMENT), dimension(:), intent(in)    :: command_content
    logical :: success
    success = this % command_buffer % put(command_content, size(command_content, kind=8), CB_DATA_ELEMENT_KIND, .true., thread_safe = .true.)
  end function local_server_stream_put_command

  !> Do any task that needs to be done with this stream: open the underlying file, process a completed grid,
  !> close the file.
  function local_server_stream_process_stream(this) result(finished)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: finished

    integer :: num_flushed
    logical :: success

    finished = .true.
    if (.not. this % is_writer) then
      print *, 'ERROR: This function should only be called from a "writer" process (not a server receiver)'
      return
    end if

    if (.not. this % is_owner()) return
    if (this % shared_instance % status == 0) return

    if (this % shared_instance % needs_open()) then
      if (this % debug_mode) then
        print '(I2, A, A, A, I2)', this % server_id, ' Opening file ',                              &
            this % shared_instance % name(:this % shared_instance % name_length), ', owner ID ',    &
            this % shared_instance % owner_id
      end if
      open(newunit = this % unit, file = this % shared_instance % name(:this % shared_instance % name_length), status = 'replace', form = 'unformatted')
      this % shared_instance % status = STREAM_STATUS_OPEN
    end if

    do while (this % command_buffer % get_num_elements(CB_KIND_INTEGER_8) > 0)
      ! Extract the command and execute it
      block
        integer(CB_DATA_ELEMENT) :: command_size
        integer(CB_DATA_ELEMENT), dimension(500) :: data_buffer
        type(jar) :: command_content
        integer   :: jar_ok
        success = this % command_buffer % get(command_size, 1_8, CB_KIND_INTEGER_8, .true.)
        command_size = command_size - 1
        success = this % command_buffer % get(data_buffer, command_size, CB_KIND_INTEGER_8, .true.) .and. success
        jar_ok = command_content % shape(data_buffer, int(command_size, kind=4))
        if (.not. success .or. jar_ok .ne. 0) then
          print *, 'ERROR: Unable to extract and package command from command buffer'
          error stop 1
        end if
        call process_command(command_content)
      end block
    end do

    num_flushed = this % shared_instance % flush_data(this % unit, this % data_heap)
    finished = .false.

    if (this % shared_instance % is_open() .and. this % shared_instance % needs_close()) then
      if (this % debug_mode) then
        print '(I2, A, A, A, I2)', this % server_id, ' Closing file ',                              &
            this % shared_instance % name(:this % shared_instance % name_length), ', owner ID ',    &
            this % shared_instance % owner_id
      end if
      ! Start closing, waiting (not indefinitely) for any incomplete grids
      success = this % close()
      if (.not. success) then
        print *, 'ERROR: Unable to close file ', this % shared_instance % name
      end if
      finished = .true.
    end if
  end function local_server_stream_process_stream

  !> Open the underlying shared stream, if we are its owner
  !> @return .true. if the underlying instance was successfully opened or if we are *not* the owner of that instance,
  !> .false. if we are the owner and opening it failed.
  function local_server_stream_request_open(this, file_name) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    character(len=*),           intent(in)    :: file_name !< [in] Base name of the file to open with the shared stream
    logical :: success
    success = this % shared_instance % request_open(file_name, this % mutex)
  end function local_server_stream_request_open

  !> Close the underlying shared stream, if we are its owner
  !> @return The result of closing the underlying stream (if we are indeed the owner), .true. otherwise
  function local_server_stream_request_close(this) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: success
    success = this % shared_instance % request_close()
  end function local_server_stream_request_close

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
    type(model_record),         intent(in)     :: record                    !< [in] Record that describes the data to insert
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

  function local_server_stream_close(this) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical :: success
    success = .false.
    if (this % is_open()) then
      success = this % shared_instance % close(this % unit, this % data_heap)
      close(this % unit)
    end if
  end function local_server_stream_close
end module server_stream_module
