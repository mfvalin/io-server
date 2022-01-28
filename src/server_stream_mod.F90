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
  ! use circular_buffer_module
  use grid_assembly_module
  ! use ioserver_constants
  use ioserver_message_module
  use heap_module
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
  integer, parameter :: STREAM_STATUS_OPEN          =  1 !< Stream is open
  integer, parameter :: STREAM_STATUS_CLOSED        =  2 !< Stream is closed (implying it has been opened before)

  !> Derived type that handles a server stream in shared memory. This is used to reassemble grids that
  !> are transmitted through that stream. Any server process can contribute to the grids, but only the
  !> owner can trigger the processing of a completed grid (interpolation, writing to file, etc.)
  type, public :: shared_server_stream
    private
    integer :: stream_id    = -1     !< Stream ID, for internal use. Assigned at object creation, associated with a model stream when opening it
    integer :: unit         = -1     !< Fortran file unit, when open
    integer :: owner_id     = -1     !< Who owns (will read/write/process) this stream and its physical file
    integer :: mutex_value  = 0      !< When we need to lock this stream with a mutex. Don't ever touch this value directly (i.e. other than through a mutex object)
    integer :: status       = STREAM_STATUS_UNINITIALIZED !< Status of the stream object
    character(len=:), allocatable :: name

    !> Object where grids sent to this stream are assembled
    type(grid_assembly) :: partial_grid_data = grid_assembly(grid_assembly_line())

    contains
    private
    procedure, pass :: open       => shared_server_stream_open
    procedure, pass :: close      => shared_server_stream_close
    procedure, pass :: flush_data => shared_server_stream_flush_data

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
    integer             :: server_id = -2 !< On which process this local instance is located (default different from the default owner ID of the shared stream)
    logical             :: got_open_request = .false. !< Whether someone *tried* to open the underlying stream (only the owner of the stream will actually open it)
    type(simple_mutex)  :: mutex                      !< Used to protect access to the underlying 
    type(heap)          :: data_heap                  !< Heap where we can get space in shared memory
    type(shared_server_stream), pointer :: shared_instance => NULL() !< Pointer to the underlying shared stream

    contains

    procedure, pass :: init     => local_server_stream_init
    procedure, pass :: is_init  => local_server_stream_is_init
    procedure, pass :: is_owner => local_server_stream_is_owner

    procedure, pass :: open       => local_server_stream_open
    procedure, pass :: close      => local_server_stream_close
    procedure, pass :: is_open    => local_server_stream_is_open
    procedure, pass :: put_data   => local_server_stream_put_data
    procedure, pass :: flush_data => local_server_stream_flush_data
  end type local_server_stream

contains

  !> Create a shared server stream object. This ensures that the stream has an ID and an owner.
  !> This sets its status to "initialized" (but it's not open yet!)
  function new_shared_server_stream(stream_id, owner_id)
    implicit none
    integer, intent(in) :: stream_id   !< ID of the stream. Will be used to associate it with a "model stream"
    integer, intent(in) :: owner_id    !< Which server process will own this stream
    type(shared_server_stream) :: new_shared_server_stream
    new_shared_server_stream % stream_id = stream_id
    new_shared_server_stream % owner_id  = owner_id
    new_shared_server_stream % status    = STREAM_STATUS_INITIALIZED
  end function new_shared_server_stream

  !> Open this shared stream. This opens an actual file in the filesystem
  !> @return .true. if the file was successfully opened (even if it had already been opened with the same name),
  !> .false. if it was previously opened with a different name
  function shared_server_stream_open(this, file_name) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    character(len=*),            intent(in)    :: file_name !< [in] Base name of the file to open for that stream
    logical :: success

    success = .false.
    if (.not. this % is_open()) then
      this % name = this % make_full_filename(file_name)

      open(newunit = this % unit, file = this % name, status = 'replace', form = 'unformatted')
      print '(A, A, A, I4, A, I6, A, I4)', 'Opened file, name = ', this % name, ', stream = ', this % stream_id, ', unit = ', this % unit, ', owner ID = ', this % owner_id
      this % status = STREAM_STATUS_OPEN
      success = .true.
    else if (this % name .ne. this % make_full_filename(file_name)) then
      print *, 'ERROR: Trying to open twice the same file, but with a different name'
    else
      ! File was already open, with the same name
      success = .true.
    end if
  end function shared_server_stream_open

  !> Close this shared stream. Closes the file associated with it
  !> @return .true. if the stream was closed peacefully (i.e. there were no incomplete grids left) or if it was already closed,
  !> .false. if it didn't close or if there were incomplete grids and it was forcefully closed
  function shared_server_stream_close(this, data_heap, force_close) result(success)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    type(heap),                  intent(inout) :: data_heap     !< [in,out] Heap from which we can allocate/access shared memory
    logical,                     intent(in)    :: force_close   !< [in] Whether to close the stream even if some grids have been started but not completed
    logical :: success

    integer :: num_flushed, num_incomplete

    integer, parameter :: MAX_NUM_ATTEMPTS = 50
    integer, parameter :: WAIT_TIME_US     = 100000
    integer :: i

    success = .false.

    num_incomplete = 1
    if (this % is_open()) then
      do i = 1, MAX_NUM_ATTEMPTS
        ! Process completed grids
        num_flushed = this % partial_grid_data % flush_completed_grids(this % unit, data_heap)
        ! if (num_flushed > 0) print *, 'Flushed ', num_flushed, ' completed grids upon closing ', this % name

        ! Find out how many grids are started but incomplete, and decide whether to wait
        num_incomplete = this % partial_grid_data % get_num_partial_grids()
        if (num_incomplete > 0 .and. force_close) then
          print '(I2, A, I4, A, A, A, F6.2, A)', this % get_owner_id(), ' DEBUG: There are still ', num_incomplete, ' incomplete grids in file "', this % name, '", will wait another ', &
                (MAX_NUM_ATTEMPTS - i) * WAIT_TIME_US / 1000000.0, ' second(s)'
        else
          exit
        end if

        call sleep_us(WAIT_TIME_US)
      end do

      ! Close the stream (if appropriate)
      if (num_incomplete == 0 .or. force_close) then
        this % status = STREAM_STATUS_CLOSED
        close(this % unit)
        this % stream_id = -1
        this % owner_id = -1
        success = (num_incomplete == 0)
      end if
    end if
  end function shared_server_stream_close

  !> Flush any grids that are complete within this stream. For now, this just writes them to file
  !> @return How many grids were flushed
  function shared_server_stream_flush_data(this, data_heap) result(num_lines_flushed)
    implicit none
    class(shared_server_stream), intent(inout) :: this
    type(heap),                  intent(inout) :: data_heap !< [in,out] Heap from which we can access shared memory
    integer :: num_lines_flushed

    num_lines_flushed = this % partial_grid_data % flush_completed_grids(this % unit, data_heap)
  end function shared_server_stream_flush_data

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

  !> Whether this stream has been closed
  function shared_server_stream_is_closed(this) result(is_closed)
    implicit none
    class(shared_server_stream), intent(in) :: this
    logical :: is_closed
    is_closed = (this % status == STREAM_STATUS_CLOSED)
  end function shared_server_stream_is_closed

  !> Create a complete filename from the given base name. This adds a suffix to the base name after removing trailing nulls from it.
  function make_full_filename(filename) result(full_filename)
    implicit none
    character(len=*), intent(in) :: filename          !< [in] The base name to give to the file
    character(len=:), allocatable :: trimmed_filename
    character(len=:), allocatable :: full_filename
    integer :: last_char

    trimmed_filename = trim(filename)
    do last_char = 1, len(trimmed_filename)
      if (trimmed_filename(last_char:last_char) == achar(0)) exit
    end do
    
    full_filename =  trimmed_filename(:last_char-1)// '.out'
  end function make_full_filename

  !> Check whether this stream has been opened with the given base name
  function shared_server_stream_is_same_name(this, filename) result(is_same_name)
    implicit none
    class(shared_server_stream), intent(in) :: this
    character(len=*),     intent(in) :: filename !< [in] Base name to check against
    logical :: is_same_name

    is_same_name = (this % name == this % make_full_filename(filename))
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
    if (allocated(this % name)) then
      print '(A, I4, I4, I4, I4)', this % name, this % stream_id, this % unit, this % owner_id, this % mutex_value
    else
      print '(I4, I4, I4, I4)', this % stream_id, this % unit, this % owner_id, this % mutex_value
    end if
  end subroutine shared_server_stream_print

  !> Initialize this local instance (but *not* the underlying shared memory stream), associate this local instance with its underlying shared stream,
  !> initialize the mutex to point to the shared instance mutex value, associate this local instance with a shared memory heap where the grids will
  !> actually be assembled
  subroutine local_server_stream_init(this, server_id, shared_instance, data_heap)
    implicit none
    class(local_server_stream),          intent(inout) :: this
    integer,                             intent(in)    :: server_id         !< [in] ID of the stream this local instance will access
    type(shared_server_stream), pointer, intent(inout) :: shared_instance   !< [in,out] Pointer to the underlying shared instance
    type(heap),                          intent(inout) :: data_heap         !< [in,out] Heap from which to allocate/access shared memory

    this % server_id        =  server_id
    this % got_open_request =  .false.
    this % shared_instance  => shared_instance
    this % data_heap        =  data_heap
    call this % mutex % init_from_int(this % shared_instance % mutex_value, this % server_id)
  end subroutine local_server_stream_init

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
    is_owner = (this % server_id == this % shared_instance % get_owner_id())
  end function local_server_stream_is_owner

  !> Open the underlying shared stream, if we are its owner
  !> @return .true. if the underlying instance was successfully opened or if we are *not* the owner of that instance,
  !> .false. if we are the owner and opening it failed.
  function local_server_stream_open(this, file_name) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    character(len=*),           intent(in)    :: file_name !< [in] Base name of the file to open with the shared stream
    logical :: success

    this % got_open_request = .true.
    if (this % is_owner()) then
      success = this % shared_instance % open(file_name)
    else
      success = .true.
    end if
  end function local_server_stream_open

  !> Close the underlying shared stream, if we are its owner
  !> @return The result of closing the underlying stream (if we are indeed the owner), .true. otherwise
  function local_server_stream_close(this, force_close) result(success)
    implicit none
    class(local_server_stream), intent(inout) :: this
    logical,                    intent(in)    :: force_close !< [in] Whether to force close the stream. If .false. we will just *try* to close it.
    logical :: success

    if (this % is_open() .and. this % is_owner()) then
      success = this % shared_instance % close(this % data_heap, force_close)
    else
      ! print *, 'DEBUG: Not the owner, so wont even try to close'
      success = .true.
    end if
  end function local_server_stream_close

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
    integer :: num_flushed

    success = .false.
    if (this % is_open()) then
      ! The stream is open, just move on to adding the data
      success = .true.
    else if ((.not. this % is_owner()) .and. (this % got_open_request)) then
      ! The stream is not open (yet), so wait until it is, or until we reach a certain timeout
      print '(I2, A, I3, A, F6.3)', this % server_id, " DEBUG: File from ", this % shared_instance % get_owner_id(), &
            " is not open yet, let's wait a bit before trying to put data in it. ", MAX_NUM_ATTEMPTS * WAIT_TIME_US / 1000000.0
      do i = 1, MAX_NUM_ATTEMPTS
        if (this % is_open()) then
          success = .true.
          ! print *, 'DEBUG: Done waiting!'
          exit
        else if (this % shared_instance % is_closed()) then
          print *, 'ERROR: Stream has been closed, we will no longer be able to put anything in it'
          exit
        end if
        ! print *, 'DEBUG: Still waiting...'
        call sleep_us(WAIT_TIME_US)
      end do
    end if

    if (.not. success) then
      ! Still not open after the timeout...
      print *, 'ERROR: Trying to put data into a stream that is not open'
      return
    end if

    success = this % shared_instance % partial_grid_data % put_data(record, subgrid_data, this % data_heap, this % mutex)
    ! Try again a few times, after waiting a bit, instead of just crashing right away
    do i = 1, MAX_NUM_ATTEMPTS
      if (.not. success) then
        print '(I2, A, I2, A, I4, A, I3, A)', this % server_id, ' DEBUG: Could not put the data into the grid for owner ', &
            this % shared_instance % get_owner_id(), '. Trying again in ', &
            WAIT_TIME_US / 1000, ' ms (', MAX_NUM_ATTEMPTS - i, ' attempts left)'

        num_flushed = this % flush_data() ! Try to flush any completed grid

        ! if (num_flushed > 0) then
        !   print *, 'Flushed grid(s)!', num_flushed
        ! end if

        if (num_flushed == 0) call sleep_us(WAIT_TIME_US)
        success = this % shared_instance % partial_grid_data % put_data(record, subgrid_data, this % data_heap, this % mutex)
      else
        exit ! We're good, can finish now
      end if
    end do

  end function local_server_stream_put_data

  !> Flush/process any completed grid in the shared instance
  !> @return How many grids were flushed
  function local_server_stream_flush_data(this) result(num_flushed)
    implicit none
    class(local_server_stream), intent(inout) :: this
    integer :: num_flushed
    
    num_flushed = 0
    if (this % is_owner()) num_flushed = this % shared_instance % flush_data(this % data_heap)
    ! if (num_flushed > 0) print '(I2, A, I2, A)', this % server_id, ' Flushed ', num_flushed, ' grids'
  end function local_server_stream_flush_data
end module server_stream_module
