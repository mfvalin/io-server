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

module model_stream_module
  use circular_buffer_module
  use heap_module
  use ioserver_message_module
  implicit none

  private

#if ! defined(VERSION)
#define VERSION 10000
#endif

  type, public :: model_stream
    private
    integer               :: version = VERSION    ! version marker, used to check version coherence
    integer               :: stream_id = -1       ! Stream ID, for internal use
    character(len=1), dimension(:), pointer :: name => NULL()
    logical               :: debug = .false.      ! debug mode at the file level
    integer               :: global_rank = -1
    type(heap)            :: local_heap
    type(circular_buffer) :: server_bound_cb
    type(ioserver_messenger), pointer :: messenger => NULL()

    contains

    ! initial, pass :: server_file_construct
    procedure   :: open
    procedure   :: read
    procedure   :: write
    procedure   :: close
    procedure   :: is_open
    procedure   :: is_version_valid
    procedure   :: set_debug

  end type

  interface model_stream
    procedure :: new_model_stream
  end interface model_stream

contains

  function new_model_stream(global_rank, local_heap, server_bound_cb, debug_mode, messenger)
    implicit none
    integer,                intent(in)  :: global_rank
    type(heap),             intent(in)  :: local_heap
    type(circular_buffer),  intent(in)  :: server_bound_cb
    logical,                intent(in)  :: debug_mode
    type(ioserver_messenger), pointer, intent(in) :: messenger
    type(model_stream) :: new_model_stream

    new_model_stream % global_rank = global_rank
    new_model_stream % local_heap  = local_heap
    new_model_stream % server_bound_cb = server_bound_cb
    new_model_stream % debug = debug_mode
    new_model_stream % messenger => messenger
  end function new_model_stream

  function set_debug(this, dbg) result(status)
    implicit none
    class(model_stream), intent(INOUT) :: this
    logical, intent(IN) :: dbg
    logical :: status

    status = this % debug
    this % debug = dbg

  end function set_debug

  function is_version_valid(this) result(status)
    implicit none
    class(model_stream), intent(IN) :: this
    logical :: status

    status = this % version == VERSION

  end function is_version_valid

  function is_open(this) result(status)
    implicit none
    class(model_stream), intent(IN) :: this
    logical :: status

    status = this % is_version_valid()
    status = status .and. (this % stream_id > 0) 
    status = status .and. associated(this % name)

  end function is_open

  function open(this, name) result(success)
    implicit none
    class(model_stream),    intent(INOUT) :: this
    character(len=*),      intent(IN)    :: name
    logical :: success

    integer :: lname
    type(message_header) :: header
    type(message_cap)    :: end_cap

    success = .false.

    if (.not. this % is_version_valid()) return ! wrong version
    if (this % is_open())                return ! already open

    if (.not. this % server_bound_cb % is_valid()) then
      print *, 'Server file has not been initialized!'
      return
    end if

    call this % messenger % bump_tag(.true.)

    this % stream_id = this % messenger % get_file_tag()

    lname = len(trim(name)) + 1
    allocate(this % name(lname))
    this % name(1:lname) = transfer(trim(name) // char(0), this % name) ! Append a NULL character because this might be read in C code

    header % content_length     = lname
    header % command            = MSG_COMMAND_OPEN_FILE
    header % stream_id          = this % stream_id
    header % tag                = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(this % name, int(lname, kind=8), CB_KIND_CHAR, .false.) .and. success
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success       ! Append length and commit message

  end function open

  function close(this) result(success)
    implicit none
    class(model_stream), intent(INOUT) :: this
    logical :: success

    type(message_header) :: header
    type(message_cap)    :: end_cap

    success = .false.
    if (.not. this % is_open()) return

    call this % messenger % bump_tag()

    header % content_length     = 0
    header % stream_id          = this % stream_id
    header % tag                = this  % messenger % get_msg_tag()
    header % command            = MSG_COMMAND_CLOSE_FILE
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    this % stream_id = -1
    deallocate(this % name)
    nullify(this % messenger)
    
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.)  .and. success ! Append length + commit

    ! call print_message_header(header)

  end function close

  ! cprs and meta only need to be supplied by one of the writing PEs
  function write(this, my_data, subgrid_area, global_grid, grid_out, cprs, meta) result(success)
    use iso_c_binding
    use jar_module
    implicit none
    class(model_stream),  intent(INOUT) :: this
    type(block_meta_f08), intent(IN)    :: my_data          !< array descriptor from h % allocate
    type(subgrid_t),      intent(IN)    :: subgrid_area     !< area of this subgrid in global grid
    type(grid_t),         intent(IN)    :: global_grid      !< global grid info
    type(grid_t),         intent(IN)    :: grid_out         !< output grid

    type(cmeta), intent(IN), optional  :: cprs             !< compression related metadata (carried serialized)
    type(jar),   intent(IN), optional  :: meta             !< metadata associated with data (carried serialized and blindly)

    logical :: success

    type(model_record)   :: rec
    type(message_header) :: header
    type(message_cap)    :: end_cap
    integer(JAR_ELEMENT), dimension(:), pointer :: metadata
    integer(JAR_ELEMENT) :: low, high

    success = .false.
    if(this % stream_id <= 0) return

    ! Check that dimensions in area are consistent with metadata
    if (.not. all(my_data % get_dimensions() == subgrid_area % size)) then
      print *, 'ERROR: Trying to send data array that does not match subgrid dimensions'
      return
    end if

    ! Check that given element size is the same as in the global grid
    if (my_data % get_kind() .ne. global_grid % elem_size) then
      print *, 'WARNING: Element size mentioned in global grid does not match data type', global_grid % elem_size, my_data % get_kind()
    end if

    call this % messenger % bump_tag()

    !----------------------
    ! Prepare the message

    ! Size of the various metadata objects included
    rec % cmeta_size = 0
    rec % meta_size  = 0
    if(present(cprs)) then
      rec % cmeta_size = int(cmeta_size_int(), kind=4)
    endif
    if(present(meta)) then
      low  = meta % low()
      high = meta % high()
      rec % meta_size = high - low       ! useful number of elements
      metadata => meta % array()
    endif

    rec % tag            = this % messenger % get_msg_tag()
    rec % stream         = this % stream_id

    rec % subgrid_area   = subgrid_area
    rec % global_grid    = global_grid
    rec % global_grid % elem_size = my_data % get_kind()

    rec % output_grid_id = grid_out % id

    rec % elem_size      = my_data % get_kind()
    rec % heap_offset    = my_data % get_offset()
    rec % data_size_byte = product(rec % subgrid_area % size) * rec % elem_size

    ! print *, rec % ni, rec % nj, rec % nk, rec % nvar, f_block % k()

    ! print *, 'From model'
    ! print *, 'Area % nv = ', area % nv
    ! call print_model_record(rec)

    header % content_length     = int(model_record_size_int() + rec % cmeta_size + rec % meta_size, kind=4)
    header % command            = MSG_COMMAND_DATA
    header % stream_id          = this % stream_id
    header % tag                = this % messenger % get_msg_tag()
    header % sender_global_rank = this % global_rank

    end_cap % msg_length = header % content_length

    !-------------------
    ! Send the message

    ! Put header + data
    success = this % server_bound_cb % put(header, message_header_size_int(), CB_KIND_INTEGER_4, .false.)
    success = this % server_bound_cb % put(rec, model_record_size_int(), CB_KIND_INTEGER_4, .false.) .and. success

    ! Optional parts of the message
    if(present(cprs)) success = this % server_bound_cb % put(cprs, int(rec % cmeta_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success
    if(present(meta)) success = this % server_bound_cb % put(metadata(low + 1 : high), int(rec % meta_size, kind=8), CB_KIND_INTEGER_4, .false.) .and. success

    ! Add the end cap and commit the message
    success = this % server_bound_cb % put(end_cap, message_cap_size_int(), CB_KIND_INTEGER_4, .true.) .and. success
  end function write

  function read(this) result(status)
    implicit none
    class(model_stream), intent(INOUT) :: this
    integer :: status

    status = -1
    if (this % stream_id <= 0) return
    call this % messenger % bump_tag()

    status = 0
  end function read
end module model_stream_module
