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

! ====================================================
!> \file
!> \brief circular buffer Fortran module (object oriented)
module circular_buffer_module
  use ISO_C_BINDING
  use cb_common_module
  use rpn_extra_module
  implicit none
  include 'io-server/circular_buffer.inc'

  private

  public :: CB_KIND_CHAR, CB_KIND_INTEGER_4, CB_KIND_INTEGER_8, CB_KIND_REAL_4, CB_KIND_REAL_8, CB_DATA_ELEMENT, CB_DATA_ELEMENT_KIND
  public :: error_code_to_string
  public :: cb_stats, cb_stats_size_byte, cb_stats_size_int8
  public :: cb_stats_min, cb_stats_max, maxed_cb_stats, print_cumulated_stats

  !> \brief User-defined type for a one-producer, one-consumer circular queue. See #circular_buffer
  type, public :: circular_buffer
    !> \private
    private
    type(C_PTR) :: p = C_NULL_PTR       !< Pointer to internal struct of the #circular_buffer
    logical :: is_shared = .false.    !< Whether the circular_buffer is stored in shared memory
    logical :: is_owner  = .false.    !< Whether the circular_buffer owns the memory where it resides (i.e. whether it is responsible for freeing it)
  contains

    procedure :: create_local_bytes         !< circular_buffer_module::create_local_bytes
    procedure :: create_shared_bytes        !< circular_buffer_module::create_shared_bytes
    procedure :: create_from_pointer_bytes  !< circular_buffer_module::create_from_pointer_bytes
    procedure :: create_from_other          !< circular_buffer_module::create_from_other
    GENERIC   :: create_bytes => create_local_bytes, create_shared_bytes, create_from_pointer_bytes, create_from_other  !< generic create circular buffer
    procedure :: delete !< circular_buffer_module::delete

    procedure :: get_num_spaces   !< circular_buffer_module::get_num_spaces
    procedure :: get_num_elements !< circular_buffer_module::get_num_elements
    procedure :: get_capacity     !< circular_buffer_module::get_capacity
    procedure :: peek             !< circular_buffer_module::peek
    procedure :: get              !< circular_buffer_module::get
    procedure :: put              !< circular_buffer_module::put

    procedure :: is_valid     !< circular_buffer_module::is_valid
    procedure :: get_integrity_status !< circular_buffer_module::get_integrity_status
    procedure :: print_header !< Print the buffer header (to help debugging). circular_buffer_module::print_header
    procedure :: print_stats  !< Print stats collected during the buffer's lifetime. circular_buffer_module::print_stats
    procedure :: get_stats

    procedure, nopass :: error_code_to_string

  end type circular_buffer

  type, public :: cb_stats_real
    real(C_DOUBLE) :: num_reads            = 0
    real(C_DOUBLE) :: num_unique_reads     = 0
    real(C_DOUBLE) :: num_read_elems       = 0
    real(C_DOUBLE) :: num_fractional_reads = 0
    real(C_DOUBLE) :: total_read_wait_time_ms = 0.0
    real(C_DOUBLE) :: total_read_time_ms      = 0.0
    real(C_DOUBLE) :: max_fill             = 0

    real(C_DOUBLE) :: num_writes            = 0
    real(C_DOUBLE) :: num_write_elems       = 0
    real(C_DOUBLE) :: num_fractional_writes = 0
    real(C_DOUBLE) :: total_write_wait_time_ms = 0.0
    real(C_DOUBLE) :: total_write_time_ms      = 0.0
  contains
    procedure :: mean_add_sample
    procedure :: variance_add_sample
    procedure :: stats_add
    procedure :: stats_add_diff_sq
    procedure :: stats_mult_scalar

    procedure, nopass :: mean_add_sample_value
    procedure, nopass :: variance_add_sample_value
    procedure, nopass :: add_diff_sq_value

    procedure, nopass :: print_cumulated_stats

    procedure :: assign_cb_stats                  !< assignment operator, cb_stats_real = cb_stats
    GENERIC :: ASSIGNMENT(=) => assign_cb_stats   !< generic assignment operator
  end type cb_stats_real

contains

  function cb_stats_size_byte()
    implicit none
    integer(C_INT64_T) :: cb_stats_size_byte !< Size of the message_header type in bytes
    type(cb_stats) :: dummy_stats

    cb_stats_size_byte = storage_size(dummy_stats) / 8
  end function cb_stats_size_byte

  function cb_stats_size_int8()
    implicit none
    integer(C_INT64_T) :: cb_stats_size_int8 !< How many 64-bit integers are needed to contain a message_header
    cb_stats_size_int8 = num_char_to_num_int8(cb_stats_size_byte())
  end function cb_stats_size_int8

  !> Check integrity of the circular buffer: the pointer is valid and the integrity check on the underlying C struct passes.
  !> \sa CB_check_integrity
  !> \return A status code that indicate a specific error, if there is one
  pure function get_integrity_status(this) result(integrity_status)
    implicit none
    class(circular_buffer), intent(in) :: this    !< circular_buffer instance
    integer(C_INT) :: integrity_status
    integrity_status = CB_check_integrity(this % p)
  end function get_integrity_status

  !> Check integrity of the circular buffer: the pointer is valid and the integrity check on the underlying C struct passes.
  !> \sa CB_check_integrity
  !> \return Wether the circular buffer passes all checks
  pure function is_valid(this)
    implicit none
    class(circular_buffer), intent(IN) :: this
    logical :: is_valid
    is_valid = (this % get_integrity_status() == CB_SUCCESS)
  end function is_valid

  !> Print the C struct header of this circular buffer. See CB_print_header()
  !> \sa CB_print_header()
  subroutine print_header(this)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    call CB_print_header(this % p)
  end subroutine print_header

  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local_bytes(num_bytes)
  !> <br>p = cb\%create(num_bytes)
  !> \sa CB_create_bytes()
  function create_local_bytes(this, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this      !< circular_buffer instance
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in bytes of the circular buffer
    logical :: success                                     !< Whether the created buffer is valid
    integer(C_INT) :: status

    this % p = CB_create_bytes(num_bytes)
    this % is_owner = .true.
    this % is_shared = .false.
    status = this % get_integrity_status()
    success = (status == CB_SUCCESS)
    if (.not. success) print '(A, A)', 'ERROR: local creation failed: ', error_code_to_string(status)
  end function create_local_bytes

  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared_bytes(shmid, num_bytes)
  !> <br>p = cb\%create(shmid, num_bytes)
  !> \sa CB_create_shared_bytes()
  function create_shared_bytes(this, shmid, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this   !< circular_buffer instance
    integer(C_INT),         intent(OUT)       :: shmid  !< identifier of shared memory area (see man shmget)
    integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success                                  !< Whether the created buffer is valid
    integer(C_INT) :: status
    this % p = CB_create_shared_bytes(shmid, num_bytes)
    this % is_owner = .false.
    this % is_shared = .true.
    status = this % get_integrity_status()
    success = (status == CB_SUCCESS)
    if (.not. success) print '(A, A)', 'ERROR: creation in shared memory failed: ', error_code_to_string(status)
  end function create_shared_bytes

  !> \brief Create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer_bytes(ptr, num_bytes)
  !> <br>p = cb\%create(ptr, num_bytes)
  !> \sa CB_from_pointer_bytes
  function create_from_pointer_bytes(this, ptr, num_bytes) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this      !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr       !< pointer to user supplied memory
    integer(C_SIZE_T), intent(IN), value  :: num_bytes !< size in 32 bit elements of the circular buffer
    logical :: success                                 !< Whether the created buffer is valid
    integer(C_INT) :: status

    this % p = CB_from_pointer_bytes(ptr, num_bytes)
    this % is_owner = .false.
    this % is_shared = .false.

    status = this % get_integrity_status()
    success = (status == CB_SUCCESS)
    if (.not. success) print '(A, A)', 'ERROR: creation from pointer failed: ', error_code_to_string(status)
  end function create_from_pointer_bytes

  !> \brief Create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(this, ptr) result(success)
    implicit none
    class(circular_buffer), intent(INOUT) :: this !< circular_buffer
    type(C_PTR), intent(IN), value        :: ptr  !< pointer to user supplied memory
    logical :: success                            !< Whether the resulting buffer is valid
    integer(C_INT) :: status

    this % p = ptr
    this % is_owner = .false.
    this % is_shared = .false.

    status = this % get_integrity_status()
    success = (status == CB_SUCCESS)
    if (.not. success) print '(A, A)', 'ERROR: creation from existing CB failed: ', error_code_to_string(status)
  end function create_from_other
  
  !> \brief Get number of empty element slots available in the buffer
  !> num_integers = cb\%get_num_spaces(CB_KIND_INTEGER_4)
  !> \sa CB_get_available_space_bytes()
  pure function get_num_spaces(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(IN) :: this     !< circular_buffer
    integer, intent(IN)                :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements             !< Number of empty slots available, -1 if error

    integer            :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size    = get_type_size(type_id)
    num_bytes    = CB_get_available_space_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_spaces

  !> \brief Get current number of data elements from the given type stored in the buffer
  !> <br> num_reals = cb\%get_num_elements(CB_KIND_REAL_8)
  !> \sa CB_get_available_data_bytes()
  pure function get_num_elements(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(IN) :: this     !< circular_buffer instance
    integer, intent(IN)                :: type_id  !< ID of the type of elements we want to fit
    integer(C_INT64_T) :: num_elements             !< Number of (full) data elements stored, -1 if error

    integer :: type_size
    integer(C_INT64_T) :: num_bytes

    type_size = get_type_size(type_id)
    num_bytes = CB_get_available_data_bytes(this % p)
    num_elements = num_bytes / type_size
    if (num_bytes < 0) num_elements = -1
  end function get_num_elements

  !> \brief Get max number of elements this buffer can hold
  !> \sa CB_get_capacity_bytes()
  function get_capacity(this, type_id) result(num_elements)
    implicit none
    class(circular_buffer), intent(INOUT) :: this         !< The circular buffer instance
    integer,                intent(IN)    :: type_id      !< ID of the type of elements we want to fit
    integer(C_INT64_T)                    :: num_elements !< Max number of elements that can fit in the buffer

    num_elements = CB_get_capacity_bytes(this % p) / get_type_size(type_id)
  end function get_capacity

  !> \brief Look at the next elements in this buffer without extracting them
  !> \return .true. if peeking was successful, .false. otherwise
  !> \sa CB_get()
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
  function peek(this, dest, num_elements, type_id, timeout_ms) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this         !< The circular_buffer instance
#include <IgnoreTypeKindRank.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements !< How many elements we want to look at
    integer,                intent(IN), value :: type_id      !< ID of the type of elements we are looking for
    integer, optional,      intent(IN)        :: timeout_ms   !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
    logical :: success

    type(C_PTR)    :: temp
    integer        :: type_size
    integer(C_INT) :: status
    integer(C_INT) :: timeout_c

    success   = .false.
    temp      = C_LOC(dest)
    type_size = get_type_size(type_id)
    timeout_c = -1
    if (present(timeout_ms)) timeout_c = timeout_ms

    status = CB_get(this % p, temp, num_elements * type_size, CB_PEEK, timeout_c)
    if (status == 0) success = .true.
  end function peek

  !> \brief Wait until num_elements (of type type_id) are available then extract them into dest
  !> success = cb\%get(dest, num_elements, type_id, commit_transaction)
  !> \sa CB_get()
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
  function get(this, dest, num_elements, type_id, commit_transaction, timeout_ms) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this               !< circular_buffer instance
#include <IgnoreTypeKindRank.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of elements to extract
    integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
    logical,                intent(IN), value :: commit_transaction !< Whether to update the buffer (ie _extract_ the data)
    integer, optional,      intent(IN)        :: timeout_ms         !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
    logical :: success                                              !< Whether the operation was successful

    integer        :: type_size
    integer(C_INT) :: operation, status, timeout_c
    type(C_PTR)    :: temp

    success   = .false.
    temp      = C_LOC(dest)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT
    timeout_c = -1
    if (present(timeout_ms)) timeout_c = timeout_ms

    status = CB_get(this % p, temp, num_elements * type_size, operation, timeout_c)
    if (status == 0) success = .true.
  end function get

  !> \brief Wait until num_elements of type type_id are available, then insert from src array
  !> success = cb\%put(src, num_elements, type_id, commit_transaction)
  !> \sa CB_put()
#define IgnoreTypeKindRank src
#define ExtraAttributes , target
  function put(this, src, num_elements, type_id, commit_transaction, timeout_ms, thread_safe) result(success)
    implicit none
    class(circular_buffer), intent(INOUT)     :: this               !< circular_buffer instance
#include <IgnoreTypeKindRank.hf>
    integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of tokens to insert from src
    integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
    logical,                intent(IN), value :: commit_transaction !< Whether to make the inserted data immediately available
    integer, optional,      intent(IN)        :: timeout_ms         !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
    logical, optional,      intent(IN)        :: thread_safe        !< [optional] Whether to perform the operation in a thread-safe way (.false. by default)
    logical :: success                                              !< Whether the operation was successful

    integer        :: type_size
    integer(C_INT) :: operation
    integer(C_INT) :: status
    type(C_PTR)    :: temp
    integer(C_INT) :: timeout_c, thread_safe_val

    success   = .false.
    temp      = C_LOC(src)
    type_size = get_type_size(type_id)
    operation = CB_NO_COMMIT
    if (commit_transaction) operation = CB_COMMIT

    timeout_c = -1
    if (present(timeout_ms)) timeout_c = timeout_ms

    thread_safe_val = 0
    if (present(thread_safe)) then
      if (thread_safe) thread_safe_val = 1
    end if

    status = CB_put(this % p, temp, num_elements * type_size, operation, timeout_c, thread_safe_val)
    if (status == 0) success = .true.
  end function put

  !> Delete the buffer by disassociating from the underlying C pointer and, if applicable, releasing the memory
  !> \return .true. if there were no issues, .false. if detaching the shared memory failed
  !> \sa CB_detach_shared
  function delete(this) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: this
    logical :: status

    status = .true.
    if (this % is_owner) then
      call free_c_ptr(this % p)
    else if (this % is_shared) then
      status = CB_detach_shared(this % p) .ge. 0
    end if
    this % p = C_NULL_PTR
  end function delete

  !> Print usage statistics collected during the lifetime of the buffer
  !> \sa CB_print_stats
  subroutine print_stats(this, buffer_id, with_header)
    implicit none
    class(circular_buffer), intent(in) :: this        !< The CB we want to print
    integer(C_INT), intent(IN), value  :: buffer_id   !< ID of the buffer (will be printed at the start of the data line)
    logical,        intent(IN), value  :: with_header !< Whether to print a line describing the data

    integer(C_INT) :: with_header_c
    with_header_c = 0
    if (with_header) with_header_c = 1

    if (this % is_valid()) call CB_print_stats(this % p, buffer_id, with_header_c)
  end subroutine print_stats

  function get_stats(this) result(stats)
    implicit none
    class(circular_buffer), intent(in) :: this !< CB instance
    type(cb_stats), pointer :: stats !< A pointer to the stats struct from this CB instance

    type(C_PTR) :: c_stats
    c_stats = CB_get_stats(this % p)
    call c_f_pointer(c_stats, stats)
  end function get_stats

  function error_code_to_string(error_code) result(error_string)
    implicit none
    integer(C_INT), intent(in) :: error_code !< The code we want to translate into a string
    character(len=:), allocatable :: error_string

    character(len=1), dimension(:), pointer :: tmp_string
    type(C_PTR) :: c_error_string
    
    integer :: i, num_char
    
    c_error_string = CB_error_code_to_string(error_code)
    call c_f_pointer(c_error_string, tmp_string, [8192])

    i = 1
    do 
      if (tmp_string(i) == c_null_char) exit
      i = i + 1
    end do
    num_char = i - 1

    allocate(character(len=num_char) :: error_string)
    error_string = transfer(tmp_string(1:num_char), error_string)
  end function error_code_to_string

  function cb_stats_min(stats_a, stats_b) result(stats_min)
    implicit none
    type(cb_stats), intent(in) :: stats_a, stats_b
    type(cb_stats) :: stats_min
    stats_min % num_reads               = min(stats_a % num_reads,               stats_b % num_reads)
    stats_min % num_unique_reads        = min(stats_a % num_unique_reads,        stats_b % num_unique_reads)
    stats_min % num_read_elems          = min(stats_a % num_read_elems,          stats_b % num_read_elems)
    stats_min % num_fractional_reads    = min(stats_a % num_fractional_reads,    stats_b % num_fractional_reads)
    stats_min % total_read_wait_time_ms = min(stats_a % total_read_wait_time_ms, stats_b % total_read_wait_time_ms)
    stats_min % total_read_time_ms      = min(stats_a % total_read_time_ms,      stats_b % total_read_time_ms)
    stats_min % max_fill                = min(stats_a % max_fill,                stats_b % max_fill)

    stats_min % num_writes               = min(stats_a % num_writes,               stats_b % num_writes)
    stats_min % num_write_elems          = min(stats_a % num_write_elems,          stats_b % num_write_elems)
    stats_min % num_fractional_writes    = min(stats_a % num_fractional_writes,    stats_b % num_fractional_writes)
    stats_min % total_write_wait_time_ms = min(stats_a % total_write_wait_time_ms, stats_b % total_write_wait_time_ms)
    stats_min % total_write_time_ms      = min(stats_a % total_write_time_ms,      stats_b % total_write_time_ms)
  end function cb_stats_min

  function cb_stats_max(stats_a, stats_b) result(stats_max)
    implicit none
    type(cb_stats), intent(in) :: stats_a, stats_b
    type(cb_stats) :: stats_max
    stats_max % num_reads               = max(stats_a % num_reads,               stats_b % num_reads)
    stats_max % num_unique_reads        = max(stats_a % num_unique_reads,        stats_b % num_unique_reads)
    stats_max % num_read_elems          = max(stats_a % num_read_elems,          stats_b % num_read_elems)
    stats_max % num_fractional_reads    = max(stats_a % num_fractional_reads,    stats_b % num_fractional_reads)
    stats_max % total_read_wait_time_ms = max(stats_a % total_read_wait_time_ms, stats_b % total_read_wait_time_ms)
    stats_max % total_read_time_ms      = max(stats_a % total_read_time_ms,      stats_b % total_read_time_ms)
    stats_max % max_fill                = max(stats_a % max_fill,                stats_b % max_fill)

    stats_max % num_writes               = max(stats_a % num_writes,               stats_b % num_writes)
    stats_max % num_write_elems          = max(stats_a % num_write_elems,          stats_b % num_write_elems)
    stats_max % num_fractional_writes    = max(stats_a % num_fractional_writes,    stats_b % num_fractional_writes)
    stats_max % total_write_wait_time_ms = max(stats_a % total_write_wait_time_ms, stats_b % total_write_wait_time_ms)
    stats_max % total_write_time_ms      = max(stats_a % total_write_time_ms,      stats_b % total_write_time_ms)
  end function cb_stats_max

  subroutine assign_cb_stats(this, other)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    type(cb_stats),       intent(in)    :: other

    this % num_reads               = other % num_reads
    this % num_unique_reads        = other % num_unique_reads
    this % num_read_elems          = other % num_read_elems
    this % num_fractional_reads    = other % num_fractional_reads
    this % total_read_wait_time_ms = other % total_read_wait_time_ms
    this % total_read_time_ms      = other % total_read_time_ms
    this % max_fill                = other % max_fill

    this % num_writes               = other % num_writes
    this % num_write_elems          = other % num_write_elems
    this % num_fractional_writes    = other % num_fractional_writes
    this % total_write_wait_time_ms = other % total_write_wait_time_ms
    this % total_write_time_ms      = other % total_write_time_ms
  end subroutine assign_cb_stats

  pure function mean_add_sample_value(val, old_mean, total_num_samples) result(new_mean)
    implicit none
    real(C_DOUBLE), intent(in) :: val, old_mean
    integer,        intent(in) :: total_num_samples
    real(C_DOUBLE) :: new_mean
    new_mean = old_mean + (val - old_mean) / total_num_samples
  end function mean_add_sample_value

  subroutine mean_add_sample(this, sample, total_num_samples)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    type(cb_stats),       intent(in)    :: sample
    integer,              intent(in)    :: total_num_samples

    type(cb_stats_real) :: s
    s = sample

    this % num_reads               = mean_add_sample_value(s % num_reads, this % num_reads, total_num_samples)
    this % num_unique_reads        = mean_add_sample_value(s % num_unique_reads, this % num_unique_reads, total_num_samples)
    this % num_read_elems          = mean_add_sample_value(s % num_read_elems, this % num_read_elems, total_num_samples)
    this % num_fractional_reads    = mean_add_sample_value(s % num_fractional_reads, this % num_fractional_reads, total_num_samples)
    this % total_read_wait_time_ms = mean_add_sample_value(s % total_read_wait_time_ms, this % total_read_wait_time_ms, total_num_samples)
    this % total_read_time_ms      = mean_add_sample_value(s % total_read_time_ms, this % total_read_time_ms, total_num_samples)
    this % max_fill                = mean_add_sample_value(s % max_fill, this % max_fill, total_num_samples)

    this % num_writes               = mean_add_sample_value(s % num_writes, this % num_writes, total_num_samples)
    this % num_write_elems          = mean_add_sample_value(s % num_write_elems, this % num_write_elems, total_num_samples)
    this % num_fractional_writes    = mean_add_sample_value(s % num_fractional_writes, this % num_fractional_writes, total_num_samples)
    this % total_write_wait_time_ms = mean_add_sample_value(s % total_write_wait_time_ms, this % total_write_wait_time_ms, total_num_samples)
    this % total_write_time_ms      = mean_add_sample_value(s % total_write_time_ms, this % total_write_time_ms, total_num_samples)
  end subroutine mean_add_sample

  pure function variance_add_sample_value(val, old_variance, old_mean, new_mean) result(new_variance)
    implicit none
    real(C_DOUBLE), intent(in) :: val, old_variance, old_mean, new_mean
    real(C_DOUBLE) :: new_variance
    new_variance = old_variance + (val - old_mean) * (val - new_mean)
  end function variance_add_sample_value

  subroutine variance_add_sample(this, sample, old_mean, new_mean)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    type(cb_stats),       intent(in)    :: sample
    type(cb_stats_real),  intent(in)    :: old_mean, new_mean

    type(cb_stats_real) :: s
    s = sample

    this % num_reads               = variance_add_sample_value(s % num_reads, this % num_reads, old_mean % num_reads, new_mean % num_reads)
    this % num_unique_reads        = variance_add_sample_value(s % num_unique_reads, this % num_unique_reads, old_mean % num_unique_reads, new_mean % num_unique_reads)
    this % num_read_elems          = variance_add_sample_value(s % num_read_elems, this % num_read_elems, old_mean % num_read_elems, new_mean % num_read_elems)
    this % num_fractional_reads    = variance_add_sample_value(s % num_fractional_reads, this % num_fractional_reads, old_mean % num_fractional_reads, new_mean % num_fractional_reads)
    this % total_read_wait_time_ms = variance_add_sample_value(s % total_read_wait_time_ms, this % total_read_wait_time_ms, old_mean % total_read_wait_time_ms, new_mean % total_read_wait_time_ms)
    this % total_read_time_ms      = variance_add_sample_value(s % total_read_time_ms, this % total_read_time_ms, old_mean % total_read_time_ms, new_mean % total_read_time_ms)
    this % max_fill                = variance_add_sample_value(s % max_fill, this % max_fill, old_mean % max_fill, new_mean % max_fill)

    this % num_writes               = variance_add_sample_value(s % num_writes, this % num_writes, old_mean % num_writes, new_mean % num_writes)
    this % num_write_elems          = variance_add_sample_value(s % num_write_elems, this % num_write_elems, old_mean % num_write_elems, new_mean % num_write_elems)
    this % num_fractional_writes    = variance_add_sample_value(s % num_fractional_writes, this % num_fractional_writes, old_mean % num_fractional_writes, new_mean % num_fractional_writes)
    this % total_write_wait_time_ms = variance_add_sample_value(s % total_write_wait_time_ms, this % total_write_wait_time_ms, old_mean % total_write_wait_time_ms, new_mean % total_write_wait_time_ms)
    this % total_write_time_ms      = variance_add_sample_value(s % total_write_time_ms, this % total_write_time_ms, old_mean % total_write_time_ms, new_mean % total_write_time_ms)
  end subroutine variance_add_sample

  subroutine stats_add(this, other, weight)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    type(cb_stats_real),  intent(in)    :: other
    real, intent(in) :: weight
    this % num_reads               = this % num_reads + other % num_reads * weight
    this % num_unique_reads        = this % num_unique_reads + other % num_unique_reads * weight
    this % num_read_elems          = this % num_read_elems + other % num_read_elems * weight
    this % num_fractional_reads    = this % num_fractional_reads + other % num_fractional_reads * weight
    this % total_read_wait_time_ms = this % total_read_wait_time_ms + other % total_read_wait_time_ms * weight
    this % total_read_time_ms      = this % total_read_time_ms + other % total_read_time_ms * weight
    this % max_fill                = this % max_fill + other % max_fill * weight

    this % num_writes               = this % num_writes + other % num_writes * weight
    this % num_write_elems          = this % num_write_elems + other % num_write_elems * weight
    this % num_fractional_writes    = this % num_fractional_writes + other % num_fractional_writes * weight
    this % total_write_wait_time_ms = this % total_write_wait_time_ms + other % total_write_wait_time_ms * weight
    this % total_write_time_ms      = this % total_write_time_ms + other % total_write_time_ms * weight
  end subroutine stats_add

  pure function add_diff_sq_value(old_val, mean_a, mean_b, weight) result(mean_diff_sq)
    implicit none
    real(kind=8), intent(in) :: old_val, mean_a, mean_b, weight
    real(kind=8) :: mean_diff_sq
    real(kind=8) :: mean_diff

    mean_diff = mean_a - mean_b
    mean_diff_sq = mean_diff * mean_diff * weight + old_val
  end function add_diff_sq_value

  subroutine stats_add_diff_sq(this, mean_a, mean_b, weight)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    type(cb_stats_real),  intent(in)    :: mean_a, mean_b
    real(kind=8),         intent(in)    :: weight

    this % num_reads               = add_diff_sq_value(this % num_reads, mean_a % num_reads, mean_b % num_reads, weight)
    this % num_unique_reads        = add_diff_sq_value(this % num_unique_reads, mean_a % num_unique_reads, mean_b % num_unique_reads, weight)
    this % num_read_elems          = add_diff_sq_value(this % num_read_elems, mean_a % num_read_elems, mean_b % num_read_elems, weight)
    this % num_fractional_reads    = add_diff_sq_value(this % num_fractional_reads, mean_a % num_fractional_reads, mean_b % num_fractional_reads, weight)
    this % total_read_wait_time_ms = add_diff_sq_value(this % total_read_wait_time_ms, mean_a % total_read_wait_time_ms, mean_b % total_read_wait_time_ms, weight)
    this % total_read_time_ms      = add_diff_sq_value(this % total_read_time_ms, mean_a % total_read_time_ms, mean_b % total_read_time_ms, weight)
    this % max_fill                = add_diff_sq_value(this % max_fill, mean_a % max_fill, mean_b % max_fill, weight)

    this % num_writes               = add_diff_sq_value(this % num_writes, mean_a % num_writes, mean_b % num_writes, weight)
    this % num_write_elems          = add_diff_sq_value(this % num_write_elems, mean_a % num_write_elems, mean_b % num_write_elems, weight)
    this % num_fractional_writes    = add_diff_sq_value(this % num_fractional_writes, mean_a % num_fractional_writes, mean_b % num_fractional_writes, weight)
    this % total_write_wait_time_ms = add_diff_sq_value(this % total_write_wait_time_ms, mean_a % total_write_wait_time_ms, mean_b % total_write_wait_time_ms, weight)
    this % total_write_time_ms      = add_diff_sq_value(this % total_write_time_ms, mean_a % total_write_time_ms, mean_b % total_write_time_ms, weight)
  end subroutine stats_add_diff_sq

  subroutine stats_mult_scalar(this, scalar)
    implicit none
    class(cb_stats_real), intent(inout) :: this
    real,                 intent(in)    :: scalar

    this % num_reads               = this % num_reads * scalar
    this % num_unique_reads        = this % num_unique_reads * scalar
    this % num_read_elems          = this % num_read_elems * scalar
    this % num_fractional_reads    = this % num_fractional_reads * scalar
    this % total_read_wait_time_ms = this % total_read_wait_time_ms * scalar
    this % total_read_time_ms      = this % total_read_time_ms * scalar
    this % max_fill                = this % max_fill * scalar

    this % num_writes               = this % num_writes * scalar
    this % num_write_elems          = this % num_write_elems * scalar
    this % num_fractional_writes    = this % num_fractional_writes * scalar
    this % total_write_wait_time_ms = this % total_write_wait_time_ms * scalar
    this % total_write_time_ms      = this % total_write_time_ms * scalar
  end subroutine stats_mult_scalar

  function get_proper_size(size_bytes, multiplier, units) result(proper_size)
    implicit none
    real(kind=8),     intent(in)  :: size_bytes
    real(kind=8),     intent(out) :: multiplier
    character(len=2), intent(out) :: units
    real(kind=8) :: proper_size

    character(len=2), dimension(7), parameter :: unit_chars = (/ 'B ', 'kB', 'MB', 'GB', 'TB', 'PB', '--' /)
    real(kind=8), parameter :: factor = 1024.0

    integer :: num_divs

    proper_size = size_bytes
    num_divs = 1
    multiplier = 1.0
    do while (proper_size > 1900.0 .and. num_divs < 6)
      proper_size = proper_size / factor
      multiplier = multiplier / factor
      num_divs = num_divs + 1
    end do
    units = unit_chars(num_divs)
  end function get_proper_size

  function get_proper_time(time_ms, multiplier, units) result(proper_time)
    implicit none
    real(kind=8),     intent(in)  :: time_ms
    real(kind=8),     intent(out) :: multiplier
    character(len=2), intent(out) :: units
    real(kind=8) :: proper_time

    character(len=2), dimension(7), parameter :: unit_chars = (/ 'ns', 'us', 'ms', 's ', 'm ', 'h ', '--' /)
    integer :: unit_id

    unit_id = 3
    proper_time = time_ms
    multiplier = 1.0

    if (time_ms < 1.0) then
      do while (proper_time < 1.0 .and. unit_id > 1)
        proper_time = proper_time * 1000.0
        multiplier = multiplier * 1000.0
        unit_id = unit_id - 1
      end do
    else
      do while (proper_time >= 1000.0 .and. unit_id < 4)
        proper_time = proper_time / 1000.0
        multiplier  = multiplier / 1000.0
        unit_id = unit_id + 1
      end do

      do while (proper_time > 120.0 .and. unit_id < 7)
        proper_time = proper_time / 60.0
        multiplier  = multiplier / 60.0
        unit_id = unit_id + 1
      end do
    end if

    units = unit_chars(unit_id)

  end function get_proper_time

  subroutine print_cumulated_stats(mean, var, min_v, max_v, print_stats_header)
    implicit none
    type(cb_stats_real), intent(in) :: mean           !< Mean of the stats samples
    type(cb_stats_real), intent(in) :: var            !< Variance of the stats samples
    type(cb_stats),      intent(in) :: min_v, max_v   !< Min/max values of the stats samples
    logical, optional,   intent(in) :: print_stats_header

    real(kind=8) :: write_size, write_size_multiplier, write_size_dev, write_size_min, write_size_max
    real(kind=8) :: write_time, write_time_multiplier, write_time_dev, write_time_min, write_time_max
    real(kind=8) :: write_wait, write_wait_multiplier, write_wait_dev, write_wait_min, write_wait_max
    real(kind=8) :: max_fill, max_fill_multiplier, max_fill_dev, max_fill_min, max_fill_max

    character(len=2) :: write_size_units, write_time_units, write_wait_units, max_fill_units

    integer, parameter :: COL_LENGTH = 30
    character(len=COL_LENGTH), parameter :: DATA_COL        = 'Total data written'
    character(len=COL_LENGTH), parameter :: WRITE_TIME_COL  = 'Total write time'
    character(len=COL_LENGTH), parameter :: WAIT_TIME_COL   = 'Total write wait time'
    character(len=COL_LENGTH), parameter :: FILL_COL        = 'Max fill'

    integer(CB_DATA_ELEMENT) :: dummy_element
    integer :: elem_size
    
    if (present(print_stats_header)) then
      if (print_stats_header) then
        print '(4(A30, A))', DATA_COL, ' : ', WRITE_TIME_COL, ' : ', WAIT_TIME_COL, ' : ', FILL_COL
      end if
    end if
    
    elem_size = storage_size(dummy_element) / 8

    write_size     = get_proper_size(mean % num_write_elems * elem_size, write_size_multiplier, write_size_units)
    write_size_dev = sqrt(var % num_write_elems) * elem_size * write_size_multiplier
    write_size_min = min_v % num_write_elems * elem_size * write_size_multiplier
    write_size_max = max_v % num_write_elems * elem_size * write_size_multiplier

    write_time     = get_proper_time(mean % total_write_time_ms, write_time_multiplier, write_time_units)
    write_time_dev = sqrt(var % total_write_time_ms) * write_time_multiplier
    write_time_min = min_v % total_write_time_ms * write_time_multiplier
    write_time_max = max_v % total_write_time_ms * write_time_multiplier

    write_wait     = get_proper_time(mean % total_write_wait_time_ms, write_wait_multiplier, write_wait_units)
    write_wait_dev = sqrt(var % total_write_wait_time_ms) * write_wait_multiplier
    write_wait_min = min_v % total_write_wait_time_ms * write_wait_multiplier
    write_wait_max = max_v % total_write_wait_time_ms * write_wait_multiplier

    max_fill     = get_proper_size(mean % max_fill * elem_size, max_fill_multiplier, max_fill_units)
    max_fill_dev = sqrt(var % max_fill) * elem_size * max_fill_multiplier
    max_fill_min = min_v % max_fill * elem_size * max_fill_multiplier
    max_fill_max = max_v % max_fill * elem_size * max_fill_multiplier

    print '(4(F6.1, A, A, F4.1, A, F6.1, A, F6.1, A))',                                                                       &
          write_size, write_size_units, ' +-', write_size_dev, '(', write_size_min, '-', write_size_max, ') : ',              &
          write_time, write_time_units, ' +-', write_time_dev, '(', write_time_min, '-', write_time_max, ') : ',              &
          write_wait, write_wait_units, ' +-', write_wait_dev, '(', write_wait_min, '-', write_wait_max, ') : ',              &
          max_fill,   max_fill_units,   ' +-', max_fill_dev,   '(', max_fill_min,   '-', max_fill_max,   ')' 

  end subroutine print_cumulated_stats

  function maxed_cb_stats() result(maxed)
    implicit none
    type(cb_stats) :: maxed
    maxed % num_reads               = huge(maxed % num_reads)
    maxed % num_unique_reads        = huge(maxed % num_unique_reads)
    maxed % num_read_elems          = huge(maxed % num_read_elems)
    maxed % num_fractional_reads    = huge(maxed % num_fractional_reads)
    maxed % total_read_wait_time_ms = huge(maxed % total_read_wait_time_ms)
    maxed % total_read_time_ms      = huge(maxed % total_read_time_ms)
    maxed % max_fill                = huge(maxed % max_fill)

    maxed % num_writes               = huge(maxed % num_writes)
    maxed % num_write_elems          = huge(maxed % num_write_elems)
    maxed % num_fractional_writes    = huge(maxed % num_fractional_writes)
    maxed % total_write_wait_time_ms = huge(maxed % total_write_wait_time_ms)
    maxed % total_write_time_ms      = huge(maxed % total_write_time_ms)
  end function maxed_cb_stats

end module circular_buffer_module
