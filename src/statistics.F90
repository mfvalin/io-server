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
!> \author V. Magnoux, Recherche en Prevision Numerique
!> \date 2020-2022

!> \file
!> Helper functions to compile usage statistics on IO-server structures

module statistics_module
  use iso_c_binding

  use circular_buffer_module
  use shmem_heap_module
  use rpn_extra_module
  implicit none
  private

  public :: ioserver_stats_size_byte, ioserver_stats_size_int8
  public :: ioserver_stats_min, ioserver_stats_max, maxed_ioserver_stats, print_cumulated_stats

  type, public, bind(C) :: ioserver_stats_c
    ! Circular buffer stats
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

    ! Heap stats
    real(C_DOUBLE) :: heap_size_byte      = 0.0   !< Total size of heap (bytes)
    real(C_DOUBLE) :: max_heap_fill_byte  = 0.0   !< High water mark in heap  (highest allocation point) (bytes)
    real(C_DOUBLE) :: total_alloc_block   = 0.0   !< Total number of blocks that have been allocated over the life of the heap
    real(C_DOUBLE) :: total_alloc_byte    = 0.0   !< Total number of bytes used by allocated blocks over the life of the heap
  end type ioserver_stats_c

  type, public :: ioserver_stats
    type(ioserver_stats_c) :: c
    logical :: is_valid = .true.
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

    procedure :: set_cb_stats
    procedure :: set_heap_stats
  end type ioserver_stats

contains

  function ioserver_stats_size_byte()
    implicit none
    integer(C_INT64_T) :: ioserver_stats_size_byte !< Size of the message_header type in bytes
    type(ioserver_stats_c) :: dummy_stats
    ioserver_stats_size_byte = storage_size(dummy_stats) / 8
  end function ioserver_stats_size_byte

  function ioserver_stats_size_int8()
    implicit none
    integer(C_INT64_T) :: ioserver_stats_size_int8 !< How many 64-bit integers are needed to contain a message_header
    ioserver_stats_size_int8 = num_char_to_num_int8(ioserver_stats_size_byte())
  end function ioserver_stats_size_int8

  function ioserver_stats_min(stats_a, stats_b) result(stats_min)
    implicit none
    type(ioserver_stats), intent(in) :: stats_a, stats_b
    type(ioserver_stats) :: stats_min
    stats_min%c % num_reads               = min(stats_a%c % num_reads,               stats_b%c % num_reads)
    stats_min%c % num_unique_reads        = min(stats_a%c % num_unique_reads,        stats_b%c % num_unique_reads)
    stats_min%c % num_read_elems          = min(stats_a%c % num_read_elems,          stats_b%c % num_read_elems)
    stats_min%c % num_fractional_reads    = min(stats_a%c % num_fractional_reads,    stats_b%c % num_fractional_reads)
    stats_min%c % total_read_wait_time_ms = min(stats_a%c % total_read_wait_time_ms, stats_b%c % total_read_wait_time_ms)
    stats_min%c % total_read_time_ms      = min(stats_a%c % total_read_time_ms,      stats_b%c % total_read_time_ms)
    stats_min%c % max_fill                = min(stats_a%c % max_fill,                stats_b%c % max_fill)

    stats_min%c % num_writes               = min(stats_a%c % num_writes,               stats_b%c % num_writes)
    stats_min%c % num_write_elems          = min(stats_a%c % num_write_elems,          stats_b%c % num_write_elems)
    stats_min%c % num_fractional_writes    = min(stats_a%c % num_fractional_writes,    stats_b%c % num_fractional_writes)
    stats_min%c % total_write_wait_time_ms = min(stats_a%c % total_write_wait_time_ms, stats_b%c % total_write_wait_time_ms)
    stats_min%c % total_write_time_ms      = min(stats_a%c % total_write_time_ms,      stats_b%c % total_write_time_ms)

    stats_min%c % heap_size_byte     = min(stats_a%c % heap_size_byte,     stats_b%c % heap_size_byte)
    stats_min%c % max_heap_fill_byte = min(stats_a%c % max_heap_fill_byte, stats_b%c % max_heap_fill_byte)
    stats_min%c % total_alloc_block  = min(stats_a%c % total_alloc_block,  stats_b%c % total_alloc_block)
    stats_min%c % total_alloc_byte   = min(stats_a%c % total_alloc_byte,   stats_b%c % total_alloc_byte)

    stats_min % is_valid = .true.
  end function ioserver_stats_min

  function ioserver_stats_max(stats_a, stats_b) result(stats_max)
    implicit none
    type(ioserver_stats), intent(in) :: stats_a, stats_b
    type(ioserver_stats) :: stats_max
    stats_max%c % num_reads               = max(stats_a%c % num_reads,               stats_b%c % num_reads)
    stats_max%c % num_unique_reads        = max(stats_a%c % num_unique_reads,        stats_b%c % num_unique_reads)
    stats_max%c % num_read_elems          = max(stats_a%c % num_read_elems,          stats_b%c % num_read_elems)
    stats_max%c % num_fractional_reads    = max(stats_a%c % num_fractional_reads,    stats_b%c % num_fractional_reads)
    stats_max%c % total_read_wait_time_ms = max(stats_a%c % total_read_wait_time_ms, stats_b%c % total_read_wait_time_ms)
    stats_max%c % total_read_time_ms      = max(stats_a%c % total_read_time_ms,      stats_b%c % total_read_time_ms)
    stats_max%c % max_fill                = max(stats_a%c % max_fill,                stats_b%c % max_fill)

    stats_max%c % num_writes               = max(stats_a%c % num_writes,               stats_b%c % num_writes)
    stats_max%c % num_write_elems          = max(stats_a%c % num_write_elems,          stats_b%c % num_write_elems)
    stats_max%c % num_fractional_writes    = max(stats_a%c % num_fractional_writes,    stats_b%c % num_fractional_writes)
    stats_max%c % total_write_wait_time_ms = max(stats_a%c % total_write_wait_time_ms, stats_b%c % total_write_wait_time_ms)
    stats_max%c % total_write_time_ms      = max(stats_a%c % total_write_time_ms,      stats_b%c % total_write_time_ms)

    stats_max%c % heap_size_byte     = max(stats_a%c % heap_size_byte,     stats_b%c % heap_size_byte)
    stats_max%c % max_heap_fill_byte = max(stats_a%c % max_heap_fill_byte, stats_b%c % max_heap_fill_byte)
    stats_max%c % total_alloc_block  = max(stats_a%c % total_alloc_block,  stats_b%c % total_alloc_block)
    stats_max%c % total_alloc_byte   = max(stats_a%c % total_alloc_byte,   stats_b%c % total_alloc_byte)
  end function ioserver_stats_max

  subroutine set_cb_stats(this, other_stats)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    type(cb_stats),        intent(in)    :: other_stats

    this%c % num_reads               = other_stats % num_reads
    this%c % num_unique_reads        = other_stats % num_unique_reads
    this%c % num_read_elems          = other_stats % num_read_elems
    this%c % num_fractional_reads    = other_stats % num_fractional_reads
    this%c % total_read_wait_time_ms = other_stats % total_read_wait_time_ms
    this%c % total_read_time_ms      = other_stats % total_read_time_ms
    this%c % max_fill                = other_stats % max_fill

    this%c % num_writes               = other_stats % num_writes
    this%c % num_write_elems          = other_stats % num_write_elems
    this%c % num_fractional_writes    = other_stats % num_fractional_writes
    this%c % total_write_wait_time_ms = other_stats % total_write_wait_time_ms
    this%c % total_write_time_ms      = other_stats % total_write_time_ms
  end subroutine set_cb_stats

  subroutine set_heap_stats(this, other_stats)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    type(heap_stats),      intent(in)    :: other_stats

    this%c % heap_size_byte     = other_stats % size_byte
    this%c % max_heap_fill_byte = other_stats % max_fill_byte
    this%c % total_alloc_block  = other_stats % total_alloc_block
    this%c % total_alloc_byte   = other_stats % total_alloc_byte
  end subroutine

  pure function mean_add_sample_value(val, old_mean, total_num_samples) result(new_mean)
    implicit none
    real(C_DOUBLE), intent(in) :: val, old_mean
    integer,        intent(in) :: total_num_samples
    real(C_DOUBLE) :: new_mean
    new_mean = old_mean + (val - old_mean) / total_num_samples
  end function mean_add_sample_value

  subroutine mean_add_sample(this, sample, total_num_samples)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    type(ioserver_stats),  intent(in)    :: sample
    integer,               intent(in)    :: total_num_samples

    type(ioserver_stats) :: s
    s = sample

    this%c % num_reads               = mean_add_sample_value(s%c % num_reads, this%c % num_reads, total_num_samples)
    this%c % num_unique_reads        = mean_add_sample_value(s%c % num_unique_reads, this%c % num_unique_reads, total_num_samples)
    this%c % num_read_elems          = mean_add_sample_value(s%c % num_read_elems, this%c % num_read_elems, total_num_samples)
    this%c % num_fractional_reads    = mean_add_sample_value(s%c % num_fractional_reads, this%c % num_fractional_reads, total_num_samples)
    this%c % total_read_wait_time_ms = mean_add_sample_value(s%c % total_read_wait_time_ms, this%c % total_read_wait_time_ms, total_num_samples)
    this%c % total_read_time_ms      = mean_add_sample_value(s%c % total_read_time_ms, this%c % total_read_time_ms, total_num_samples)
    this%c % max_fill                = mean_add_sample_value(s%c % max_fill, this%c % max_fill, total_num_samples)

    this%c % num_writes               = mean_add_sample_value(s%c % num_writes, this%c % num_writes, total_num_samples)
    this%c % num_write_elems          = mean_add_sample_value(s%c % num_write_elems, this%c % num_write_elems, total_num_samples)
    this%c % num_fractional_writes    = mean_add_sample_value(s%c % num_fractional_writes, this%c % num_fractional_writes, total_num_samples)
    this%c % total_write_wait_time_ms = mean_add_sample_value(s%c % total_write_wait_time_ms, this%c % total_write_wait_time_ms, total_num_samples)
    this%c % total_write_time_ms      = mean_add_sample_value(s%c % total_write_time_ms, this%c % total_write_time_ms, total_num_samples)

    this%c % heap_size_byte     = mean_add_sample_value(s%c % heap_size_byte, this%c % heap_size_byte, total_num_samples)
    this%c % max_heap_fill_byte = mean_add_sample_value(s%c % max_heap_fill_byte, this%c % max_heap_fill_byte, total_num_samples)
    this%c % total_alloc_block  = mean_add_sample_value(s%c % total_alloc_block, this%c % total_alloc_block, total_num_samples)
    this%c % total_alloc_byte   = mean_add_sample_value(s%c % total_alloc_byte, this%c % total_alloc_byte, total_num_samples)
  end subroutine mean_add_sample

  pure function variance_add_sample_value(val, old_variance, old_mean, new_mean) result(new_variance)
    implicit none
    real(C_DOUBLE), intent(in) :: val, old_variance, old_mean, new_mean
    real(C_DOUBLE) :: new_variance
    new_variance = old_variance + (val - old_mean) * (val - new_mean)
  end function variance_add_sample_value

  subroutine variance_add_sample(this, sample, old_mean, new_mean)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    type(ioserver_stats),  intent(in)    :: sample
    type(ioserver_stats),  intent(in)    :: old_mean, new_mean

    type(ioserver_stats) :: s
    s = sample

    this%c % num_reads               = variance_add_sample_value(s%c % num_reads, this%c % num_reads, old_mean%c % num_reads, new_mean%c % num_reads)
    this%c % num_unique_reads        = variance_add_sample_value(s%c % num_unique_reads, this%c % num_unique_reads, old_mean%c % num_unique_reads, new_mean%c % num_unique_reads)
    this%c % num_read_elems          = variance_add_sample_value(s%c % num_read_elems, this%c % num_read_elems, old_mean%c % num_read_elems, new_mean%c % num_read_elems)
    this%c % num_fractional_reads    = variance_add_sample_value(s%c % num_fractional_reads, this%c % num_fractional_reads, old_mean%c % num_fractional_reads, new_mean%c % num_fractional_reads)
    this%c % total_read_wait_time_ms = variance_add_sample_value(s%c % total_read_wait_time_ms, this%c % total_read_wait_time_ms, old_mean%c % total_read_wait_time_ms, new_mean%c % total_read_wait_time_ms)
    this%c % total_read_time_ms      = variance_add_sample_value(s%c % total_read_time_ms, this%c % total_read_time_ms, old_mean%c % total_read_time_ms, new_mean%c % total_read_time_ms)
    this%c % max_fill                = variance_add_sample_value(s%c % max_fill, this%c % max_fill, old_mean%c % max_fill, new_mean%c % max_fill)

    this%c % num_writes               = variance_add_sample_value(s%c % num_writes, this%c % num_writes, old_mean%c % num_writes, new_mean%c % num_writes)
    this%c % num_write_elems          = variance_add_sample_value(s%c % num_write_elems, this%c % num_write_elems, old_mean%c % num_write_elems, new_mean%c % num_write_elems)
    this%c % num_fractional_writes    = variance_add_sample_value(s%c % num_fractional_writes, this%c % num_fractional_writes, old_mean%c % num_fractional_writes, new_mean%c % num_fractional_writes)
    this%c % total_write_wait_time_ms = variance_add_sample_value(s%c % total_write_wait_time_ms, this%c % total_write_wait_time_ms, old_mean%c % total_write_wait_time_ms, new_mean%c % total_write_wait_time_ms)
    this%c % total_write_time_ms      = variance_add_sample_value(s%c % total_write_time_ms, this%c % total_write_time_ms, old_mean%c % total_write_time_ms, new_mean%c % total_write_time_ms)

    this%c % heap_size_byte     = variance_add_sample_value(s%c % heap_size_byte, this%c % heap_size_byte, old_mean%c % heap_size_byte, new_mean%c % heap_size_byte)
    this%c % max_heap_fill_byte = variance_add_sample_value(s%c % max_heap_fill_byte, this%c % max_heap_fill_byte, old_mean%c % max_heap_fill_byte, new_mean%c % max_heap_fill_byte)
    this%c % total_alloc_block  = variance_add_sample_value(s%c % total_alloc_block, this%c % total_alloc_block, old_mean%c % total_alloc_block, new_mean%c % total_alloc_block)
    this%c % total_alloc_byte   = variance_add_sample_value(s%c % total_alloc_byte, this%c % total_alloc_byte, old_mean%c % total_alloc_byte, new_mean%c % total_alloc_byte)
  end subroutine variance_add_sample

  subroutine stats_add(this, other, weight)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    type(ioserver_stats),  intent(in)    :: other
    real, intent(in) :: weight
    this%c % num_reads               = this%c % num_reads + other%c % num_reads * weight
    this%c % num_unique_reads        = this%c % num_unique_reads + other%c % num_unique_reads * weight
    this%c % num_read_elems          = this%c % num_read_elems + other%c % num_read_elems * weight
    this%c % num_fractional_reads    = this%c % num_fractional_reads + other%c % num_fractional_reads * weight
    this%c % total_read_wait_time_ms = this%c % total_read_wait_time_ms + other%c % total_read_wait_time_ms * weight
    this%c % total_read_time_ms      = this%c % total_read_time_ms + other%c % total_read_time_ms * weight
    this%c % max_fill                = this%c % max_fill + other%c % max_fill * weight

    this%c % num_writes               = this%c % num_writes + other%c % num_writes * weight
    this%c % num_write_elems          = this%c % num_write_elems + other%c % num_write_elems * weight
    this%c % num_fractional_writes    = this%c % num_fractional_writes + other%c % num_fractional_writes * weight
    this%c % total_write_wait_time_ms = this%c % total_write_wait_time_ms + other%c % total_write_wait_time_ms * weight
    this%c % total_write_time_ms      = this%c % total_write_time_ms + other%c % total_write_time_ms * weight

    this%c % heap_size_byte     = this%c % heap_size_byte + other%c % heap_size_byte * weight
    this%c % max_heap_fill_byte = this%c % max_heap_fill_byte + other%c % max_heap_fill_byte * weight
    this%c % total_alloc_block  = this%c % total_alloc_block + other%c % total_alloc_block * weight
    this%c % total_alloc_byte   = this%c % total_alloc_byte + other%c % total_alloc_byte * weight
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
    class(ioserver_stats), intent(inout) :: this
    type(ioserver_stats),  intent(in)    :: mean_a, mean_b
    real(kind=8),          intent(in)    :: weight

    this%c % num_reads               = add_diff_sq_value(this%c % num_reads, mean_a%c % num_reads, mean_b%c % num_reads, weight)
    this%c % num_unique_reads        = add_diff_sq_value(this%c % num_unique_reads, mean_a%c % num_unique_reads, mean_b%c % num_unique_reads, weight)
    this%c % num_read_elems          = add_diff_sq_value(this%c % num_read_elems, mean_a%c % num_read_elems, mean_b%c % num_read_elems, weight)
    this%c % num_fractional_reads    = add_diff_sq_value(this%c % num_fractional_reads, mean_a%c % num_fractional_reads, mean_b%c % num_fractional_reads, weight)
    this%c % total_read_wait_time_ms = add_diff_sq_value(this%c % total_read_wait_time_ms, mean_a%c % total_read_wait_time_ms, mean_b%c % total_read_wait_time_ms, weight)
    this%c % total_read_time_ms      = add_diff_sq_value(this%c % total_read_time_ms, mean_a%c % total_read_time_ms, mean_b%c % total_read_time_ms, weight)
    this%c % max_fill                = add_diff_sq_value(this%c % max_fill, mean_a%c % max_fill, mean_b%c % max_fill, weight)

    this%c % num_writes               = add_diff_sq_value(this%c % num_writes, mean_a%c % num_writes, mean_b%c % num_writes, weight)
    this%c % num_write_elems          = add_diff_sq_value(this%c % num_write_elems, mean_a%c % num_write_elems, mean_b%c % num_write_elems, weight)
    this%c % num_fractional_writes    = add_diff_sq_value(this%c % num_fractional_writes, mean_a%c % num_fractional_writes, mean_b%c % num_fractional_writes, weight)
    this%c % total_write_wait_time_ms = add_diff_sq_value(this%c % total_write_wait_time_ms, mean_a%c % total_write_wait_time_ms, mean_b%c % total_write_wait_time_ms, weight)
    this%c % total_write_time_ms      = add_diff_sq_value(this%c % total_write_time_ms, mean_a%c % total_write_time_ms, mean_b%c % total_write_time_ms, weight)

    this%c % heap_size_byte     = add_diff_sq_value(this%c % heap_size_byte, mean_a%c % heap_size_byte, mean_b%c % heap_size_byte, weight)
    this%c % max_heap_fill_byte = add_diff_sq_value(this%c % max_heap_fill_byte, mean_a%c % max_heap_fill_byte, mean_b%c % max_heap_fill_byte, weight)
    this%c % total_alloc_block  = add_diff_sq_value(this%c % total_alloc_block, mean_a%c % total_alloc_block, mean_b%c % total_alloc_block, weight)
    this%c % total_alloc_byte   = add_diff_sq_value(this%c % total_alloc_byte, mean_a%c % total_alloc_byte, mean_b%c % total_alloc_byte, weight)
  end subroutine stats_add_diff_sq

  subroutine stats_mult_scalar(this, scalar)
    implicit none
    class(ioserver_stats), intent(inout) :: this
    real,                  intent(in)    :: scalar

    this%c % num_reads               = this%c % num_reads * scalar
    this%c % num_unique_reads        = this%c % num_unique_reads * scalar
    this%c % num_read_elems          = this%c % num_read_elems * scalar
    this%c % num_fractional_reads    = this%c % num_fractional_reads * scalar
    this%c % total_read_wait_time_ms = this%c % total_read_wait_time_ms * scalar
    this%c % total_read_time_ms      = this%c % total_read_time_ms * scalar
    this%c % max_fill                = this%c % max_fill * scalar

    this%c % num_writes               = this%c % num_writes * scalar
    this%c % num_write_elems          = this%c % num_write_elems * scalar
    this%c % num_fractional_writes    = this%c % num_fractional_writes * scalar
    this%c % total_write_wait_time_ms = this%c % total_write_wait_time_ms * scalar
    this%c % total_write_time_ms      = this%c % total_write_time_ms * scalar
    
    this%c % heap_size_byte     = this%c % heap_size_byte * scalar
    this%c % max_heap_fill_byte = this%c % max_heap_fill_byte * scalar
    this%c % total_alloc_block  = this%c % total_alloc_block * scalar
    this%c % total_alloc_byte   = this%c % total_alloc_byte * scalar
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
    type(ioserver_stats), intent(in) :: mean           !< Mean of the stats samples
    type(ioserver_stats), intent(in) :: var            !< Variance of the stats samples
    type(ioserver_stats), intent(in) :: min_v, max_v   !< Min/max values of the stats samples
    logical, optional,    intent(in) :: print_stats_header !< [optional] Whether to print a header describing the values

    real(kind=8) :: write_size, write_size_multiplier, write_size_dev, write_size_min, write_size_max
    real(kind=8) :: write_time, write_time_multiplier, write_time_dev, write_time_min, write_time_max
    real(kind=8) :: write_wait, write_wait_multiplier, write_wait_dev, write_wait_min, write_wait_max
    real(kind=8) :: max_fill, max_fill_multiplier, max_fill_dev, max_fill_min, max_fill_max
    real(kind=8) :: max_fill_h, max_fill_multiplier_h, max_fill_dev_h, max_fill_min_h, max_fill_max_h

    character(len=2) :: write_size_units, write_time_units, write_wait_units, max_fill_units, max_fill_units_h

    integer, parameter :: COL_LENGTH = 30
    character(len=COL_LENGTH), parameter :: DATA_COL        = 'Total data written'
    character(len=COL_LENGTH), parameter :: WRITE_TIME_COL  = 'Total write time'
    character(len=COL_LENGTH), parameter :: WAIT_TIME_COL   = 'Total write wait time'
    character(len=COL_LENGTH), parameter :: FILL_COL        = 'Max CB fill'
    character(len=COL_LENGTH), parameter :: HEAP_FILL_COL   = 'Max heap fill'

    integer(CB_DATA_ELEMENT) :: dummy_element
    integer :: elem_size
    
    if (.not. min_v % is_valid) then
      print '(A)', 'WARNING: Min stats is not valid. Cannot print it!'
      return
    end if
    
    if (present(print_stats_header)) then
      if (print_stats_header) then
        print '(5(A30, A))', DATA_COL, ' : ', WRITE_TIME_COL, ' : ', WAIT_TIME_COL, ' : ', FILL_COL, ' : ', HEAP_FILL_COL
      end if
    end if
    
    elem_size = storage_size(dummy_element) / 8

    write_size     = get_proper_size(mean%c % num_write_elems * elem_size, write_size_multiplier, write_size_units)
    write_size_dev = sqrt(var%c % num_write_elems) * elem_size * write_size_multiplier
    write_size_min = min_v%c % num_write_elems * elem_size * write_size_multiplier
    write_size_max = max_v%c % num_write_elems * elem_size * write_size_multiplier

    write_time     = get_proper_time(mean%c % total_write_time_ms, write_time_multiplier, write_time_units)
    write_time_dev = sqrt(var%c % total_write_time_ms) * write_time_multiplier
    write_time_min = min_v%c % total_write_time_ms * write_time_multiplier
    write_time_max = max_v%c % total_write_time_ms * write_time_multiplier

    write_wait     = get_proper_time(mean%c % total_write_wait_time_ms, write_wait_multiplier, write_wait_units)
    write_wait_dev = sqrt(var%c % total_write_wait_time_ms) * write_wait_multiplier
    write_wait_min = min_v%c % total_write_wait_time_ms * write_wait_multiplier
    write_wait_max = max_v%c % total_write_wait_time_ms * write_wait_multiplier

    max_fill     = get_proper_size(mean%c % max_fill * elem_size, max_fill_multiplier, max_fill_units)
    max_fill_dev = sqrt(var%c % max_fill) * elem_size * max_fill_multiplier
    max_fill_min = min_v%c % max_fill * elem_size * max_fill_multiplier
    max_fill_max = max_v%c % max_fill * elem_size * max_fill_multiplier

    max_fill_h     = get_proper_size(mean%c % max_heap_fill_byte, max_fill_multiplier_h, max_fill_units_h)
    max_fill_dev_h = sqrt(var%c % max_heap_fill_byte) * max_fill_multiplier_h
    max_fill_min_h = min_v%c % max_heap_fill_byte * max_fill_multiplier_h
    max_fill_max_h = max_v%c % max_heap_fill_byte * max_fill_multiplier_h

    print '(5(F6.1, A, A, F4.1, A, F6.1, A, F6.1, A))',                                                                       &
          write_size, write_size_units, ' +-', write_size_dev, '(', write_size_min, '-', write_size_max, ') : ',              &
          write_time, write_time_units, ' +-', write_time_dev, '(', write_time_min, '-', write_time_max, ') : ',              &
          write_wait, write_wait_units, ' +-', write_wait_dev, '(', write_wait_min, '-', write_wait_max, ') : ',              &
          max_fill,   max_fill_units,   ' +-', max_fill_dev,   '(', max_fill_min,   '-', max_fill_max,   ') : ',              &
          max_fill_h, max_fill_units_h, ' +-', max_fill_dev_h, '(', max_fill_min_h, '-', max_fill_max_h, ')' 

  end subroutine print_cumulated_stats

  function maxed_ioserver_stats() result(maxed)
    implicit none
    type(ioserver_stats) :: maxed
    maxed % is_valid = .false.
    maxed%c % num_reads               = huge(maxed%c % num_reads)
    maxed%c % num_unique_reads        = huge(maxed%c % num_unique_reads)
    maxed%c % num_read_elems          = huge(maxed%c % num_read_elems)
    maxed%c % num_fractional_reads    = huge(maxed%c % num_fractional_reads)
    maxed%c % total_read_wait_time_ms = huge(maxed%c % total_read_wait_time_ms)
    maxed%c % total_read_time_ms      = huge(maxed%c % total_read_time_ms)
    maxed%c % max_fill                = huge(maxed%c % max_fill)

    maxed%c % num_writes               = huge(maxed%c % num_writes)
    maxed%c % num_write_elems          = huge(maxed%c % num_write_elems)
    maxed%c % num_fractional_writes    = huge(maxed%c % num_fractional_writes)
    maxed%c % total_write_wait_time_ms = huge(maxed%c % total_write_wait_time_ms)
    maxed%c % total_write_time_ms      = huge(maxed%c % total_write_time_ms)

    maxed%c % heap_size_byte      = huge(maxed%c % heap_size_byte)
    maxed%c % max_heap_fill_byte  = huge(maxed%c % max_heap_fill_byte)
    maxed%c % total_alloc_block   = huge(maxed%c % total_alloc_block)
    maxed%c % total_alloc_byte    = huge(maxed%c % total_alloc_byte)
  end function maxed_ioserver_stats

end module statistics_module
