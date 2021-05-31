/*
 * Copyright (C) 2021  Environnement et Changement climatique Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Authors:
 *     M. Valin,   Recherche en Prevision Numerique, 2020/2021
 *     V. Magnoux, Recherche en Prevision Numerique, 2020/2021
 */

//C_StArT
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

//! Timer that can accumulate microsecond intervals
typedef struct {
  uint64_t start;      //! Timestamp when the timer was started
  uint64_t total_time; //! How many clock ticks have been recorded (updates every time the timer stops)
} io_timer_t;

//! Get current system time in microseconds, wraps around approximately every year
static inline uint64_t get_current_time_us() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);

  // Wraps around every year or so. Not sure why you would need microsecond precision for longer
  const uint64_t now_us = ((uint64_t)now.tv_sec % (1 << 25)) * 1000000 + (uint64_t)now.tv_nsec / 1000;

  return now_us;
}
//C_EnD


//F_StArT
//  interface
//F_EnD

static inline void IO_timer_init(io_timer_t* timer) {
  timer->start = 0;
  timer->total_time = 0;
}

static inline io_timer_t* IO_timer_create() {
  io_timer_t* timer = (io_timer_t*)malloc(sizeof(io_timer_t));
  if (timer != NULL)
    IO_timer_init(timer);
  return timer;
}

//F_StArT
//  function IO_timer_create() result(timer) BIND(C, name = 'IO_timer_create_f')
//    import C_PTR
//    implicit none
//    type(C_PTR) :: timer
//  end function IO_timer_create
//F_EnD
io_timer_t* IO_timer_create_f() { return IO_timer_create(); }

static inline void IO_timer_delete(io_timer_t* timer) {
  if(timer != NULL)
    free(timer);
}

//F_StArT
//  subroutine IO_timer_delete(timer) BIND(C, name = 'IO_timer_delete_f')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_delete
//F_EnD
void IO_timer_delete_f(io_timer_t* timer) { IO_timer_delete(timer); }

//C_StArT
//! Record the current timestamp
static inline void IO_timer_start(io_timer_t* timer) {
  timer->start = get_current_time_us();
}
//C_EnD

//F_StArT
//  subroutine IO_timer_start(timer) BIND(C, name = 'IO_timer_start_f')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_start
//F_EnD
void IO_timer_start_f(io_timer_t* timer) { IO_timer_start(timer); }

//C_StArT
//! Increment total time with number of ticks since last start
static void IO_timer_stop(io_timer_t* timer) {
  timer->total_time += get_current_time_us() - timer->start;
}
//C_EnD

//F_StArT
//  subroutine IO_timer_stop(timer) BIND(C, name = 'IO_timer_stop_f')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_stop
//F_EnD
void IO_timer_stop_f(io_timer_t* timer) { IO_timer_stop(timer); }


//C_StArT
//! Retrieve the accumulated time in number of milliseconds, as a double
static inline double IO_time_ms(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return timer->total_time / 1000.0;
}
//C_EnD

//F_StArT
//  function IO_time_ms(timer) result(time) BIND(C, name = 'IO_time_ms_f')
//    import C_PTR, C_DOUBLE
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//    real(C_DOUBLE) :: time
//  end function IO_time_ms
//F_EnD
double IO_time_ms_f(const io_timer_t* timer) { IO_time_ms(timer); }

//C_StArT
static inline double IO_time_since_start(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return (get_current_time_us() - timer->start) / 1000.0;
}
//C_EnD

//! Read the processor time stamp counter. "in order" version, with "serialization"
static inline uint64_t rdtscp() {
#if defined(__x86_64__) || defined( __i386__ )
  uint32_t lo, hi;
  __asm__ volatile ("rdtscp"
      : /* outputs */ "=a" (lo), "=d" (hi)
      : /* no inputs */
      : /* clobbers */ "%rcx");
  __asm__ volatile ("mfence");
  return (uint64_t)lo | (((uint64_t)hi) << 32);
#else
  return time0++;
#endif
}

//F_StArT
//  function rdtscp() result(count) BIND(C, name = 'rdtscp_f')
//    import C_INT64_T
//    implicit none
//    integer(C_INT64_T) :: count
//  end function rdtscp
//F_EnD
uint64_t rdtscp_f() { return rdtscp(); }

//! Read the processor time stamp counter. Fast version, "out of order"
static inline uint64_t rdtsc() {
#if defined(__x86_64__) || defined( __i386__ )
  uint32_t lo, hi;
  __asm__ volatile ("rdtsc"
      : /* outputs */ "=a" (lo), "=d" (hi)
      : /* no inputs */
      : /* clobbers */ "%rcx");
  return (uint64_t)lo | (((uint64_t)hi) << 32);
#else
  return time0++;
#endif
}

//F_StArT
//  function rdtsc() result(count) BIND(C, name = 'rdtsc_f')
//    import C_INT64_T
//    implicit none
//    integer(C_INT64_T) :: count
//  end function rdtsc
//F_EnD
uint64_t rdtsc_f() { return rdtsc(); }

//F_StArT
//  end interface
//F_EnD
