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

//F_StArT
//  function IO_timer_create() result(timer) BIND(C, name = 'IO_timer_create')
//    import C_PTR
//    implicit none
//    type(C_PTR) :: timer
//  end function IO_timer_create
//F_EnD
io_timer_t* IO_timer_create() {
  io_timer_t* timer = (io_timer_t*)malloc(sizeof(io_timer_t));
  if (timer != NULL)
    IO_timer_init(timer);
  return timer;
}

//F_StArT
//  subroutine IO_timer_delete(timer) BIND(C, name = 'IO_timer_delete')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_delete
//F_EnD
void IO_timer_delete(io_timer_t* timer) {
  if(timer != NULL)
    free(timer);
}

//F_StArT
//  subroutine IO_timer_start(timer) BIND(C, name = 'IO_timer_start')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_start
//F_EnD
//C_StArT
//! Record the current timestamp
void IO_timer_start(io_timer_t* timer){
//C_EnD
  timer->start = get_current_time_us();
}

//F_StArT
//  subroutine IO_timer_stop(timer) BIND(C, name = 'IO_timer_stop')
//    import C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//  end subroutine IO_timer_stop
//F_EnD
//C_StArT
//! Increment total time with number of ticks since last start
void IO_timer_stop(io_timer_t* timer){
//C_EnD
  timer->total_time += get_current_time_us() - timer->start;
}

//F_StArT
//  function IO_time_ms(timer) result(time) BIND(C, name = 'IO_time_ms')
//    import C_PTR, C_DOUBLE
//    implicit none
//    type(C_PTR), intent(IN), value :: timer
//    real(C_DOUBLE) :: time
//  end function IO_time_ms
//F_EnD
//C_StArT
//! Retrieve the accumulated time in number of milliseconds, as a double
double IO_time_ms(const io_timer_t* timer){
//C_EnD
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return timer->total_time / 1000.0;
}

//C_StArT
static inline double IO_time_since_start(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return (get_current_time_us() - timer->start) / 1000.0;
}
//C_EnD

//F_StArT
//  end interface
//F_EnD
