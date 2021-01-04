/*
 * Copyright (C) 2021  Environnement Canada
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

#ifndef IO_SERVER_TIMER_H
#define IO_SERVER_TIMER_H

#include <stdint.h>
#include <time.h>

// Timer that can accumulate microsecond intervals
typedef struct {
  uint64_t start;
  uint64_t total_time;
} io_timer_t;

// Get current system time in microseconds, wraps around approximately every year
static inline uint64_t get_current_time_us() {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);

  // Wraps around every year or so. Not sure why you would need microsecond precision for longer
  const uint64_t now_us = ((uint64_t)now.tv_sec % (1 << 25)) * 1000000 + (uint64_t)now.tv_nsec / 1000;

  return now_us;
}

void io_timer_start(io_timer_t* timer) {
  timer->start = get_current_time_us();
}

void io_timer_stop(io_timer_t* timer) {
  timer->total_time += get_current_time_us() - timer->start;
}

double io_time_ms(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return timer->total_time / 1000.0;
}

#endif // IO_SERVER_TIMER_H
