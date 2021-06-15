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
// This file has been generated from timer.c
#ifndef IO_SERVER_timer_GEN_H
#define IO_SERVER_timer_GEN_H

/******************************************************************************
         INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
   --------------------------------------------------------------------------
 We use the '//C_StArT' and '//C_EnD' tags to indicate the beginning and end of
 extraction.
 To extract the entire function (for inline functions, for example), you must
 put the begin/end tags around the entire function code, and **MAKE SURE TO
 LEAVE A SPACE** between the closing parenthesis of the function header and the
 opening bracket of the function body. They have to be on the same line.

 For example:
     //C_StArT
     inline int my_function(type1 arg1, type2* arg2) {
         [function body]
     }
     //C_EnD

 or also:
     //C_StArT
     inline int my_function(type1 arg1, //!< Doxygen doc
                            type2* arg2 //!< More doc
       ) {
         [function body]
     }
     //C_EnD


 To extract the function interface only, you must put the begin/end tags around
 the header. The closing parenthesis/opening bracket must either
  - be on the same line, without a space between them
or
  - be on different lines, with the closing parenthesis by itself on a line

 For example:
     //C_StArT
     int my_function(
         type1 arg1, //! Doxygen doc
         type2* arg2 //! Moar doc
         )
     //C_EnD
     {
         [function body]
     }

 or also:
     //C_StArT
     int my_function(type1 arg1, type2* arg2){
     //C_EnD
         [function body]
     }
 ******************************************************************************/

#include <stdint.h>
#include <stdlib.h>
#include <time.h>

//! Timer that can accumulate microsecond intervals
typedef struct {
  uint64_t start;      //! Timestamp when the timer was started
  uint64_t total_time; //! How many clock ticks have been recorded (updates every time the timer stops)
} io_timer_t;

static const clockid_t IO_CLOCK_ID = CLOCK_MONOTONIC;

//! Get current system time in microseconds, wraps around approximately every year
static inline uint64_t get_current_time_us() {
  struct timespec now;
  clock_gettime(IO_CLOCK_ID, &now);

  // Wraps around every year or so. Not sure why you would need microsecond precision for longer
  const uint64_t now_us = ((uint64_t)now.tv_sec % (1 << 25)) * 1000000 + (uint64_t)now.tv_nsec / 1000;

  return now_us;
}
//! Record the current timestamp
static inline void IO_timer_start(io_timer_t* timer) {
  timer->start = get_current_time_us();
}
//! Increment total time with number of ticks since last start
static void IO_timer_stop(io_timer_t* timer) {
  timer->total_time += get_current_time_us() - timer->start;
}
//! Retrieve the accumulated time in number of milliseconds, as a double
static inline double IO_time_ms(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return timer->total_time / 1000.0;
}
static inline double IO_time_since_start(const io_timer_t* timer) {
  // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
  return (get_current_time_us() - timer->start) / 1000.0;
}

#endif // IO_SERVER_timer_GEN_H
