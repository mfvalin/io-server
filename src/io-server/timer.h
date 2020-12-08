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
