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

//F_StArT
//  interface
//F_EnD

//C_StArT
#include <immintrin.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

//! Memory store fence
static inline void write_fence() {
  __asm__ volatile("sfence" : : : "memory");
  //   _mm_sfence();
}

//! Memory load fence
static inline void read_fence() {
  __asm__ volatile("lfence" : : : "memory");
  //   _mm_lfence();
}

//! memory load+store fence
static inline void memory_fence() {
  __asm__ volatile("mfence" : : : "memory");
  //   _mm_mfence();
}

static inline void lock_set(int* location) {
  while (__sync_val_compare_and_swap(location, 0, 1) != 0)
    ;
}
static inline void lock_reset(int* location) {
  *(volatile int*)location = 0;
}

static inline int lock_is_set(int* location) {
  const int value = *(volatile int*)location;
  return value;
}
//C_EnD

//F_StArT
//  subroutine lock_set(location) BIND(C, name = 'lock_set_F')
//    import :: C_PTR
//    type(C_PTR), intent(IN), value :: location
//  end subroutine lock_set
//F_EnD
void lock_set_F(int* location) { lock_set(location); }

//F_StArT
//  subroutine lock_reset(location) BIND(C, name = 'lock_reset_F')
//    import :: C_PTR
//    type(C_PTR), intent(IN), value :: location
//  end subroutine lock_reset
//F_EnD
void lock_reset_F(int* location) { lock_reset(location); }

//F_StArT
//  function lock_is_set(location) BIND(C, name = 'lock_is_set_F')
//    import :: C_PTR, C_INT
//    type(C_PTR), intent(IN), value :: location
//    integer(C_INT) :: lock_is_set
//  end function lock_is_set
//F_EnD
int lock_is_set_F(int* location) { return lock_is_set(location); }

//F_StArT
//  subroutine sleep_us(num_us) BIND(C, name = 'sleep_us')
//    import :: C_INT
//    implicit none
//    integer(C_INT), intent(in), value :: num_us    !< How many microseconds to sleep
//  end subroutine sleep_us
//F_EnD
//C_StArT
//! Do nothing for a certain number of microseconds
void sleep_us(const int num_us //!< [in] How many microseconds we want to wait
              )
//C_EnD
{
  struct timespec ts;
  ts.tv_sec  = num_us / 1000000;
  ts.tv_nsec = num_us % 1000000 * 1000;
  nanosleep(&ts, NULL);
}

//C_StArT
static inline void rpn_usleep(const int num_us)
{
  sleep_us(num_us);
}
//C_EnD

//F_StArT
//  subroutine free_c_ptr(ptr) BIND(C, name = 'free_c_ptr')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(INOUT) :: ptr
//  end subroutine free_c_ptr
//F_EnD
void free_c_ptr(void** ptr) {
  free(*ptr);
  *ptr = NULL;
}

//F_StArT
//  end interface
//F_EnD
