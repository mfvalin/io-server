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

//C_StArT

#include <immintrin.h>
#include <stdint.h>
#include <stdlib.h>
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

//C_EnD

//C_StArT
//! Type of individual elements stored in a container
typedef int32_t data_element;
//! Type of index for computing offsets in a container (must be at least the same size as #data_element)
typedef int32_t data_index;
//C_EnD

//F_StArT
//  integer, parameter :: DATA_ELEMENT = C_INT !< Element type for containers. Must match the size of #data_element
//  interface
//F_EnD

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
