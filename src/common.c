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
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
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

//C_StArT
/**
 * @brief Copy buffer elements into another array (either into or out of the buffer)
 */
static inline void copy_elements(
    data_element*       dst, //!< [out] Where to copy the elements
    const data_element* src, //!< [in]  The elements to copy
    int                 n    //!< [in] How many we want to copy
) {
  memcpy(dst, src, sizeof(data_element) * (size_t)n);
}
//C_EnD

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

//C_StArT
//! Compute the space in kilobytes taken by the given number of elements
static inline double num_elem_to_kb(const size_t num_elements) {
  return num_elements * sizeof(data_element) / 1024.0;
}
//C_EnD

//C_StArT
//! Provide a string representation of a number in a human readable way (with the k, M or G suffix if needed)
void readable_element_count(
    const double num_elements, //!< [in]  Number we want to represent
    char*        buffer        //!< [out] Buffer where the string will be stored. Must contain at least 8 bytes
    )
//C_EnD
{
  double amount = num_elements;
  int    unit   = 0;

  const char UNITS[] = {'\0', 'k', 'M', 'G'};

  while (amount > 1900.0 && unit < 3) {
    amount /= 1000.0;
    unit++;
  }

  if (unit == 0) {
    if (ceil(amount) == amount)
      sprintf(buffer, "%7.0f", amount);
    else
      sprintf(buffer, "%7.2f", amount);
  }
  else {
    sprintf(buffer, "%6.1f%c", amount, UNITS[unit]);
  }
}

//F_StArT
//  end interface
//F_EnD
