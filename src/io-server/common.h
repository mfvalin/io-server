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
// This file has been generated from common.c
#ifndef IO_SERVER_common_GEN_H
#define IO_SERVER_common_GEN_H


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

//! Type of individual elements stored in a container
typedef int32_t data_element;
//! Type of index for computing offsets in a container (must be at least the same size as #data_element)
typedef int32_t data_index;
//! Do nothing for a certain number of microseconds
void sleep_us(const int num_us //!< [in] How many microseconds we want to wait
);

#endif // IO_SERVER_common_GEN_H
