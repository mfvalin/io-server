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
// This file has been generated from rpn_extra.c
#ifndef IO_SERVER_rpn_extra_GEN_H
#define IO_SERVER_rpn_extra_GEN_H

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
}

//! Memory load fence
static inline void read_fence() {
  __asm__ volatile("lfence" : : : "memory");
}

//! Memory load+store fence
static inline void full_memory_fence() {
  __asm__ volatile("mfence" : : : "memory");
}

//! Acquire the given lock, with the given ID, *without* a memory fence
static inline void acquire_idlock_no_fence(volatile int32_t *lock, int32_t id) {
  __asm__ volatile("": : :"memory");
  while(__sync_val_compare_and_swap(lock, 0, (id+1)) != 0)
    ;
  __asm__ volatile("": : :"memory");
}

//! Acquire the given lock, with the given ID.
static inline void acquire_idlock(volatile int32_t *lock, int32_t id) {
  __asm__ volatile("": : :"memory");
  while(__sync_val_compare_and_swap(lock, 0, (id+1)) != 0)
    ;
  full_memory_fence();
}

//! Acquire the given lock, no specific ID, *without* a memory fence
static inline void acquire_lock_no_fence(volatile int32_t *lock) {
  acquire_idlock_no_fence(lock, 1); // Use 1 as ID
}

//! Acquire the given lock, no specific ID.
static inline void acquire_lock(volatile int32_t *lock) {
  acquire_idlock(lock, 1); // Use 1 as ID
}

//! Try to acquire the given lock, with a specific ID
//! @return true if the lock was successfully acquired by the given ID, false if it was already held by someone
static inline int32_t try_acquire_idlock(volatile int32_t *lock, int32_t id) {
  __asm__ volatile("": : :"memory");
  if (__sync_val_compare_and_swap(lock, 0, (id+1)) != 0) return 0;
  full_memory_fence();
  return 1;
}

//! Release given lock if it has this specific ID (will deadlock if ID is wrong), *without* a memory fence
static inline void release_idlock_no_fence(volatile int32_t *lock, int32_t id) {
  __asm__ volatile("": : :"memory");
  while(__sync_val_compare_and_swap(lock, (id+1), 0) != (id+1))
    ;
  __asm__ volatile("": : :"memory");
}

//! Release lock if it has specific ID (deadlocks if ID is wrong)
static inline void release_idlock(volatile int32_t *lock, int32_t id) {
  full_memory_fence();
  __sync_val_compare_and_swap(lock, (id+1), 0);
  __asm__ volatile("": : :"memory");
}

//! Release given lock with ID 1 (deadlocks if ID is wrong), *without* a fence
static inline void release_lock_no_fence(volatile int32_t *lock) {
  release_idlock_no_fence(lock, 1);
}

//! Release given lock with ID 1 (deadlocks if ID is wrong)
static inline void release_lock(volatile int32_t *lock) {
  release_idlock(lock, 1) ;
}

//! Test if lock is held by given ID
//! @return true if lock is held by [ID], false otherwise
static inline int32_t is_idlock_taken(volatile int32_t *lock, int32_t id) {
  return (*lock == (id+1));
}

//! Test if lock is held by anyone
//! @return true if lock is held by someone, false otherwise
static inline int32_t is_lock_taken(volatile int32_t *lock) {
  return (*lock != 0);
}

//! Forcefully reset given lock
static inline void reset_lock(volatile int32_t *lock) { 
  *lock = 0;
}
//! Do nothing for a certain number of microseconds
void sleep_us(const int num_us //!< [in] How many microseconds we want to wait
);
static inline void rpn_usleep(const int num_us)
{
  sleep_us(num_us);
}

#endif // IO_SERVER_rpn_extra_GEN_H
