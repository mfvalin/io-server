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
  const int32_t val = __sync_val_compare_and_swap(lock, (id+1), 0);
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
//C_EnD

//F_StArT
//  subroutine acquire_idlock(lock, id) BIND(C, name = 'acquire_idlock_F')
//    import :: C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: lock
//    integer(C_INT), intent(in), value :: id
//  end subroutine acquire_idlock
//  subroutine release_idlock(lock, id) BIND(C, name = 'release_idlock_F')
//    import :: C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: lock
//    integer(C_INT), intent(in), value :: id
//  end subroutine release_idlock
//  function try_acquire_idlock(lock, id) result(is_successfully_acquired) BIND(C, name = 'try_acquire_idlock_F')
//    import :: C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: lock
//    integer(C_INT), intent(in), value :: id
//    integer(C_INT) :: is_successfully_acquired
//  end function try_acquire_idlock
//  function is_idlock_taken(lock, id) result(is_locked) BIND(C, name = 'is_idlock_taken_F')
//    import :: C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: lock
//    integer(C_INT), intent(in), value :: id
//    integer(C_INT) :: is_locked
//  end function is_idlock_taken
//  function is_lock_taken(lock) result(is_locked) BIND(C, name = 'is_lock_taken_F')
//    import :: C_INT, C_PTR
//    implicit none
//    type(C_PTR),    intent(in), value :: lock
//    integer(C_INT) :: is_locked
//  end function is_lock_taken
//  subroutine reset_lock(lock) BIND(C, name = 'reset_lock_F')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(in), value :: lock
//  end subroutine reset_lock
//F_EnD
void    acquire_idlock_F(volatile int32_t *lock, int32_t id) { acquire_idlock(lock, id); }
void    release_idlock_F(volatile int32_t *lock, int32_t id) { release_idlock(lock, id); }
int32_t try_acquire_idlock_F(volatile int32_t *lock, int32_t id) { return try_acquire_idlock(lock, id); }
int32_t is_idlock_taken_F(volatile int32_t *lock, int32_t id) { return is_idlock_taken(lock, id); }
int32_t is_lock_taken_F(volatile int32_t *lock) { return is_lock_taken(lock); }
void    reset_lock_F(volatile int32_t *lock) { reset_lock(lock); }


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
