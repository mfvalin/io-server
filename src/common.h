#include <immintrin.h>
#include <stdint.h>

//! Memory store fence
static inline void write_fence() {
  __asm__ volatile("" : : : "memory");
  _mm_sfence();
}

//! Memory load fence
static inline void read_fence() {
  __asm__ volatile("" : : : "memory");
  _mm_lfence();
}

//! memory load+store fence
static inline void memory_fence() {
  __asm__ volatile("" : : : "memory");
  _mm_mfence();
}

//! Type of individual elements stored in a circular buffer
typedef int32_t cb_element;
//! Type of index for computing offsets in a circular buffer (must be at least the same size as #cb_element)
typedef int32_t cb_index;
//C_EnD
//F_StArT
//  integer, parameter :: CB_ELEMENT = C_INT !< Circular buffer element type. Must match the size of #cb_element
//F_EnD
