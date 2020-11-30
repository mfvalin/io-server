
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
//! Type of individual elements stored in a container
typedef int32_t data_element;
//! Type of index for computing offsets in a container (must be at least the same size as #data_element)
typedef int32_t data_index;
//! Do nothing for a certain number of microseconds
void sleep_us(const int num_us //!< [in] How many microseconds we want to wait
);
