#ifndef IO_SERVER_CB_DATA_H_
#define IO_SERVER_CB_DATA_H_

#ifndef IN_FORTRAN_CODE
#include <stdint.h>
typedef int32_t data_element; //!< Type of individual elements stored in a circular buffer type container
#else
integer, parameter :: DATA_ELEMENT = C_INT !< Element type for containers. Must match the size of #data_element
#endif

#if 0
// List of defines that are used from both C and Fortran code
#endif
#define CB_NO_COMMIT_val 0
#define CB_COMMIT_val 1
#define CB_PEEK_val 2

#ifndef IN_FORTRAN_CODE
enum
{
  CB_NO_COMMIT = CB_NO_COMMIT_val,
  CB_COMMIT    = CB_COMMIT_val,
  CB_PEEK      = CB_PEEK_val
};
#else
        integer, parameter :: CB_NO_COMMIT = CB_NO_COMMIT_val
        integer, parameter :: CB_COMMIT = CB_COMMIT_val
        integer, parameter :: CB_PEEK = CB_PEEK_val

        integer, parameter :: CB_KIND_CHAR      = -1
        integer, parameter :: CB_KIND_INTEGER_4 = -4
        integer, parameter :: CB_KIND_INTEGER_8 = -8
        integer, parameter :: CB_KIND_REAL_4    = -4
        integer, parameter :: CB_KIND_REAL_8    = -8
#endif

#undef CB_NO_COMMIT_val
#undef CB_COMMIT_val
#undef CB_PEEK_val

#endif /* IO_SERVER_CB_DATA_H_ */
