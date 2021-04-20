#ifndef IO_SERVER_CB_DATA_H_
#define IO_SERVER_CB_DATA_H_

#ifndef IN_FORTRAN_CODE
//! Type of individual elements stored in a circular buffer type container
typedef int32_t data_element;
#else
integer, parameter :: DATA_ELEMENT = C_INT !< Element type for containers. Must match the size of #data_element
#endif

#endif /* IO_SERVER_CB_DATA_H_ */
