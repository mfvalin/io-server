! This file has been generated from common.c

integer, parameter :: DATA_ELEMENT = C_INT !< Element type for containers. Must match the size of #data_element
interface


subroutine sleep_us(num_us) BIND(C, name = 'sleep_us')
import :: C_INT
implicit none
integer(C_INT), intent(in), value :: num_us    !< How many microseconds to sleep
end subroutine sleep_us


subroutine free_c_ptr(ptr) BIND(C, name = 'free_c_ptr')
import :: C_PTR
implicit none
type(C_PTR), intent(INOUT) :: ptr
end subroutine free_c_ptr


end interface

