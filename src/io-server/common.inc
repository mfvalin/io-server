
integer, parameter :: DATA_ELEMENT = C_INT !< Element type for containers. Must match the size of #data_element
interface


subroutine sleep_us(num_us) BIND(C, name = 'sleep_us')
import :: C_INT
implicit none
integer(C_INT), intent(in), value :: num_us    !< How many microseconds to sleep
end subroutine sleep_us


end interface

