! This file has been generated from distributed_circular_buffer.c

include 'io-server/circular_buffer.inc'
interface


function DCB_create(f_communicator, num_producers, num_elements) result(p) BIND(C, name = 'DCB_create_f')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
integer(C_INT), intent(IN), value :: num_elements   !< Number of desired #data_element in the circular buffer
type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
end function DCB_create


subroutine DCB_print(buffer) BIND(C, name = 'DCB_print')
import :: C_PTR
implicit none
type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
end subroutine DCB_print


subroutine DCB_delete(buffer) BIND(C, name = 'DCB_delete')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
end subroutine DCB_delete


function DCB_get_latest_num_elements(buffer) result(num_elements) BIND(C, name = 'DCB_get_latest_num_elements')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_elements
end function DCB_get_latest_num_elements


function DCB_get_num_elements(buffer, buffer_id) result(num_elements) BIND(C, name = 'DCB_get_num_elements')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value    :: buffer
integer(C_INT), intent(in), value :: buffer_id
integer(C_INT) :: num_elements
end function DCB_get_num_elements


function DCB_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'DCB_put')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(in), value           :: buffer
integer(DATA_ELEMENT), dimension(*), intent(in) :: src_data
integer(C_INT), intent(in), value        :: num_elements
integer(C_INT) :: num_available
end function DCB_put


function DCB_get(buffer, buffer_id, dest_data, num_elements) result(num_available) BIND(C, name = 'DCB_get')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(in), value              :: buffer
integer(C_INT), intent(in), value           :: buffer_id
integer(DATA_ELEMENT), dimension(*), intent(inout) :: dest_data
integer(C_INT), intent(in), value           :: num_elements
integer(C_INT) :: num_available
end function DCB_get


end interface

