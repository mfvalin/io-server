interface

function remote_circular_buffer_create(f_communicator, root, rank, comm_size, num_words) result(p) BIND(C, name = 'remote_circular_buffer_create')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the remote buffer is shared
integer(C_INT), intent(IN), value :: root           !< Process rank on which buffer data is located
integer(C_INT), intent(IN), value :: rank           !< Rank of the current process
integer(C_INT), intent(IN), value :: comm_size      !< Number of processes in the communicator
integer(C_INT), intent(IN), value :: num_words      !< Number of 32-bit elements in the circular buffer
type(C_PTR) :: p                                    !< Pointer to created remote circular buffer
end function remote_circular_buffer_create


subroutine remote_circular_buffer_print(buffer) BIND(C, name = 'remote_circular_buffer_print')
import :: C_PTR
implicit none
type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
end subroutine remote_circular_buffer_print


subroutine remote_circular_buffer_delete(buffer) BIND(C, name = 'remote_circular_buffer_delete')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
end subroutine remote_circular_buffer_delete


function remote_circular_buffer_put(buffer, src_data, num_elements) result(num_available) BIND(C, name = 'remote_circular_buffer_put')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value           :: buffer
integer(C_INT), dimension(*), intent(in) :: src_data
integer(C_INT), intent(in), value        :: num_elements
integer(C_INT) :: num_available
end function remote_circular_buffer_put


subroutine buffer_write_test(buffer) BIND(C, name = 'buffer_write_test')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: buffer
end subroutine buffer_write_test

end interface
