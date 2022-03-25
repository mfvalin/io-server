interface

subroutine process_command(command_content, grid_data)
import :: jar, C_PTR
implicit none
type(jar), intent(inout) :: command_content
type(C_PTR), intent(in) :: grid_data
end subroutine process_command

end interface