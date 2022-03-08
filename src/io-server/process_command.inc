interface

subroutine process_command(command_content)
import :: jar
implicit none
type(jar), intent(inout) :: command_content
end subroutine process_command

end interface