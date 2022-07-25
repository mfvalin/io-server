interface

function process_command(command_content, stream_id) result(success)
    import :: jar, C_PTR
    implicit none
    type(jar), intent(inout) :: command_content
    integer,   intent(in)    :: stream_id
    logical :: success
end function process_command

function process_data(data_c, command, stream_id) result(success)
    import :: jar, C_PTR
    implicit none
    type(C_PTR), intent(in)    :: data_c
    type(jar),   intent(inout) :: command
    integer,     intent(in)    :: stream_id
    logical :: success
end function process_data

end interface