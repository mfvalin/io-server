interface

subroutine process_command(command_content, stream_id)
    import :: jar, C_PTR
    implicit none
    type(jar), intent(inout) :: command_content
    integer,   intent(in)    :: stream_id
end subroutine process_command

subroutine process_data(data_c, command, stream_id)
    import :: jar, C_PTR
    implicit none
    type(C_PTR), intent(in)    :: data_c
    type(jar),   intent(inout) :: command
    integer,     intent(in)    :: stream_id
end subroutine process_data

end interface