! Copyright (C) 2022  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
!
! Authors:
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022
module process_command_module
  use iso_c_binding
  implicit none
  
  integer(C_INT), parameter :: COMMAND_TYPE_NONE        = -1
  integer(C_INT), parameter :: COMMAND_TYPE_OPEN_FILE   =  0
  integer(C_INT), parameter :: COMMAND_TYPE_CLOSE_FILE  =  1
  integer(C_INT), parameter :: COMMAND_TYPE_WRITE_DATA  =  2
  
  type, public, bind(C) :: command_header
    integer(C_INT) :: command_type = COMMAND_TYPE_NONE  !< Indicates what kind of command we want to execute
    integer(C_INT) :: size_bytes   = 0                  !< Size of the data sent along with the command
  end type command_header

  integer, parameter :: MAX_NUM_OPEN_STREAMS = 20
  integer, dimension(MAX_NUM_OPEN_STREAMS) :: stream_ids = -1
  integer, dimension(MAX_NUM_OPEN_STREAMS) :: file_units = -1

contains
end module process_command_module

subroutine process_command(command_data, stream_id)
  use iso_c_binding
  use jar_module
  use process_command_module
  implicit none
#include <serializer.hf>

  type(jar), intent(inout) :: command_data
  integer,   intent(in)    :: stream_id

  logical :: success
  integer(JAR_ELEMENT) :: num_char
  type(command_header) :: header

  character(len=1), dimension(:), allocatable :: filename
  character(len=:), allocatable :: fname
  integer :: i_stream, first_free_stream

  print '(A, I8)', 'Processing command with size ', command_data % get_top()

  success = JAR_GET_ITEM(command_data, header)

  if (header % command_type == COMMAND_TYPE_OPEN_FILE) then
    print '(A, I5)', 'OPENING FILE for stream ', stream_id

    first_free_stream = -1
    do i_stream = 1, MAX_NUM_OPEN_STREAMS
      if (stream_ids(i_stream) == -1 .and. first_free_stream == -1) first_free_stream = i_stream
      if (stream_ids(i_stream) == stream_id) then
        print *, 'AAAAhhhhh ERROR WE already have an open file on that stream!!!!!'
        error stop 1
      end if
    end do

    if (first_free_stream == -1) then
      print *, 'AAAAHHHHHH no more room left to open new files!!!'
      error stop 1
    end if

    stream_ids(first_free_stream) = stream_id

    num_char = header % size_bytes
    print *, 'NUM CHARS: ', num_char
    allocate(filename(num_char))
    allocate(character(len=num_char) :: fname)
    success = JAR_GET_ITEMS(command_data, filename)
    print '(A, 20A)', ' ------------------ [process_command] ----------------  File name = ', filename(1:num_char + 1)

    fname(1:num_char) = transfer(filename(1:num_char), fname)
    open(newunit = file_units(first_free_stream), file = fname, status = 'replace')

    deallocate(filename)
    deallocate(fname)

  else if (header % command_type == COMMAND_TYPE_CLOSE_FILE) then
    do i_stream = 1, MAX_NUM_OPEN_STREAMS
      if (stream_ids(i_stream) == stream_id) exit
    end do
    if (i_stream <= MAX_NUM_OPEN_STREAMS) then
      print '(A, I5)', 'CLOSING FILE! for stream ', stream_id
      close(file_units(i_stream))
    else
      print *, 'ERROR: This stream had no open file!!!!'
      error stop 1
    end if
  else if (header % command_type == COMMAND_TYPE_WRITE_DATA) then
    print *, 'WRITING DATA. Should not be doing it in this function!!!'
  else
    print *, 'AHHHH INVALID COMMAND TYPE'
    error stop 1
  end if

end subroutine process_command

subroutine process_data(data_c, command, stream_id)
  use iso_c_binding
  use jar_module
  use process_command_module
  implicit none
#include <serializer.hf>

  type(C_PTR), intent(in)    :: data_c
  type(jar),   intent(inout) :: command
  integer,     intent(in)    :: stream_id
end subroutine process_data
