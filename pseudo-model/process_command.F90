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
!> \author  V. Magnoux, Recherche en Prevision Numerique
!> \date    2020-2022
!> \file process_command.F90 Example implementation of the command/data processing functions provided by a model

module process_command_module
  use iso_c_binding
  implicit none
  
  integer(C_INT), parameter :: COMMAND_TYPE_NONE        = -1
  integer(C_INT), parameter :: COMMAND_TYPE_OPEN_FILE   =  0
  integer(C_INT), parameter :: COMMAND_TYPE_CLOSE_FILE  =  1
  integer(C_INT), parameter :: COMMAND_TYPE_WRITE_DATA  =  2
  integer(C_INT), parameter :: COMMAND_TYPE_WRITE_DISPLAY_DATA  =  3
  
  type, public, bind(C) :: command_header
    integer(C_INT) :: command_type = COMMAND_TYPE_NONE  !< Indicates what kind of command we want to execute
    integer(C_INT) :: size_bytes   = 0                  !< Size of the data sent along with the command
  end type command_header

  type, public, bind(C) :: model_grid
    integer(C_INT64_T), dimension(5) :: dims
    integer(C_INT) :: elem_size
  end type model_grid

  integer, parameter :: MAX_NUM_OPEN_STREAMS = 20
  integer, dimension(MAX_NUM_OPEN_STREAMS) :: stream_ids = -1
  integer, dimension(MAX_NUM_OPEN_STREAMS) :: file_units = -1

contains

  function get_free_stream_rank(stream_id) result(free_stream_rank)
    implicit none
    integer, intent(in) :: stream_id
    integer :: free_stream_rank

    integer :: i_stream

    free_stream_rank = -1
    do i_stream = 1, MAX_NUM_OPEN_STREAMS
      if (stream_ids(i_stream) == -1 .and. free_stream_rank == -1) free_stream_rank = i_stream
      if (stream_ids(i_stream) == stream_id) then
        print *, 'AAAAhhhhh ERROR WE already have an open file on that stream!!!!!'
        error stop 1
      end if
    end do

  end function get_free_stream_rank

  function get_stream_rank(stream_id) result(stream_rank)
    implicit none
    integer, intent(in) :: stream_id
    integer :: stream_rank

    do stream_rank = 1, MAX_NUM_OPEN_STREAMS
      if (stream_ids(stream_rank) == stream_id) return
    end do
    stream_rank = -1
  end function get_stream_rank

end module process_command_module

function process_command(command_data, stream_id) result(success)
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

  character(len=:), allocatable :: filename
  integer :: stream_rank

  print '(A, I8)', 'Processing command with size ', command_data % get_top()

  success = JAR_GET_ITEM(command_data, header)

  if (.not. success) return

  if (header % command_type == COMMAND_TYPE_OPEN_FILE) then
    print '(A, I5)', 'OPENING FILE for stream ', stream_id

    stream_rank = get_free_stream_rank(stream_id)
    if (stream_rank == -1) then
      print *, 'AAAAHHHHHH no more room left to open new files!!!'
      success = .false.
      return
    end if

    stream_ids(stream_rank) = stream_id

    ! Retrieve file name
    num_char = header % size_bytes
    allocate(character(len=num_char) :: filename)
    success = JAR_GET_ITEM(command_data, filename)

    if (success) open(newunit = file_units(stream_rank), file = filename, status = 'replace', form = 'unformatted')

    deallocate(filename)

  else if (header % command_type == COMMAND_TYPE_CLOSE_FILE) then
    stream_rank = get_stream_rank(stream_id)
    if (stream_rank > 0) then
      print '(A, I5)', 'CLOSING FILE! for stream ', stream_id
      close(file_units(stream_rank))
      success = .true.
    else
      print *, 'ERROR: This stream had no open file!!!!'
      success = .false.
      return
    end if

  else if (header % command_type == COMMAND_TYPE_WRITE_DATA .or. header % command_type == COMMAND_TYPE_WRITE_DISPLAY_DATA) then
    print *, 'ERROR: WRITING DATA. Should not be doing it in this function!!!'
    success = .false.

  else
    print '(A, I12)', 'ERROR: AHHHH INVALID COMMAND TYPE ', header % command_type
    success = .false.

  end if

end function process_command

function process_data(data_c, command, stream_id) result(success)
  use iso_c_binding
  use jar_module
  use process_command_module
  implicit none
#include <serializer.hf>

  type(C_PTR), intent(in)    :: data_c
  type(jar),   intent(inout) :: command
  integer,     intent(in)    :: stream_id

  type(command_header) :: header
  type(model_grid)     :: grid
  integer(C_INT8_T), dimension(:, :, :, :, :), pointer :: grid_data_byte
  integer(C_INT64_T), dimension(5) :: grid_size_byte
  integer :: stream_rank
  logical :: success

  if (command % get_num_avail() <= 0) then
    print *, 'ERROR: There is no command coming with the data!'
    success = .false.
    return
  end if

  success = JAR_GET_ITEM(command, header)

  ! print *, 'Processing data for stream ', stream_id, transfer(data_c, 1_8), command % get_top(), header % command_type
  
  if (header % command_type == COMMAND_TYPE_WRITE_DATA .or. header % command_type == COMMAND_TYPE_WRITE_DISPLAY_DATA) then
    success = JAR_GET_ITEM(command, grid)
    ! print *, 'Grid: ', grid % dims, grid % elem_size

    if (.not. success) return

    if (header % command_type == COMMAND_TYPE_WRITE_DISPLAY_DATA) then
      if (grid % elem_size .ne. 8) then
        print *, 'ERROR: Can only handle grid elements of size 8 for now'
        success = .false.
        return
      end if

      block
        integer(C_INT64_T), dimension(:, :, :, :, :), pointer :: grid_data_int8
        call c_f_pointer(data_c, grid_data_int8, grid % dims)
        print '(A, /, (10I5))', 'Grid content: ', grid_data_int8
      end block
    end if

    stream_rank = get_stream_rank(stream_id)

    if (stream_rank > 0) then
      grid_size_byte = grid % dims
      grid_size_byte(1) = grid_size_byte(1) * grid % elem_size
      call c_f_pointer(data_c, grid_data_byte, grid_size_byte)
      write(unit=file_units(stream_rank)) grid_data_byte
    else
      success = .false.
      return
    end if
  
  else
    print *, 'ERROR: Unrecognized command type for processing data', header % command_type
    success = .false.
    return

  end if
  
  if (command % get_num_avail() > 0) then
    print *, 'ERROR: There is still data available in the command buffer', command % get_num_avail()
    success = .false.
    return
  end if
  
end function process_data
