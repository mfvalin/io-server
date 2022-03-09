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

subroutine process_command(command_data)
  use iso_c_binding
  use jar_module
  implicit none
#include <serializer.hf>

  type(jar), intent(inout) :: command_data
  integer(JAR_ELEMENT) :: num_jar_elem
  integer :: num_char

  character(len=1), dimension(:), allocatable :: filename
  character(len=:), allocatable :: fname
  integer :: file_unit

  num_char = command_data % high() * 8
  allocate(filename(num_char))
  allocate(character(len=num_char) :: fname)
  num_jar_elem = JAR_GET_ITEMS(command_data, filename)
  print *, 'File name = ', filename

  fname(1:num_char) = transfer(filename(1:num_char), fname)
  open(newunit = file_unit, file = fname, status = 'replace')
  ! close(10)

end subroutine process_command
