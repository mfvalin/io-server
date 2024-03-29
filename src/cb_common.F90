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
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

module cb_common_module
  use ISO_C_BINDING
  implicit none

#include "io-server/cb_data.hf"

contains

  !> \brief Compute the size in bytes of a given type ID
  !> \return The size in bytes of the given type ID. -1 if the ID is invalid
  pure function get_type_size(type_id) result(type_size)
    implicit none
    integer, intent(IN) :: type_id
    integer :: type_size
    type_size = -1
    if (type_id < 0) type_size = -type_id
  end function get_type_size

 end module cb_common_module
