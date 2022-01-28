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

module rpn_extra_module
  use ISO_C_BINDING
  implicit none

  include 'io-server/rpn_extra.inc'

contains

  !> Compute the number of 32-bit integers into which the given number of characters can entirely fit
  !> It's basically [num_char] / 4, rounded up
  function num_char_to_num_int4(num_char) result(num_int)
    implicit none
    integer, intent(in) :: num_char !< [in] How many characters we have
    integer             :: num_int  !< How many 32-bit integers we need to contain every char

    num_int = (num_char + 3) / 4
  end function num_char_to_num_int4

  !> Compute the number of 64-bit integers into which the given number of characters can entirely fit
  !> It's basically [num_char] / 8, rounded up
  function num_char_to_num_int8(num_char) result(num_int8)
    implicit none
    integer(kind=8), intent(in) :: num_char !< [in] How many characters we have
    integer(kind=8)             :: num_int8 !< How many 64-bit integers we need to contain every char
    num_int8 = (num_char + 7) / 8
  end function num_char_to_num_int8

end module rpn_extra_module
