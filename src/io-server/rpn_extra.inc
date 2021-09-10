! Copyright (C) 2021  Environnement et Changement climatique Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

! This file has been generated from rpn_extra.c

!******************************************************************************
!        INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
!  --------------------------------------------------------------------------
! We use the '//F_StArT' and '//F_EnD' tags to indicate the beginning and end
! of extraction. Anything that happens to be between these tags will be
! included in the output, with the leading '//' and trailing spaces removed.
!
!******************************************************************************
!     INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A FORTRAN FILE
!  --------------------------------------------------------------------------
! Same as for .C files, except that we use the '!! F_StArT' and '!! F_EnD'
! tags. Don't forget to include parameter declarations too.
!
!******************************************************************************

interface
subroutine lock_set(location) BIND(C, name = 'lock_set_F')
import :: C_PTR
type(C_PTR), intent(IN), value :: location
end subroutine lock_set
subroutine lock_reset(location) BIND(C, name = 'lock_reset_F')
import :: C_PTR
type(C_PTR), intent(IN), value :: location
end subroutine lock_reset
function lock_is_set(location) BIND(C, name = 'lock_is_set_F')
import :: C_PTR, C_INT
type(C_PTR), intent(IN), value :: location
integer(C_INT) :: lock_is_set
end function lock_is_set
subroutine sleep_us(num_us) BIND(C, name = 'sleep_us')
import :: C_INT
implicit none
integer(C_INT), intent(in), value :: num_us    !< How many microseconds to sleep
end subroutine sleep_us
subroutine free_c_ptr(ptr) BIND(C, name = 'free_c_ptr')
import :: C_PTR
implicit none
type(C_PTR), intent(INOUT) :: ptr
end subroutine free_c_ptr
end interface
