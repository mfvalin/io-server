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

! This file has been generated from timer.c

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
function IO_timer_create() result(timer) BIND(C, name = 'IO_timer_create_f')
import C_PTR
implicit none
type(C_PTR) :: timer
end function IO_timer_create
subroutine IO_timer_delete(timer) BIND(C, name = 'IO_timer_delete_f')
import C_PTR
implicit none
type(C_PTR), intent(IN), value :: timer
end subroutine IO_timer_delete
subroutine IO_timer_start(timer) BIND(C, name = 'IO_timer_start_f')
import C_PTR
implicit none
type(C_PTR), intent(IN), value :: timer
end subroutine IO_timer_start
subroutine IO_timer_stop(timer) BIND(C, name = 'IO_timer_stop_f')
import C_PTR
implicit none
type(C_PTR), intent(IN), value :: timer
end subroutine IO_timer_stop
function IO_time_ms(timer) result(time) BIND(C, name = 'IO_time_ms_f')
import C_PTR, C_DOUBLE
implicit none
type(C_PTR), intent(IN), value :: timer
real(C_DOUBLE) :: time
end function IO_time_ms
function rdtscp() result(count) BIND(C, name = 'rdtscp_f')
import C_INT64_T
implicit none
integer(C_INT64_T) :: count
end function rdtscp
function rdtsc() result(count) BIND(C, name = 'rdtsc_f')
import C_INT64_T
implicit none
integer(C_INT64_T) :: count
end function rdtsc
end interface
