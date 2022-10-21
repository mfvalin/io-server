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
!> \file atomic.F90 Module to facilitate atomic updates to variables

module atomic_module
  use iso_c_binding
  use rpn_extra_module
  implicit none
  private

  type, public :: atomic_int32
    private
    type(C_PTR) :: c_location = C_NULL_PTR
  contains
    procedure, pass :: init_from_int  => atomic_int32_init_from_int
    procedure, pass :: add            => atomic_int32_add
    procedure, pass :: try_update     => atomic_int32_try_update
    procedure, pass :: read           => atomic_int32_read
  end type atomic_int32
contains

subroutine atomic_int32_init_from_int(this, variable)
  implicit none
  class(atomic_int32), intent(inout) :: this
  integer(C_INT32_T),  intent(in), target :: variable
  this % c_location = c_loc(variable)
end subroutine atomic_int32_init_from_int

function atomic_int32_add(this, increment) result(new_value)
  implicit none
  class(atomic_int32), intent(inout) :: this
  integer(C_INT32_T),  intent(in)    :: increment
  integer(C_INT32_T) :: new_value
  new_value = atomic_add_int32(this % c_location, increment)
end function atomic_int32_add

function atomic_int32_try_update(this, old_value, new_value) result(has_updated)
  implicit none
  class(atomic_int32), intent(inout) :: this !< atomic_int32 instance
  integer(C_INT32_T),  intent(in)    :: old_value
  integer(C_INT32_T),  intent(in)    :: new_value
  logical :: has_updated
  has_updated = .false.
  if (c_associated(this % c_location)) has_updated = (try_update_int32(this % c_location, old_value, new_value) == 1)
end function

function atomic_int32_read(this) result(val)
  implicit none
  class(atomic_int32), intent(in) :: this
  integer(C_INT32_T) :: val
  integer, pointer :: value_ptr
  call c_f_pointer(this % c_location, value_ptr)
  val = value_ptr
end function atomic_int32_read

end module atomic_module
