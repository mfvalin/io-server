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
! ====================================================

module simple_mutex_module
  use ISO_C_BINDING
  use rpn_extra_module
  implicit none

  private

  type, public :: simple_mutex
    private
    type(C_PTR) :: c_location = C_NULL_PTR
    logical :: is_mine = .false.
  contains
    procedure, pass :: is_valid
    procedure, pass :: init
    procedure, pass :: lock
    procedure, pass :: unlock
    procedure, pass :: is_locked
  end type simple_mutex

contains
  
  function is_valid(this)
    implicit none
    class(simple_mutex), intent(in) :: this
    logical :: is_valid
    is_valid = c_associated(this % c_location)
  end function is_valid

  subroutine init(this, c_location)
    implicit none
    class(simple_mutex), intent(inout) :: this
    type(C_PTR), intent(in), value :: c_location

    if (.not. this % is_valid()) then
      this % c_location = c_location
      this % is_mine = .false.
    end if
  end subroutine init

  subroutine lock(this)
    implicit none
    class(simple_mutex), intent(inout) :: this
    if (this % is_valid()) call lock_set(this % c_location)
    this % is_mine = .true.
  end subroutine lock

  subroutine unlock(this)
    implicit none
    class(simple_mutex), intent(inout) :: this
    if (this % is_valid() .and. this % is_mine) then
      call lock_unset(this % c_location)
      this % is_mine = .false.
    end if
  end subroutine unlock

  function is_locked(this, by_me)
    implicit none
    class(simple_mutex), intent(in) :: this
    logical, intent(in) :: by_me
    logical :: is_locked
    is_locked = .false.
    if (this % is_valid()) then
      if (this % is_mine) then
        is_locked = .true.
      else
        if (by_me) then
          is_locked = .false.
        else
          is_locked = (lock_is_set(this % c_location) .ne. 0)
        end if
      end if
    end if
  end function is_locked
end module simple_mutex_module