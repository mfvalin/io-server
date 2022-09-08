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
! ====================================================

module simple_mutex_module
  use ISO_C_BINDING
  use rpn_extra_module
  use ioserver_timer_module
  implicit none

  private

  type, public :: simple_mutex
    private
    type(C_PTR)          :: c_location = C_NULL_PTR
    type(ioserver_timer) :: wait_timer
    integer(C_INT)       :: id = -1
  contains
    procedure, pass :: is_valid
    procedure, pass :: init_from_ptr
    procedure, pass :: init_from_int
    procedure, pass :: lock
    procedure, pass :: unlock
    procedure, pass :: try_lock
    procedure, pass :: is_locked
    procedure, pass :: is_locked_by_me
    procedure, pass :: reset
    procedure, pass :: get_id
    procedure, pass :: get_total_wait_time_ms
    procedure, pass :: get_latest_wait_time_ms
  end type simple_mutex

contains
  
  function is_valid(this)
    implicit none
    class(simple_mutex), intent(in) :: this
    logical :: is_valid
    is_valid = c_associated(this % c_location) .and. (this % id >= 0)
  end function is_valid

  subroutine init_from_ptr(this, c_location, id)
    implicit none
    class(simple_mutex), intent(inout)     :: this
    type(C_PTR),         intent(in), value :: c_location
    integer(C_INT),      intent(in)        :: id

    if (.not. this % is_valid()) then
      this % c_location = c_location
      this % id = id
      call this % reset()
    end if
  end subroutine init_from_ptr

  subroutine init_from_int(this, lock_var, id)
    implicit none
    class(simple_mutex), intent(inout)      :: this
    integer(C_INT),      intent(in), target :: lock_var
    integer(C_INT),      intent(in)         :: id

    if (.not. this % is_valid()) then
      this % c_location = C_LOC(lock_var)
      this % id = id
      call this % reset()
    end if
  end subroutine init_from_int

  subroutine lock(this)
    implicit none
    class(simple_mutex), intent(inout) :: this
    if (this % is_valid()) then
      call this % wait_timer % start()
      call acquire_idlock(this % c_location, this % id)
      call this % wait_timer % stop()
    end if
  end subroutine lock

  subroutine unlock(this)
    implicit none
    class(simple_mutex), intent(inout) :: this
    if (this % is_valid()) call release_idlock(this % c_location, this % id)
  end subroutine unlock

  function try_lock(this) result(is_successfully_acquired)
    implicit none
    class(simple_mutex), intent(inout) :: this
    logical :: is_successfully_acquired

    is_successfully_acquired = .false.

    if (this % is_valid()) then
      if (try_acquire_idlock(this % c_location, this % id) .ne. 0) then
        is_successfully_acquired = .true.
      end if
    end if
  end function try_lock

  function is_locked(this)
    implicit none
    class(simple_mutex), intent(in) :: this
    logical :: is_locked

    integer(C_INT) :: is_locked_c

    is_locked = .false.
    if (this % is_valid()) then
      is_locked_c = is_lock_taken(this % c_location)
      if (is_locked_c .ne. 0) then
        is_locked = .true.
      end if
    end if
  end function is_locked

  function is_locked_by_me(this)
    implicit none
    class(simple_mutex), intent(in) :: this
    logical :: is_locked_by_me

    integer(C_INT) :: is_locked_c

    is_locked_by_me = .false.
    if (this % is_valid()) then
      is_locked_c = is_idlock_taken(this % c_location, this % id)
      if (is_locked_c .ne. 0) then
        is_locked_by_me = .true.
      end if
    end if
  end function is_locked_by_me

  subroutine reset(this)
    implicit none
    class(simple_mutex), intent(inout) :: this
    if (this % is_valid()) call reset_lock(this % c_location)
    if (this % wait_timer % is_valid()) call this % wait_timer % delete()
    call this % wait_timer % create()
  end subroutine reset

  function get_id(this) result(id)
    implicit none
    class(simple_mutex), intent(in) :: this
    integer(C_INT) :: id
    id = this % id
  end function get_id

  function get_total_wait_time_ms(this) result(wait_time)
    implicit none
    class(simple_mutex), intent(in) :: this
    real(C_DOUBLE) :: wait_time
    wait_time = this % wait_timer % get_total_time_ms()
  end function get_total_wait_time_ms

  function get_latest_wait_time_ms(this) result(wait_time)
    implicit none
    class(simple_mutex), intent(in) :: this
    real(C_DOUBLE) :: wait_time
    wait_time = this % wait_timer % get_latest_time_ms()
  end function get_latest_wait_time_ms
end module simple_mutex_module