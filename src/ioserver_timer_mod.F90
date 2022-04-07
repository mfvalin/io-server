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

module ioserver_timer_module
  use ISO_C_BINDING
  implicit none
  include 'io-server/timer.inc'

  private
  
  type, public :: ioserver_timer
    private
      type(C_PTR) :: c_timer = C_NULL_PTR !< Pointer to the C struct containing the timer
    contains
      procedure :: create
      procedure :: delete
      procedure :: start
      procedure :: stop
      procedure :: get_time_ms
  end type ioserver_timer

  public :: rdtscp, rdtsc

contains

  !> See IO_timer_create
  subroutine create(this)
    implicit none
    class(ioserver_timer), intent(inout) :: this
    this % c_timer = IO_timer_create()
  end subroutine create

  !> See IO_timer_delete
  subroutine delete(this)
    implicit none
    class(ioserver_timer), intent(inout) :: this
    call IO_timer_delete(this % c_timer)
  end subroutine delete

  !> See IO_timer_start
  subroutine start(this)
    implicit none
    class(ioserver_timer), intent(inout) :: this
    call IO_timer_start(this % c_timer)
  end subroutine start

  !> See IO_timer_stop
  subroutine stop(this)
    implicit none
    class(ioserver_timer), intent(inout) :: this
    call IO_timer_stop(this % c_timer)
  end subroutine stop

  !> See IO_time_ms
  function get_time_ms(this) result(time)
    implicit none
    class(ioserver_timer), intent(inout) :: this
    real(C_DOUBLE) :: time
    time = IO_time_ms(this % c_timer)
  end function get_time_ms

end module ioserver_timer_module
