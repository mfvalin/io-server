!  functions for C and FORTRAN programming
!  Copyright (C) 2021  Recherche en Prevision Numerique
! 
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
! 
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
module ioserver_functions
  use shmem_heap
  use circular_buffer_module
  implicit none
  include 'io-server/ioserver.inc'

  save

  public :: server_file
  type :: server_file
    private
    integer :: fd = -1
    character(len=1), dimension(:), pointer :: name => NULL()
    type(heap) :: h

    contains

    procedure   :: open => ioserver_open
    procedure   :: read => ioserver_read
    procedure   :: write => ioserver_write
    procedure   :: close => ioserver_close
  end type

  type, public :: grid
    integer :: id            ! grid id
    integer :: gni, gnj      ! horizontal dimensions of full grid
  end type

  type, public :: subgrid
    integer :: i0            ! starting point of this subgrid in the x direction
    integer :: ni            ! number of points of this subgrid in the x direction
    integer :: j0            ! starting point of this subgrid in the y direction
    integer :: nj            ! number of points of this subgrid in the y direction
    integer :: nk            ! number of vertical levels
    integer :: nv            ! number of variablethiss
  end type

  type, private :: data_header        ! record : data_header , metadata(nm integers) , subgrid(ni * nj * nk * nv elements)
    integer :: nw            ! number of elements in record
    integer :: nbits         ! number of bits per subgrid element in record
    integer :: tag           ! unique sequence tag
    integer :: weight        ! number of pieces in parcel (normally 1)
    integer :: np            ! number of pieces to reassemble
    integer :: grid          ! grid id
    type(subgrid) :: s       ! subgrid description
    integer :: nm            ! length of metadata "jar" (32 bit units)
  end type

  private  :: fd_seq, local_heap, cio_in, cio_out, initialized    ! , gdt, MAXGRIDS

  logical                :: initialized = .false.
  integer                :: fd_seq = 0
  type(heap)             :: local_heap   ! type is self initializing
  type(circular_buffer)  :: cio_in       ! type is self initializing
  type(circular_buffer)  :: cio_out      ! type is self initializing

!   integer, parameter :: MAXGRIDS = 1024
!   type(grid), dimension(MAXGRIDS) :: gdt   ! grid description table
 contains

  function ioserver_heap(n) result(h)
    implicit none
    integer, intent(IN) :: n
    type(heap) :: h
    type(heap) :: nh
    if(n == 0) then
      if(.not. C_ASSOCIATED(local_heap % ptr()) ) then
        print *,'initializing io relay heap'
        local_heap = IOserver_get_heap()
      endif
      h = local_heap
    else
      h = nh                  ! null heap
    endif
  end function ioserver_heap

  function ioserver_init(nio_node, app_class) result(status)
    implicit none
    integer, intent(IN)  :: nio_node     ! number of relay processes per compute node
    character(len=*), intent(IN) :: app_class
    integer :: status
    integer :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
    integer :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
    integer :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
    integer :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
    integer :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
    integer :: nodecom      ! communicator for io server PEs on a node   (may be MPI_COMM_NULL)
    status = ioserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, app_class)
    if(.not. initialized) then
      local_heap  = IOserver_get_heap()
      cio_in      = IOserver_get_cio_in()
      cio_out     = IOserver_get_cio_out()
      initialized = .true.
      print *,'initializing io heap and circular buffers'
    endif
  end function ioserver_init

  function ioserver_finalize() result(status)
  integer :: status
    status = ioserver_int_finalize()
  end function ioserver_finalize

  function ioserver_open(f, name) result(status)
    implicit none
    class(server_file), intent(INOUT) :: f
    character(len=*), intent(IN) :: name
    integer :: status
    integer :: lname
    type(C_PTR) :: p

    status = -1
    if(f % fd > 0)           return    ! already open
    if(associated(f % name)) return    ! already open

    fd_seq = fd_seq + 1
    lname = len(trim(name))
    allocate(f % name(lname))
    f % name(1:lname) = transfer(trim(name), f % name)
print *,"'",f % name(1:lname),"'"

    if(.not. C_ASSOCIATED(local_heap % ptr()) ) then
      print *,'locating local heap'
    endif
    f % h = local_heap                 ! associate heap to file
p = f % h % ptr()
if(C_ASSOCIATED(p)) then 
print *,' p is defined'
else
print *,' p is NULL'
endif

    status = 0
  end function ioserver_open

  function ioserver_close(f) result(status)
    implicit none
    class(server_file), intent(INOUT) :: f
    integer :: status

    status = -1
    if(f % fd <= 0)                return    ! not open
    if(.not. associated(f % name)) return    ! not open

    fd_seq = fd_seq + 1
    f % fd = -1
    deallocate(f % name)
    status = 0
  end function ioserver_close

  function ioserver_write(f) result(status)
    implicit none
    class(server_file), intent(INOUT) :: f
    integer :: status

    status = -1
    if(f % fd <= 0) return
    fd_seq = fd_seq + 1

    status = 0
  end function ioserver_write

  function ioserver_read(f) result(status)
    implicit none
    class(server_file), intent(INOUT) :: f
    integer :: status

    status = -1
    if(f % fd <= 0) return
    fd_seq = fd_seq + 1

    status = 0
  end function ioserver_read

end module

subroutine ioserver_functions_demo
  use ioserver_functions
  implicit none
  type(server_file) :: f
  integer :: status
  type(heap) :: h

#if defined(WITH_ERRORS)
  print *, fd_seq               ! this line must not compile successfully (private reference)
  print *, f % fd               ! this line must not compile successfully (private reference)
#endif
  status = ioserver_init(0, 'M')
  h = ioserver_heap(0)
  status = f % open('my/file/path')
  status = f % close()
  status = ioserver_finalize()
end subroutine ioserver_functions_demo
