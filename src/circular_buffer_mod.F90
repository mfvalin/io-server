!/* useful routines for C and FORTRAN programming
! * Copyright (C) 2020  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This software is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This software is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! */
! ====================================================
!> \file
!> \brief circular buffer Fortran module (object oriented)
module circular_buffers
  use ISO_C_BINDING
  implicit none
  include 'circular_buffer.inc'
  !> \brief circular_buffer user defined type
  type, public :: circular_buffer
    !> \private
    private
    type(C_PTR) :: p                    !< pointer to storage used by circular buffer
  contains
    !> \return pointer to created circular buffer
    procedure :: init                   !< initialize a circular buffer
    !> \return pointer to created circular buffer
    procedure :: create_local           !< create a circular buffer in local memory
    !> \return pointer to created circular buffer
    procedure :: create_shared          !< create a circular buffer in shared memory
    !> \return pointer to created circular buffer
    procedure :: create_from_pointer    !< create a circular buffer from user supplied memory
    !> \return pointer to created circular buffer
    procedure :: create_from_other      !< public creaator
    !> \return pointer to created circular buffer
    GENERIC   :: create => create_local, create_shared, create_from_pointer, create_from_other  !< generic create circular buffer
    !> \return 0 if success, -1 if error
    procedure :: detach_shared          !< detach shared memory segment used by circular buffer
    !> \return number of empty slots available, -1 on error
    procedure :: space_available        !< get number of empty slots available
    !> \return number of empty slots available, -1 on error
    procedure :: wait_space_available   !< wait until at least na empty slots
    !> \return number of empty slots available, -1 on error
    GENERIC   :: space => space_available, wait_space_available  !< generic wait for free space
    !> \return number of data tokens available, -1 if error
    procedure :: data_available         !< get current number of data tokens available
    !> \return number of data tokens available, -1 if error
    procedure :: wait_data_available    !< wait until at least na data tokens are available
    !> \return number of data tokens available, -1 if error
    GENERIC   :: data => data_available, wait_data_available     !< generic wait for available data
    !> \return address of the beginning of the buffer
    procedure :: buffer_start           !< get address of the beginning ot the buffer
    !> \return address of the insertion point
    procedure :: data_in                !< get address of the insertion point
    !> \return address of the extraction point
    procedure :: data_out               !< get address of the extraction point
    !> \return pointer to the "in" position
    procedure :: advance_in             !< get pointer to the in position, and space available
    !> \return pointer to the "out" position
    procedure :: advance_out            !< get pointer to the out position, and data available
    !> \return number of data tokens available after this operation, -1 if error
    procedure :: atomic_get             !< wait until enough data is available then extract data
    !> \return number of free slots available after this operation, -1 if error
    procedure :: atomic_put             !< wait until enough free slots are available then insert data
  end type circular_buffer
contains
  !> \brief initialize a circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%init(nwords)
  function init(cb, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_init(cb%p, nwords)
    p = cb%p
  end function init 
  !> \brief create a circular buffer in local memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_local(nwords)
  !> <br>p = cb\%create(nwords)
  function create_local(cb, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create(nwords)
    p = cb%p
  end function create_local
  !> \brief create a circular buffer in shared memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_shared(shmid, nwords)
  !> <br>p = cb\%create(shmid, nwords)
  function create_shared(cb, shmid, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT) :: shmid                    !< identifier of shared memory area (see man shmget)
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_create_shared(shmid, nwords)
    p = cb%p
   end function create_shared
  !> \brief create a circular buffer from user supplied memory
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_pointer(ptr, nwords)
  !> <br>p = cb\%create(ptr, nwords)
  function create_from_pointer(cb, ptr, nwords) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR), intent(IN), value :: ptr                   !< pointer to user supplied memory
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the circular buffer
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = circular_buffer_from_pointer(ptr, nwords)
    p = cb%p
  end function create_from_pointer
  !> \brief create a circular buffer from address of another circular buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: p<br>
  !> p = cb\%create_from_other(ptr)
  !> <br>p = cb\%create(ptr)
  function create_from_other(cb, ptr) result(p)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR), intent(IN), value :: ptr                   !< pointer to user supplied memory
    type(C_PTR) :: p                                        !< pointer to created circular buffer 
    cb%p = ptr
    p = cb%p
  end function create_from_other
  
  !> \brief detach shared memory segment used by circular buffer 
  !> <br>type(circular_buffer) :: cb<br>integer :: status<br>
  !> status = cb\%detach_shared()
  function detach_shared(cb) result(status)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: status                                !< 0 if success, -1 if error
    status = circular_buffer_detach_shared(cb%p)
  end function detach_shared
  !> \brief get number of empty slots available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%space_available()
  !> <br>n = cb\%space()
  function space_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of empty slots available, -1 if error
    n = circular_buffer_space_available(cb%p)
  end function space_available
  !> \brief wait until at least na empty slots are available for inserting data
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%wait_space_available(na)
  !> <br>n = cb\%space(na)
  function wait_space_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available slots
    integer(C_INT) :: n                                     !< number of empty slots available, -1 on error
    n = circular_buffer_wait_space_available(cb%p, na)
  end function wait_space_available
  !> \brief get current number of data tokens available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%data_available()
  !> <br>n = cb\%data()
  function data_available(cb) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = circular_buffer_data_available(cb%p)
  end function data_available
  !> \brief wait until at least na data tokens are available
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%wait_data_available(na)
  !> <br>n = cb\%data(na)
  function wait_data_available(cb, na) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: na                 !< needed number of available tokens
    integer(C_INT) :: n                                     !< number of data tokens available, -1 if error
    n = circular_buffer_wait_data_available(cb%p, na)
  end function wait_data_available
  !> \brief get address of the beginning of the buffer
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: start<br>
  !> start = cb\%buffer_start()
  function buffer_start(cb) result(start)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: start                                    !< address of the start of the circular data buffer
    start = circular_buffer_start(cb%p)
  end function buffer_start
  !> \brief get address of the insertion point
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: inp<br>
  !> inp = cb\%data_in()
  function data_in(cb) result(inp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: inp                                      !< address of the insertion point in the circular data buffer
    inp = circular_buffer_data_in(cb%p)
  end function data_in
  !> \brief get address of the extraction point
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: outp<br>
  !> outp = cb\%data_out()
  function data_out(cb) result(outp)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    type(C_PTR) :: outp                                     !< address of the extraction point in the circular data buffer
    outp = circular_buffer_data_out(cb%p)
  end function data_out
  !> \brief get pointer to the in position
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: inp<br>
  !> inp = cb\%advance_in(n1, n2)
  function advance_in(cb, n1, n2) result(inp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of slots available at the "in" position, -1 if error
    integer(C_INT), intent(OUT)    :: n2                    !< number of slots available at the "start" of the buffer, -1 if error
    type(C_PTR)                    :: inp                   !< pointer to the "in" position, C_NULL_PTR if error
    inp = circular_buffer_advance_in(cb%p, n1, n2)
  end function advance_in
  !> \brief get pointer to the "out" position
  !> <br>type(circular_buffer) :: cb<br>type(C_PTR) :: outp<br>
  !> outp = cb\%advance_out(n1, n2)
  function advance_out(cb, n1, n2) result(outp)
                                                            !< assume that the caller knows the start of data buffer
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(OUT)    :: n1                    !< number of tokens available at the "out" position, -1 if error
    integer(C_INT), intent(OUT)    :: n2                    !< number of tokens available at the "start" of the buffer, -1 if error
    type(C_PTR)                    :: outp                  !< pointer to the "out" position, C_NULL_PTR if error
    outp = circular_buffer_advance_out(cb%p, n1, n2)
  end function advance_out
  !> \brief wait until ndst tokens are available then extract them into dst
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_get(dst, ndst)
  function atomic_get(cb, dst, ndst) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: ndst               !< number of tokens to extract
    integer(C_INT), dimension(*), intent(OUT) :: dst        !< destination array to receive extracted data
    integer(C_INT) :: n                                     !< number of data tokens available after this operation, -1 if error
    n = circular_buffer_atomic_get(cb%p, dst, ndst)
  end function atomic_get
  !> \brief wait until nsrc free slots are available then insert from src array
  !> <br>type(circular_buffer) :: cb<br>integer :: n<br>
  !> n = cb\%atomic_put(src, nsrc)
  function atomic_put(cb, src, nsrc) result(n)
    implicit none
    class(circular_buffer), intent(INOUT) :: cb             !< circular_buffer
    integer(C_INT), intent(IN), value :: nsrc               !< number of tokens to insert from src
    integer(C_INT), dimension(*), intent(IN) :: src         !< source array for data insertion
    integer(C_INT) :: n                                     !< number of free slots available after this operation, -1 if error
    n = circular_buffer_atomic_put(cb%p, src, nsrc)
  end function atomic_put

end module circular_buffers

#ifndef DOXYGEN_SHOULD_SKIP_THIS

#if defined(SLEF_TEST)

#define NPTEST 125
program demo
  use circular_buffers
  implicit none
  include 'mpif.h'

  type(circular_buffer) :: a, b, c, d, e, f
  integer :: shmid, n, status, n1, n2
  type(C_PTR) :: p, q, r, s, t, x
  integer, dimension(256), target :: local, local2, cbuf
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, baseptr, sendbase, mybase, mysize, tosize
  integer, dimension(:), pointer :: cb

  integer :: myrank, nprocs, ierr, win, disp_unit, sendto, getfrom, i, errors, navail, navail2

  myrank = 0
  nprocs = 1
  ! MPI multiprocess test
#if ! defined(SINGLE)
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
  print *,'this is PE', myrank+1, ' of', nprocs

  winsize = 1024*1024
  disp_unit = 4
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, baseptr, win, ierr)
  sendto = mod(myrank+1, nprocs)           ! next in the "ring"
  getfrom = mod(nprocs+myrank-1, nprocs)   ! previous in the "ring"
  call MPI_Win_shared_query(win, myrank, mysize, disp_unit, mybase,   ierr)  ! get my base address
  call MPI_Win_shared_query(win, sendto, tosize, disp_unit, sendbase, ierr)  ! get my victim's base address
  print *,'win =',win,' to, from', sendto, getfrom
  print 1, 'my base =', mybase, ' target base =',sendbase,' delta =',abs(sendbase-mybase)
1 format(3(A,2X,Z16.16))

  do i = 1, size(local)
    local(i) = myrank*1000 + 1000 + i
  enddo
  local2 = 0

  p = transfer(mybase,   C_NULL_PTR)  !  pointer to my circular buffer
  q = transfer(sendbase, C_NULL_PTR)  !  pointer to my target's circular buffer
  p = a%create(p, 128)                !  create my circular buffer
  q = b%create(q)                     !  point to target's circular buffer
  call C_F_POINTER(p, cb, [128])      !  array cb points to my circular buffer
  print 2,'CB :',cb(1:15)             ! initial state of circular buffer
2 format(A,20I8)

  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  n = b%atomic_put(local, 5)             ! inject data into target's circular buffer
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  n = a%atomic_get(local2, 4)            ! get from my own buffer
  print 2,'CB-0 :',cb(1:15)
  print 2,'Got-0:',local2(1:10)
  n = a%atomic_get(local2(5), 1)         ! get from my own buffer (remainder of what was put)
  print 2,'CB-1 :',cb(1:15)
  print 2,'Got-1:',local2(1:10)
  call MPI_Barrier(MPI_COMM_WORLD, ierr) ! we want no interference from further down

  do i = 1, NPTEST, 5                    ! ring test with wraparound , make sure NPTEST > size of circular buffer
    if(myrank == 0) then
      n = b%atomic_put(local(i) , 5)     ! send to next in ring
      n = a%atomic_get(local2(i), 5)     ! then get from previous in ring
    else
      n = a%atomic_get(local(i) , 5)     ! get from previous in ring
      n = b%atomic_put(local(i) , 5)     ! pass to next in ring
    endif
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
  enddo
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  print 2,'RING :',cb(1:15)
  if(myrank == 0) then  ! check that we got back what we sent
    errors = 0
    do i = 1, NPTEST
      if(local(i) .ne. local2(i)) errors = errors + 1
    enddo
    print 2,'RING errors :',errors
    if(errors > 0)print 3,local2(1:NPTEST)
3   format(25I5)
  endif

  if(myrank == -1) then ! never true, what follows is only a syntax check for OO implementation
    p = b%create(128000)
    p = b%init(128000)
    s = c%create_shared(shmid, 128000)
    q = d%create(128000)                  ! generic call
    r = d%create(shmid, 128000)           ! generic call
    t = d%create(C_LOC(local), 128000)    ! generic call
    status = c%detach_shared()
    status = b%space_available()
    n = b%space_available()
    n = b%space()
    n = b%wait_space_available(256)
    n = b%space()                    ! generic call
    n = b%space(256)                 ! generic call
    n = b%data_available()
    n = b%wait_data_available(256)
    n = b%data()                     ! generic call
    n = b%data(256)                  ! generic call
    x = b%buffer_start()
    x = b%data_in()
    x = b%data_out()
    x = b%advance_in(n1, n2)
    x = b%advance_out(n1, n2)
    n = b%atomic_get(local(100), 18)
    n = b%atomic_put(local(20), 45)
  endif

9 continue
  call MPI_Win_free(win, ierr)
  call mpi_finalize(ierr)
#else
  ! single thread test
  do i = 1, size(local)
    local(i) = myrank*1000 + 1000 + i
  enddo
  local2 = 0
  p = a%create(C_LOC(cbuf), 128)
  call C_F_POINTER(p, cb, [128])
  print 2,'CB :',cb(1:5)
2 format(A,20I8)
  do i = 1, NPTEST, 5
    n = a%atomic_put(local(i), 5)
    navail = a%data_available()
    print 2,'after put',i,navail,cb(1:5)
    n = a%atomic_get(local2(i), 5)
    navail = a%data_available()
    print 2,'after get',i,navail,cb(1:5),local2(i:i+4)
  enddo
#endif
end program demo
#endif
#endif
