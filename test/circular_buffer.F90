! Copyright (C) 2020 Recherche en Prevision Numerique
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation,
! version 2 of the License.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this program; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
program test_circular_buffer

  use ISO_C_BINDING
  use circular_buffer_module, only : circular_buffer, DATA_ELEMENT
  implicit none

  integer, parameter :: NUM_BUFFER_ELEMENTS = 10000
  integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: NPTEST = 125

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
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
  print *, 'This is PE', myrank + 1, ' of', nprocs

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

  call MPI_Win_free(win, ierr)
  call MPI_Finalize(ierr)


end program test_circular_buffer
