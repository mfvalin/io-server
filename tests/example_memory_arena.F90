! Copyright (C) 2022  Environnement Canada
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

subroutine simple_test

  use ISO_C_BINDING
  implicit none

  integer, parameter :: NSYM=256
  integer, parameter :: DBLK=20

  include 'io-server/memory_arena.inc'
  include 'io-server/circular_buffer.inc'
  include 'mpif.h'

  interface
    function gethostid() result(id) BIND(C,name='gethostid')
      import :: C_LONG
      integer(C_LONG) :: id
    end function gethostid
    function sleep(delay) result(status) BIND(C,name='sleep')
      import :: C_INT
      integer(C_INT), intent(IN), value :: delay
      integer(C_INT) :: status
    end function sleep
  end interface

  integer :: err, rank, isiz, id, MY_World
  type(C_PTR) :: shmaddr, p, cio_from, cio_into, masteraddr
  integer(C_INT) :: shmid
  integer(C_INT64_T) :: shmsz64
  character(len=128) :: command, myblock
  integer :: bsz, flags, i, errors, j, bsza, nareas
  integer, dimension(:), pointer :: fp
  integer, dimension(128) :: message

  call mpi_init(err)
  call mpi_split_by_node(MPI_COMM_WORLD, MY_World, rank, isiz, err)

  id = memory_arena_set_id(rank) ! send my rank as id
  if(rank == 0) then             ! PE0 creates arena and some blocks
    shmsz64 = 1024 * 1024 * 512    ! 512 MBytes
!     shmaddr = memory_arena_create_shared(shmid, NSYM, shmsz64)
    masteraddr = master_arena_create_shared(shmid, NSYM, shmsz64)
    shmaddr = memory_arena_from_master(masteraddr);                   ! get memory arena address from master arena address
    nareas = update_local_table(masteraddr)
!     do id = 1, isiz
!       write(myblock,100)'BLOCK',id-1
100   format(A5,I3.3)
!       p = memory_block_create(shmaddr, DBLK*id, trim(myblock))
!     enddo
    write(command,*)'ipcs -m -u -i ',shmid
    call system(trim(command))       ! list shared memory blocks on system
    call memory_arena_print_status(shmaddr)  ! print arena metadata
  endif

  call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, MY_World, err)          ! broadcast id of shared memory segment

  if(rank .ne. 0) then
!     shmaddr = memory_arena_from_id(shmid)                        ! everybody but PE0 gets the segment address
!     shmaddr = memory_arena_from_master_id(shmid)                 ! everybody but PE0 gets the segment address
    masteraddr = memory_address_from_id(shmid)                     ! everybody but PE0 gets the master arena address
    shmaddr = memory_arena_from_master(masteraddr);                ! get memory arena address from master arena address
    nareas = update_local_table(masteraddr)
    write(myblock,100)'FROM-',rank
    p = memory_block_create(shmaddr, 8*1024, trim(myblock))        ! outbound circular buffer
    cio_from = CB_from_pointer(p, 8*1024)
    write(myblock,100)'INTO-',rank
    p = memory_block_create(shmaddr, 16*1024, trim(myblock))       ! inbound circular buffer
    cio_into = CB_from_pointer(p, 16*1024)
  else
    do id = 2, isiz                                                ! wait for block creation by other PEs
      write(myblock,100)'BLOCK',id-1
      p = memory_block_find_wait(shmaddr, bsz, flags, trim(myblock), 10000)
      write(0,*) trim(myblock)//' found, size =',bsz
      write(myblock,100)'FROM-',id-1
      p = memory_block_find_wait(shmaddr, bsz, flags, trim(myblock), 10000)
      bsza = CB_get_available_space(p)
      write(0,*) trim(myblock)//' found, available space =',bsza,' of',bsz
      write(myblock,100)'INTO-',id-1
      p = memory_block_find_wait(shmaddr, bsz, flags, trim(myblock), 10000)
      bsza = CB_get_available_space(p)
      write(0,*) trim(myblock)//' found, available space =',bsza,' of',bsz
    enddo
  endif

  if(rank == 0) then  ! send message to other inbound buffers
    do id = 2, isiz
      write(myblock,100)'INTO-',id-1
      p = memory_block_find(shmaddr, bsz, flags, trim(myblock))
      do i=1,64
        message(i) = i + ishft(id-1,24)
      enddo
      bsza = CB_get_available_space(p)
      bsz = CB_atomic_put(p, message, 64)
      write(0,*) trim(myblock)//' available space after =',bsz,' before =',bsza
    enddo
  else                  ! send traffic to my outbound buffer
    write(myblock,100)'FROM-',rank
    p = memory_block_find(shmaddr, bsz, flags, trim(myblock))
    do i=1,128
     message(i) = i + ishft(rank,24)
    enddo
    bsza = CB_get_available_space(p)
    bsz = CB_atomic_put(p, message, 128)
    write(0,*) trim(myblock)//' available space after =',bsz,' before = ',bsza
  endif

  write(myblock,100)'BLOCK',rank
  p = memory_block_create(shmaddr, DBLK*(rank+1), trim(myblock))   ! create MY block
  p = memory_block_find(shmaddr, bsz, flags, trim(myblock))        ! get MY block
  call c_f_pointer(p, fp, [bsz])                                   ! make Fortran pointer
  do i = 1, bsz                    ! fill array with marker including rank
    fp(i) = i + ishft(rank,24)
  enddo
  write(0,*) trim(myblock), ' created with',bsz,' values'
  p = memory_block_mark_init(shmaddr, trim(myblock))               ! mark block as initialized

!===============================================
  call MPI_Barrier(MY_World, err)
!===============================================

  if(rank == isiz -1 ) then        ! last PE prints  arena metadata
    write(command,*)'ipcs -m -u -i ',shmid
    call system(command)         ! list shared memory blocks on system
    call memory_arena_print_status(shmaddr)
  endif

!===============================================
  call MPI_Barrier(MY_World, err)
!===============================================

  if(rank == 0) then             ! PE0 checks everything
    write(0,*) '--------------------------------------------------------'
    do i = 1, isiz
      write(myblock,100)'BLOCK',i-1
      p = memory_block_find(shmaddr, bsz, flags, trim(myblock)) 
      call c_f_pointer(p, fp, [bsz])
      errors = 0
      do j = 1, bsz
        if(fp(j) .ne. j + ishft(i-1,24)) errors = errors + 1
      enddo
      write(0,*) trim(myblock), ' values =',bsz,' errors =',errors
    enddo
    write(0,*) '--------------------------------------------------------'
  endif

!===============================================
  call MPI_Barrier(MY_World, err)
!===============================================

  if(rank == 0) then             ! PE0 checks FROM- buffers
    do id = 2, isiz
     write(myblock,100)'FROM-',id-1
     p = memory_block_find(shmaddr, bsz, flags, trim(myblock))
     if(bsz < 1) write(0,*) trim(myblock),' ERROR'
     bsza = CB_get_available_data(p)
     message = 0
     errors = 0
     bsz = CB_atomic_get(p, message, 128)
     do i = 1,128
      if(message(i) .ne. (i + ishft(id-1,24)) ) errors = errors + 1
     enddo
     write(0,*) trim(myblock), ' outbound data =',bsza,' left =',bsz,' errors =',errors
    enddo
  else                           ! other PEs check their INTO- buffer
    write(myblock,100)'INTO-',rank
    p = memory_block_find(shmaddr, bsz, flags, trim(myblock))
    bsza = CB_get_available_data(p)
    message = 0
    bsz = CB_atomic_get(p, message, 64)
    errors = 0
    do i=1,64
     if(message(i) .ne. (i + ishft(rank,24)) ) errors = errors + 1
    enddo
    write(0,*) trim(myblock), ' inbound data =',bsza,' left =',bsz,' errors =',errors
  endif

!===============================================
  call MPI_Barrier(MY_World, err)
!===============================================

  write(0, *)'I am process',rank+1,' of',isiz,' on node'
  call mpi_finalize(err)
end subroutine


subroutine mpi_split_by_node(oldcomm, newcomm, rank, isiz, err)
  use ISO_C_BINDING
  implicit none
  integer, intent(IN)  :: oldcomm   ! MPI communicator to split on a host basis
  integer, intent(OUT) :: newcomm   ! newcommunicator to be used py PEs on same host
  integer, intent(OUT) :: rank      ! rank in new communicator
  integer, intent(OUT) :: isiz      ! size of new communicator
  integer, intent(OUT) :: err       ! error code
  include 'mpif.h'
  interface
    function gethostid() result(id) BIND(C,name='gethostid')
      import :: C_LONG
      integer(C_LONG) :: id
    end function gethostid
  end interface

  integer, parameter :: MAX_CACHE=16
  integer :: myhost, myhost0, myhost1, tmpcomm, i
  integer, save :: ncached = 0
  integer, dimension(MAX_CACHE) :: cold, cnew

  err = MPI_ERR_OTHER     ! precondition for failure
  rank = -1
  isiz = 0
  newcomm = MPI_COMM_NULL

  do i = 1, ncached
    if(cold(i) == oldcomm) newcomm = cnew(i)  ! cached entry found
  enddo

  if(newcomm == MPI_COMM_NULL) then          ! nothing useful found in cache
    call mpi_comm_rank(oldcomm, rank, err)
    myhost  = INT(gethostid())               ! host id
    myhost0 = iand(myhost , Z'7FFFFFFF')     ! lower 31 bits
    myhost1 = iand( ishft(myhost, -31) , 1 ) ! upper bit

    call MPI_Comm_split(oldcomm , myhost0, rank, tmpcomm, err)     ! split oldcomm using the lower 31 bits of host id , weight=rank in base
    if(err .ne. MPI_SUCCESS) return
    call MPI_Comm_split(tmpcomm ,myhost1, rank, newcomm, err)     ! re split using the upper bit of host id , weight=rank in base
    if(err .ne. MPI_SUCCESS) return
  endif

  if(ncached < MAX_CACHE) then                ! add to cache if cache is not full
    ncached = ncached + 1
    cold(ncached) = oldcomm
    cnew(ncached) = newcomm
  endif

  call MPI_Comm_rank(newcomm, rank,err);                         ! rank of this PE on this SMP node
  if(err .ne. MPI_SUCCESS) return
  call MPI_Comm_size(newcomm, isiz, err);                        ! number of PEs on this SMP node
  if(err .ne. MPI_SUCCESS) return
  
end subroutine mpi_split_by_node
