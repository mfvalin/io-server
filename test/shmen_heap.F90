! Copyright (C) 2020  Environnement Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020
!     V. Magnoux, Recherche en Prevision Numerique, 2020

!> \cond DOXYGEN_SHOULD_SKIP_THIS
#define NPTEST 125
#define MAXINDEXES  1024

program test_shmen_heap
  implicit none
  include 'mpif.h'
  integer :: myrank, nprocs, ierr

  myrank = 0
  nprocs = 1
  ! MPI multiprocess test
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
  print 3,'this is PE', myrank+1, ' of', nprocs

!   call base_test(nprocs, myrank)
  call relay_test(nprocs, myrank)

  call Mpi_Finalize(ierr)
  stop
3 format(2(A,I8))
end program

subroutine relay_test(nprocs, myrank)     ! simulate model PE to IO relay PE traffic
  use shmem_heap                          ! heap and block management functions
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: myrank, nprocs

  integer(HEAP_ELEMENT) :: he              ! only used for C_SIZEOF
  integer :: ierr, win, disp_unit, i, j, nheaps, status, array_rank, lastblock
  integer, dimension(:), allocatable :: disp_units
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, baseptr, mybase
  integer(KIND=MPI_ADDRESS_KIND), dimension(:), allocatable :: bases, sizes
  type, bind(C) :: mem_layout             ! shared memory layout description
    integer(C_INT) :: nindexes            ! size of index table
    type(C_PTR)    :: pindex              ! pointer to index table (nindexes elements)
    type(C_PTR)    :: pheap               ! pointer to heap
  end type mem_layout
  type(mem_layout) :: memory
  type(mem_layout), dimension(:), allocatable :: memories
  integer(C_INT), dimension(:), pointer :: ixtab   ! index table (integer array)
  integer(C_INT), dimension(:), pointer :: ram     ! shared memory (addressable as an integer array)
  type(heap)  :: h                        ! a managed heap
  type(heap), dimension(:), allocatable :: heaps   ! the heaps for "model" PEs
  type(C_PTR) :: p                        ! scratch variable
  integer(C_INT)    :: free_blocks, used_blocks
  integer(C_SIZE_T) :: free_space, used_space
  type(C_PTR), dimension(128) :: blocks   !  addresses of allocated memory blocks
  integer(C_INT), dimension(:,:,:), pointer :: demo    ! the array that will be allocated
  type(block_meta_f08) :: blk_meta                     ! metadata for allocated block (Fortran style with bound procedures)
  type(block_meta_f08), dimension(128) :: metas
  integer, dimension(MAX_ARRAY_RANK) :: ad             ! maximum size of dimensions array in metadata
  integer(C_LONG_LONG) :: sz64, max64, nblk64, nbyt64  ! to get heap stats

  print 3,'==================== RELAY TEST ===================='
  if(nprocs < 5) then
    print 3,'FAIL: too few processes for relay test, a minimum of 5 is required, got',nprocs
    return
  endif
! ================================= allocate shared memory =================================
  if(myrank == 0 .or. myrank == nprocs-1) then
    winsize = 0                                     ! relay PE, no space needed in local window
    print 3, 'relay PE, rank =',myrank
  else
    winsize = 4*1024*1024 + 1024*myrank             ! "model" PE, allocate space for PE heap
    print 3, 'model PE, rank =',myrank
  endif

  if( C_ASSOCIATED(h%ptr()) ) then                  ! check initialization clause in declaration
    print *,"FAIL: uninitialized heap C pointer is not NULL"
  else
    print *,"PASS: uninitialized heap C pointer is NULL"
  endif

  disp_unit = C_SIZEOF(he)                          ! displacement unit in window = size of a heap element
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, baseptr, win, ierr)
  allocate(bases(0:nprocs-1))
  allocate(sizes(0:nprocs-1))
  allocate(disp_units(0:nprocs-1))
  do i = 0, nprocs-1
    call MPI_Win_shared_query(win, i, sizes(i), disp_units(i), bases(i),   ierr)  ! get base addresses of partners
  enddo
  mybase = bases(myrank)  ! my own base address
  print 5,'INFO: PE ',myrank,', my base address = ',bases(myrank)
! ================================= setup of the "model" PE heaps =================================
  if(myrank == 0 .or. myrank == nprocs-1) then  ! relay PE (first rank and last rank)
    do i = 0, nprocs-1
      print 5,'INFO: PE ',i,', base address = ',bases(i),' size =',sizes(i),' disp_unit =',disp_units(i)
    enddo
  else                                          ! "model" PE, create and populate heaps
    p = transfer(mybase, C_NULL_PTR)                  ! make large integer from Win_shared_query into a C pointer
    call c_f_pointer(p, ram,[winsize/4])              ! Fortran array ram points to shared memory segment
    ram(1)          = MAXINDEXES + myrank             ! post index table size
    memory%nindexes = ram(1)                          ! get size of index table
    memory%pindex   = C_LOC(ram(2))                   ! pointer to local index table
    memory%pheap    = C_LOC(ram(memory%nindexes+2))   ! pointer to start of local heap
    call c_f_pointer(memory%pindex, ixtab, [memory%nindexes]) ! variable index now points to index table
    ixtab = -1                                        ! invalidate indexes
    p = memory%pheap                                  ! p points to the local heap
    p = h%create(p, (8192 + 8*myrank)*C_SIZEOF(he))   ! create heap h, 8 K + 8*rank elements
    if( C_ASSOCIATED(h%ptr()) ) print *,"PASS: heap is successfully initialized, C pointer is not NULL"
    !  ================================= allocate arrays in the heaps =================================
    blocks = C_NULL_PTR                               ! fill pointer table with NULLs
    lastblock = 0                                     ! no block successfully allocated
    do i = 1 , 2 + myrank * 2                         ! array allocation loop
      call h%allocate(demo, [(700+i*100+myrank*10)/10,2,5], blk_meta)    ! allocate a 3D integer array demo + get metadata
      metas(i) = blk_meta
      if((.not. (metas(i) == blk_meta)) .or. (metas(i) .ne. blk_meta)) print 6,'FAIL: failed equality check'
      print 6,'INFO: block type, kind, rank, dimensions :', blk_meta%t(), blk_meta%k(), blk_meta%r(), metas(i)%dims()
      if( .not. ASSOCIATED(demo) ) then               ! test returned fortran pointer
        print 6,'WARN: allocation failed for block',i       ! failure expected at some point for high rank PEs
        exit                                          ! exit loop as all subsequent allocations would fail (heap full)
      endif
      blocks(i) = C_LOC(demo(1,1,1))                  ! get address of allocated array if allocation succcedeed
      print 7,'INFO: block ',i,', allocated at index =',h%offset(blocks(i)),', shape :',shape(demo)
      call blk_meta%reset()                           ! nullify blk_meta
      status = blk_meta%meta(blocks(i))               ! get metadata for allocated array
      array_rank = blk_meta%r()                       ! get rank of array
      ad = blk_meta%dims()                            ! get all MAX_ARRAY_RANK potential dimensions
      print 6,'INFO: array dimensions fromn metadata=',ad(1:array_rank)   ! but only print the used ones
      ixtab(i) = h%offset(blocks(i))                  ! offset of blocks in heap
      lastblock = i
    enddo
    print 6,'INFO: ixtab =', ixtab(1:lastblock)             ! print used portion of index table
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait for heap creation and population to complete
! ================================= sanity check of the "model" PE heaps by "relay" PEs =================================
  if(myrank == 0 .or. myrank == nprocs-1) then             ! relay PE, scan heaps
    allocate(memories(1:nprocs-2))
    allocate(heaps(1:nprocs-2))
    do i = 1, nprocs-2                                     ! scan "model" PEs
      p = transfer(bases(i), C_NULL_PTR)                   ! get base memory address for PE of rank i
      call c_f_pointer(p, ram,[sizes(i)/C_SIZEOF(he)])     ! ram points to shared memory segment of a "model" PE
      memories(i)%nindexes = ram(1)                        ! size of index table for PE of rank i
      memories(i)%pindex   = C_LOC(ram(2))                 ! address of index table for PE of rank i
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])    ! Fortran array for index table
      memories(i)%pheap    = C_LOC(ram(memories(i)%nindexes+2))              ! heap base address for PE of rank i
      nheaps = heaps(i)%register(memories(i)%pheap)        ! register heap for PE of rank i
      print 3,'INFO: nheaps =',nheaps,', nindexes =',memories(i)%nindexes
      print 6,'INFO: ixtab =', ixtab(1:10)                       ! print index table for PE of rank i
      status = heaps(i)%check(free_blocks, free_space, used_blocks, used_space)   ! heap sanity check for PE of rank i
      print 2,free_blocks,' free block(s),',used_blocks,' used block(s)'  &
             ,free_space,' free bytes,',used_space,' bytes in use'
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait until all sanity check operations are completed
! ================================= free some blocks from the "model" PE heaps =================================
  if(myrank == 0) then                                ! PE 0 will now free some blocks
    do i = 1, nprocs-2                                ! loop over "model" PEs
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])   ! ixtab is Fortan array containing index table
      do j = 2, memories(i)%nindexes, 2
        if(ixtab(j) == -1) exit                       ! no more valid blocks
        status = heaps(i)%freebyoffset(ixtab(j))      ! free operation using index (offset) in heap
        print 3,'INFO: freeing heap',i,', block',j,', status =',status,   &
                ', offset =',heaps(i)%offset(heaps(i)%address(ixtab(j)))
      enddo
    enddo
  endif

  if(myrank == nprocs-1) then                         ! PE nprocs-1 will also free some blocks
    do i = 1, nprocs-2                                ! loop over "model" PEs
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])
      do j = 3, memories(i)%nindexes, 2
        if(ixtab(j) == -1) exit
        status = heaps(i)%freebyoffset(ixtab(j))
        print 3,'INFO: freeing heap',i,', block',j,', status =',status,   &
                ', offset =',heaps(i)%offset(heaps(i)%address(ixtab(j)))
      enddo
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait until all free operations are completed
! ================================= sanity check of the "model" PE heaps =================================
  if(myrank == 0 .or. myrank == nprocs-1) then        ! relay PE
    do i = 1, nprocs-2                                ! heap statistics
      status = heaps(i)%GetInfo(sz64, max64, nblk64, nbyt64)
      if(status == 0) print 8,"INFO: heap from PE",i, sz64, max64, nblk64, nbyt64
    enddo
    print *,""
  else                                                ! "model" PE, check what is left on heap
    status = h%check(free_blocks, free_space, used_blocks, used_space)
    print 2,free_blocks,' free block(s),',used_blocks,' used block(s)'  &
            ,free_space,' free bytes,',used_space,' bytes in use'
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait 

  call ShmemHeapDumpInfo()                            ! dump info for all known heaps

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait 

  if(myrank > 0 .and. myrank < nprocs-1) then         ! "model" PE
    do i = 0, 8
      status = ShmemHeapGetInfo(i, sz64, max64, nblk64, nbyt64)
      if(status == 0) print 8,"INFO: heap stats(1)",i, sz64, max64, nblk64, nbyt64
      status = h%GetInfoReg(i, sz64, max64, nblk64, nbyt64)
      if(status == 0) print 8,"INFO: heap stats(2)",i, sz64, max64, nblk64, nbyt64
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait 

  call MPI_Win_free(win, ierr)

  return
! 1 format(2(A,Z16.16))
2 format(4(I8,A))
3 format(4(A,I8))
5 format(A,I3,A,Z16.16,2(A,I8))
6 format(A,10I6)
7 format(A,I3,A,I8,A,10I6)
8 format(A,i3,4I10)
end subroutine relay_test

subroutine base_test(nprocs, myrank)
  use shmem_heap
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: myrank, nprocs

  integer :: ierr, win, disp_unit, i, status
  type, bind(C) :: mem_layout             ! shared memory layout description
    integer(C_INT) :: nindexes            ! size of index table
    type(C_PTR)    :: pindex              ! pointer to index table (nindexes elements)
    type(C_PTR)    :: pheap               ! pointer to heap
  end type mem_layout
  type(mem_layout) :: memory
  type(heap)  :: h
  type(C_PTR) :: p
  type(C_PTR), dimension(128) :: blocks   !  addresses of allocated memory blocks
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, baseptr, mybase, mysize
  integer(C_INT)    :: free_blocks, used_blocks
  integer(C_SIZE_T) :: free_space, used_space
  integer(C_INT), dimension(:), pointer :: ixtab   ! index table (integer array)
  integer(C_INT), dimension(:), pointer :: ram     ! shared memory (addressable as an integer array)
  integer(HEAP_ELEMENT) :: he              ! only used for C_SIZEOF purpose
  logical, parameter :: bugged = .false.
  integer(C_INT), dimension(:), pointer :: demo
  type(block_meta_f08) :: blk_meta

  print 3,'==================== BASIC TEST ===================='
  winsize = 1024*1024
  disp_unit = C_SIZEOF(he)                          ! size of a heap element
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, baseptr, win, ierr)
  call MPI_Win_shared_query(win, 0, mysize, disp_unit, mybase,   ierr)  ! get my base address

  p = transfer(mybase, C_NULL_PTR)
  call c_f_pointer(p, ram,[winsize/4])              ! ram points to shared memory segment
  if(myrank == 0) then
    ram(1) = MAXINDEXES                             ! post index table size
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)            ! all processes have access to ram(1)

  memory%nindexes = ram(1)                          ! get size of index table
  memory%pindex   = C_LOC(ram(2))                   ! pointer to index table
  memory%pheap    = C_LOC(ram(MAXINDEXES+2))        ! pointer to start of heap
  call c_f_pointer(memory%pindex, ixtab, [MAXINDEXES]) ! variable index now points to index table
  p = memory%pheap                                  ! p points to the heap

  if(bugged) then
    ixtab = -1                                      ! this is a bug (potential race condition)
  endif

  if(myrank == 0) then                              ! create, initialize, register the heap
    ixtab = -1
    p = h%create(p, 1024*8*C_SIZEOF(he))            ! create heap, 32 KBytes
    do i = 1, 10
!       blocks(i) = h%alloc((1022+i)*C_SIZEOF(he), 0)     ! attempt to allocate block
      call sm_allocate(h, demo, [1022+i], blk_meta)
      blocks(i) = C_LOC(demo(1))
      if( .not. C_ASSOCIATED(blocks(i)) ) then
        print *,'allocation failed for block',i
        exit
      endif
      ixtab(i) = h%offset(blocks(i))
      print *,'ixtab =',ixtab(i),h%offset(h%address(ixtab(i))),h%offset(blocks(i))  ! test of address and offset methods
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)            ! wait for heap creation and block allocation

  if(myrank .ne. 0) then
    do i = 1, MAXINDEXES
      if(ixtab(i) > 0 .and. myrank == nprocs - 1) print *,'ixtab',i,ixtab(i)
    enddo
    i = h%register(p)                               ! register heap on this process (other than process 0)
    print *,'process',myrank,', nheaps =',i
    print 1 , 'RAM address  :',loc(ram), ', HEAP address :',transfer(p,baseptr)
    status = h%check(free_blocks, free_space, used_blocks, used_space)
    print 2,free_blocks,' free block(s),',used_blocks,' used block(s)'  &
           ,free_space,' free bytes,',used_space,' bytes in use'
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)            ! allow check to complete for all processes before freeing blocks

  if(myrank .ne. 0) then
    do i = myrank, used_blocks-1,nprocs-1           ! each process will free some blocks
      if(ixtab(i) > 0) then                         ! block allocated previously by process 0
        status = h%freebyoffset(ixtab(i))
        print 3,'h%free status =',status,', offset =',h%offset(h%address(ixtab(i)))
        if(status == 0) ixtab(i) = 0                ! set index to free
      endif
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)            ! allow all free operations to complete

  status = h%check(free_blocks, free_space, used_blocks, used_space)
  print 2,free_blocks,' free block(s),',used_blocks,' used block(s)' &
         ,free_space,' free bytes,',used_space,' bytes in use'
  if(myrank == 0) then
    print 4, 'IXTAB(1:10) =',ixtab(1:10)
  endif

  call MPI_Win_free(win, ierr)

1 format(2(A,Z16.16))
2 format(4(I8,A))
3 format(2(A,I8))
4 format(A,20I8)
end subroutine base_test
!> \endcond
