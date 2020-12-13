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
  use shmem_heap
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: myrank, nprocs

  integer(HEAP_ELEMENT) :: he              ! only used for C_SIZEOF purpose
  integer :: ierr, win, disp_unit, i, j, nheaps, status
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
  type(heap)  :: h
  type(heap), dimension(:), allocatable :: heaps
  type(C_PTR) :: p
  integer(C_INT)    :: free_blocks, used_blocks
  integer(C_SIZE_T) :: free_space, used_space
  type(C_PTR), dimension(128) :: blocks   !  addresses of allocated memory blocks
  integer(C_INT), dimension(:,:,:), pointer :: demo

  print 3,'==================== RELAY TEST ===================='
  if(nprocs < 5) then
    print 3,'too few processes for relay test, need at least 5, got',nprocs
    return
  endif
  if(myrank == 0 .or. myrank == nprocs-1) then
    winsize = 0                                ! relay PE
    print 3, 'relay PE, rank =',myrank
  else
    winsize = 4*1024*1024 + 1024*myrank                     ! "model" PE
    print 3, 'model PE, rank =',myrank
  endif
  disp_unit = C_SIZEOF(he)                          ! size of a heap element
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, baseptr, win, ierr)
  allocate(bases(0:nprocs-1))
  allocate(sizes(0:nprocs-1))
  allocate(disp_units(0:nprocs-1))
  do i = 0, nprocs-1
    call MPI_Win_shared_query(win, i, sizes(i), disp_units(i), bases(i),   ierr)  ! get base address of partners
  enddo
  mybase = bases(myrank)  ! get my base address
  print 5,'PE ',myrank,', my base address = ',bases(myrank)
  
  if(myrank == 0 .or. myrank == nprocs-1) then  ! relay PE
    do i = 0, nprocs-1
      print 5,'PE ',i,', base address = ',bases(i),' size =',sizes(i),' disp_unit =',disp_units(i)
    enddo
  else                                          ! "model" PE, create heaps
    p = transfer(mybase, C_NULL_PTR)
    call c_f_pointer(p, ram,[winsize/4])              ! ram points to shared memory segment
    ram(1)          = MAXINDEXES + myrank             ! post index table size
    memory%nindexes = ram(1)                          ! get size of index table
    memory%pindex   = C_LOC(ram(2))                   ! pointer to local index table
    memory%pheap    = C_LOC(ram(memory%nindexes+2))   ! pointer to start of local heap
    call c_f_pointer(memory%pindex, ixtab, [memory%nindexes]) ! variable index now points to index table
    ixtab = -1                                        ! invalidate indexes
    p = memory%pheap                                  ! p points to the local heap
    p = h%create(p, (8192 + 8*myrank)*C_SIZEOF(he)) ! create heap, 8 K + 128*rank elements
    blocks = C_NULL_PTR
    do i = 1 , 2 + myrank * 2
!       blocks(i) = h%alloc((1100+i*10+myrank)*C_SIZEOF(he), 0)     ! try to allocate block
!       call sm_allocate(h, demo, [1100+i*10+myrank])
      call h%allocate(demo, [(700+i*100+myrank*10)/10,2,5])    ! 3D integer array
      blocks(i) = C_LOC(demo(1,1,1))

      if( .not. C_ASSOCIATED(blocks(i)) ) then
        print *,'allocation failed for block',i
        exit
      endif
      print 7,'block ',i,', allocated at index =',h%offset(blocks(i)),', shape :',shape(demo)
      ixtab(i) = h%offset(blocks(i))
    enddo
    print 6,'ixtab =', ixtab(1:10)
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait for heap creation to be complete

  if(myrank == 0 .or. myrank == nprocs-1) then                                ! relay PE, scan for heaps
    allocate(memories(1:nprocs-2))
    allocate(heaps(1:nprocs-2))
    do i = 1, nprocs-2                                ! scan "model" PEs
      p = transfer(bases(i), C_NULL_PTR)
      call c_f_pointer(p, ram,[sizes(i)/C_SIZEOF(he)])     ! ram points to shared memory segment of a "model" PE
      memories(i)%nindexes = ram(1)
      memories(i)%pindex   = C_LOC(ram(2))
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])
      memories(i)%pheap    = C_LOC(ram(memories(i)%nindexes+2))
      nheaps = heaps(i)%register(memories(i)%pheap)
      print 3,'nheaps =',nheaps,', nindexes =',memories(i)%nindexes
      print 6,'ixtab =', ixtab(1:10)
      status = heaps(i)%check(free_blocks, free_space, used_blocks, used_space)
      print 2,free_blocks,' free block(s),',used_blocks,' used block(s)'  &
             ,free_space,' free bytes,',used_space,' bytes in use'
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait until all check operations are completed

  if(myrank == 0) then                                ! PE 0 will free some blocks
    do i = 1, nprocs-2
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])
      do j = 2, memories(i)%nindexes, 2
        if(ixtab(j) == -1) exit
        status = heaps(i)%freebyoffset(ixtab(j))
        print 3,'freeing heap',i,', block',j,', status =',status,   &
                ', offset =',heaps(i)%offset(heaps(i)%address(ixtab(j)))
      enddo
    enddo
  endif

  if(myrank == nprocs-1) then                         ! PE nprocs-1 will free some blocks
    do i = 1, nprocs-2
      call c_f_pointer(memories(i)%pindex, ixtab, [memories(i)%nindexes])
      do j = 3, memories(i)%nindexes, 2
        if(ixtab(j) == -1) exit
        status = heaps(i)%freebyoffset(ixtab(j))
        print 3,'freeing heap',i,', block',j,', status =',status,   &
                ', offset =',heaps(i)%offset(heaps(i)%address(ixtab(j)))
      enddo
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait until all free operations are completed

  if(myrank == 0 .or. myrank == nprocs-1) then        ! relay PE
  else                                                ! "model" PE
    status = h%check(free_blocks, free_space, used_blocks, used_space)
    print 2,free_blocks,' free block(s),',used_blocks,' used block(s)'  &
            ,free_space,' free bytes,',used_space,' bytes in use'
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait 

  call MPI_Barrier(MPI_COMM_WORLD, ierr)              ! wait 

  call MPI_Win_free(win, ierr)

  return
! 1 format(2(A,Z16.16))
2 format(4(I8,A))
3 format(4(A,I8))
5 format(A,I3,A,Z16.16,2(A,I8))
6 format(A,10I6)
7 format(A,I3,A,I8,A,10I6)
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
      call sm_allocate(h, demo, [1022+i])
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
