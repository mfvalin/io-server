!> \file
!> \brief shared memory heap Fortran module (object oriented)
module shmem_heap
  use ISO_C_BINDING
  !> \brief heap user defined type
  type, public :: heap
    !> \private
    private
    type(C_PTR) :: p                    !< address of storage used by heap
  contains

    !> \return                          address of heap
    procedure :: create                 !< create, initialize, register a heap at specified address

    !> \return                          address of heap
    procedure :: clone                  !< clone a heap object using the address of an existing heap

    !> \return                          address of heap
    procedure :: register               !< register an existing heap at specified address

    !> \return                          0 if O.K., nonzero if not
    procedure :: check                  !< heap integrity check

    !> \return                          address of heap (NULL if not in a registered heap)
    procedure, NOPASS :: inheap         !< find if address belongs to a registered heap

    !> \return                          offset in heap (-1 if not in a registered heap)
    procedure, NOPASS :: offset         !< translate address to offset in heap

    !> \return                          address from offset in heap, NULL if not a heap
    procedure :: address                !< translate offset in heap into actual address

    !> \return                          0 if valid block, -1 unknown heap, 1 not block pointer
    procedure, NOPASS :: validblock     !< find if address belongs to a registered heap

    !> \return                          block size marker if valid block, -1 unknown heap, 1 not block pointer
    procedure, NOPASS :: blockcode     !< get block size (elements) of a heap block (negative if block in use)

    !> \return                          block size marker if valid block, -1 unknown heap, 1 not block pointer
    procedure, NOPASS :: blocksize     !< get block size (elements) of a heap block (negative if block in use)

    !> \return                          size of heap in bytes , -1 if error
    procedure :: heapsize              !< get size of a known heap

    !> \return                          block address, NULL if allocation fails
    procedure :: alloc                  !< allocate a block in a registered heap

    !> \return                          0 if O.K., nonzero if error
    procedure, NOPASS :: free           !< free an allocated block by address in memory

    !> \return                           0 if O.K., nonzero if error
    procedure :: freebyoffset           !< free space associated to offset into heap

!> \cond DOXYGEN_SHOULD_SKIP_THIS
    !> \return                           a fortran pointer
    procedure   ::            &  !< specific procedures needed for generic type associated allocate
                              I1_5D, I1_4D, I1_3D, I1_2D, I1_1D, &
                              I2_5D, I2_4D, I2_3D, I2_2D, I2_1D, &
                              I4_5D, I4_4D, I4_3D, I4_2D, I4_1D, &
                              I8_5D, I8_4D, I8_3D, I8_2D, I8_1D, &
                              R4_5D, R4_4D, R4_3D, R4_2D, R4_1D, &
                              R8_5D, R8_4D, R8_3D, R8_2D, R8_1D 
!> \endcond
    !> \return     a fortran pointer to 1 - 5 dimensional integer and real arrays (see f_alloc.inc)
    GENERIC   :: allocate =>  &
                              I1_5D, I1_4D, I1_3D, I1_2D, I1_1D, &
                              I2_5D, I2_4D, I2_3D, I2_2D, I2_1D, &
                              I4_5D, I4_4D, I4_3D, I4_2D, I4_1D, &
                              I8_5D, I8_4D, I8_3D, I8_2D, I8_1D, &
                              R4_5D, R4_4D, R4_3D, R4_2D, R4_1D, &
                              R8_5D, R8_4D, R8_3D, R8_2D, R8_1D       !< generic Fortran array type associated allocatior
  end type heap

  include 'io-server/common.inc'

! type of a heap element (must be consistent with io-server definition)
  integer, parameter :: HEAP_ELEMENT =  DATA_ELEMENT  !<  type of a heap element (must be consistent with C code)

! tell doxygen to ignore the following block (for now)
!> \cond DOXYGEN_SHOULD_SKIP_THIS
  interface sm_allocate   ! generic procedure
    module procedure I1_5D, I1_4D, I1_3D, I1_2D, I1_1D, &
                     I2_5D, I2_4D, I2_3D, I2_2D, I2_1D, &
                     I4_5D, I4_4D, I4_3D, I4_2D, I4_1D, &
                     I8_5D, I8_4D, I8_3D, I8_2D, I8_1D, &
                     R4_5D, R4_4D, R4_3D, R4_2D, R4_1D, &
                     R8_5D, R8_4D, R8_3D, R8_2D, R8_1D
  end interface
  
  interface
    function malloc(sz) result(p) BIND(C,name='malloc')
      import :: C_PTR, C_SIZE_T
      integer(C_SIZE_T), value :: sz
      type(C_PTR) :: p
    end function malloc

    function ShmemHeapInit(heap, nbytes) result(h) bind(C,name='ShmemHeapInit')
      import :: C_PTR, C_SIZE_T
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_SIZE_T), intent(IN), value :: nbytes
      type(C_PTR) :: h
    end function ShmemHeapInit

    function ShmemHeapCheck(heap, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeapCheck')
      import :: C_INT, C_PTR, C_SIZE_T
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
      integer(C_SIZE_T), intent(OUT) :: free_space, used_space
      integer(C_INT) :: status
    end function ShmemHeapCheck

    function ShmemHeapAllocBlock(heap, nbytes, safe) result(b) bind(C,name='ShmemHeapAllocBlock')
      import :: C_INT, C_PTR, C_SIZE_T
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_SIZE_T), intent(IN), value :: nbytes
      integer(C_INT), intent(IN), value :: safe
      type(C_PTR) :: b
    end function ShmemHeapAllocBlock

    function ShmemHeapFreeBlock(block) result(status) bind(C,name='ShmemHeapFreeBlock')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: block
      integer(C_INT) :: status
    end function ShmemHeapFreeBlock

    function ShmemHeapRegister(heap) result(status) bind(C,name='ShmemHeapRegister')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_INT) :: status
    end function ShmemHeapRegister

    function ShmemHeapContains(addr) result(p) bind(C,name='ShmemHeapContains')
      import :: C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      type(C_PTR) :: p
    end function ShmemHeapContains

    function ShmemHeapSize(heap) result(s) bind(C,name='ShmemHeapSize')
      import :: C_PTR, HEAP_ELEMENT
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(HEAP_ELEMENT) :: s
    end function ShmemHeapSize

    function ShmemHeapValidBlock(addr) result(status) bind(C,name='ShmemHeapValidBlock')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(C_INT) :: status
    end function ShmemHeapValidBlock

    function ShmemHeapBlockSizeCode(addr) result(bsz) bind(C,name='ShmemHeapBlockSizeCode')
      import :: C_PTR, HEAP_ELEMENT
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(HEAP_ELEMENT) :: bsz
    end function ShmemHeapBlockSizeCode

    function ShmemHeapBlockSize(heap, addr, offset) result(bsz) bind(C,name='ShmemHeapBlockSize')
      import :: C_PTR, C_SIZE_T, HEAP_ELEMENT
      implicit none
      type(C_PTR), intent(IN), value :: heap
      type(C_PTR), intent(IN), value :: addr
      integer(HEAP_ELEMENT) :: offset
      integer(C_SIZE_T) :: bsz
    end function ShmemHeapBlockSize

    function ShmemHeapPtr2Offset(addr) result(offset) bind(C,name='ShmemHeapPtr2Offset')
      import :: C_PTR, HEAP_ELEMENT
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(HEAP_ELEMENT) :: offset
    end function ShmemHeapPtr2Offset

    function ShmemHeapPtr(heap, offset) result(p) bind(C,name='ShmemHeapPtr')
      import :: C_PTR, HEAP_ELEMENT
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(HEAP_ELEMENT), intent(IN), value :: offset
      type(C_PTR) :: p
    end function ShmemHeapPtr

  end interface

!> \endcond
  contains

  include 'io-server/f_alloc.inc'

  !> \brief create, perform a full, register heap
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%create(nbytes)
  function create(h, addr, nbytes) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the heap
    type(C_PTR) :: p                                        !< address of created heap
    h%p = ShmemHeapInit(addr, nbytes)
    p = h%p
  end function create 

  !> \brief create a heap object using the address of an existing heap (NO SETUP)
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%clone(nbytes)
  function clone(h, addr) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    type(C_PTR) :: p                                        !< address of already created heap
    h%p = addr
    p = h%p
  end function clone 

  !> \brief allocate a memory block in a heap
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%alloc(nbytes, safe)
  function alloc(h, nbytes, safe) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the desired block
    integer(C_INT), intent(IN), value :: safe               !< if nonzero perform operation under lock (atomic operation)
    type(C_PTR) :: p                                        !< address of created heap
    p = ShmemHeapAllocBlock(h%p, nbytes, safe)
  end function alloc 
  
  !> \brief free block by address in memory
  !> <br>type(heap) :: h<br>integer(C_INT) :: status<br>
  !> status = h\%free(addr)
  function free(addr) result(status)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< address of block to free
    integer(C_INT) :: status                    !< 0 if O.K., nonzero if error
    status = ShmemHeapFreeBlock(addr)
  end function free 
  
  !> \brief free block by offset in heap
  !> <br>type(heap) :: h<br>integer(C_INT) :: status<br>
  !> status = h\%validblock(offset)
  function freebyoffset(h, offset) result(status)
    implicit none
    class(heap), intent(INOUT) :: h                   !< heap object
    integer(C_INT), intent(IN), value :: offset       !< offset into heap of block to free
    integer(C_INT) :: status                          !< 0 if O.K., nonzero if error
    type(C_PTR) :: addr

    addr   = ShmemHeapPtr(h%p, offset)
    status = ShmemHeapFreeBlock(addr)
  end function freebyoffset 
  
  !> \brief register a heap
  !> <br>type(heap) :: h<br>integer(C_INT) :: nheaps<br>
  !> nheaps = h\%register(addr)
  function register(h, addr) result(nheaps)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    type(C_PTR), intent(IN), value :: addr      !< memory address
    integer(C_INT) :: nheaps                    !< number of registered heaps if successful, -1 otherwise
    h%p = addr
    nheaps = ShmemHeapRegister(addr)
  end function register 
  
  !> \brief check integrity of a heap
  !> <br>type(heap) :: h<br>integer(C_INT) :: status<br>
  !> status = h\%check(addr)
  function check(h, free_blocks, free_space, used_blocks, used_space) result(status)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(C_INT), intent(OUT)    :: free_blocks   !< number of free blocks in heap
    integer(C_INT), intent(OUT)    :: used_blocks   !< number of used blocks in heap
    integer(C_SIZE_T), intent(OUT) :: free_space    !< available space in heap (bytes)
    integer(C_SIZE_T), intent(OUT) :: used_space    !< used space in heap (bytes)
    integer(C_INT) :: status                    !< 0 if O.K., nonzero if error
    status = ShmemHeapCheck(h%p, free_blocks, free_space, used_blocks, used_space)
  end function check 
  
  !> \brief find if address belongs to a registered heap
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%inheap(addr)
  function inheap(addr) result(p)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    type(C_PTR) :: p                            !< address of heap (NULL if not in a registered heap)
    p = ShmemHeapContains(addr)
  end function inheap 
  
  !> \brief find if address belongs to a block from a registered heap
  !> <br>type(heap) :: h<br>integer(C_INT) :: status<br>
  !> status = h\%validblock(addr)
  function validblock(addr) result(status)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(C_INT) :: status                    !< 0 if valid block from registered heap, 
                                                !< -1 if unknown heap, 
                                                !< 1 if not a proper block pointer
    status = ShmemHeapValidBlock(addr)
  end function validblock 
  
  !> \brief get the size of a heap
  !> <br>type(heap) :: h<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%heapsize(p, addr, offset)
  function heapsize(h) result(bsz)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(C_SIZE_T) :: bsz                    !< size if known heap, -1 otherwise
    bsz = ShmemHeapSize(h%p)
  end function heapsize
  
  !> \brief get the size code of a heap block
  !> <br>type(heap) :: h<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%blocksize(p, addr, offset)<br>
  !> bsz = h\%blocksize(h\%address(0), addr, offset)
  function blocksize(p, addr, offset) result(bsz)
    implicit none
    type(C_PTR), intent(IN), value :: p         !< address of heap (if C_NULL_PTR, addr is needed)
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset from base of registered heap, 
    integer(C_SIZE_T) :: bsz                    !< size if valid block from known heap,
                                                !< -1 if unknown heap, 
                                                !< 1 if not a proper block pointer
    bsz = ShmemHeapBlockSize(p, addr, offset)
  end function blocksize
  
  !> \brief get the size code of a heap block
  !> <br>type(heap) :: h<br>integer(HEAP_ELEMENT) :: bsz<br>
  !> bsz = h\%blockcode(addr)
  function blockcode(addr) result(bsz)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(HEAP_ELEMENT) :: bsz                !< size marker if valid block from registered heap, (< 0 if block in use)
                                                !< -1 if unknown heap, 
                                                !< 1 if inside a registered heap but not a proper block pointer
    bsz = ShmemHeapBlockSizeCode(addr)
  end function blockcode
  
  !> \brief get offset into heap for a memory address
  !> <br>type(heap) :: h<br>integer(C_INT) :: off<br>
  !> off = h\%offset(addr)
  function offset(addr) result(off)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(C_INT) :: off                       !< offset from base of registered heap, 
                                                !< -1 if unknown heap, 
    off = ShmemHeapPtr2Offset(addr)
  end function offset 
  
  !> \brief translate offset in heap into address
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%address(off)
  function address(h, off) result(p)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(C_INT), intent(IN), value :: off    !< offset into heap
    type(C_PTR) :: p                            !< address, NULL if invalid offset/heap combination
    p = ShmemHeapPtr(h%p, off)
  end function address 
  
end module shmem_heap

!> \cond DOXYGEN_SHOULD_SKIP_THIS

#if defined(SELF_TEST)
#define NPTEST 125
#define MAXINDEXES  1024

program self_test
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
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, baseptr, mybase, mysize
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
      call h%allocate(demo, [1100+i*10+myrank,1,1])    ! 3D integer array
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
1 format(2(A,Z16.16))
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
#endif

!> \endcond
