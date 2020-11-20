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

    !> \return                          address for offset in heap, NULL if address not a heap
    procedure :: address                !< translate offset in heap into actual address

    !> \return                          0 if valid block, -1 unknown heap, 1 not block pointer
    procedure, NOPASS :: validblock     !< find if address belongs to a registered heap

    !> \return                          block address, NULL if allocation fails
    procedure :: alloc                  !< allocate a block in a registered heap

    !> \return                          0 if O.K., nonzero if error
    procedure, NOPASS :: free           !< free an allocated block by address in memory

    !> \return                           0 if O.K., nonzero if error
    procedure :: freebyoffset           !< free space associated to offset into heap
  end type heap
! tell doxygen to ignore the following block
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  interface

    function ShmemHeapInit(p, nwords) result(h) bind(C,name='ShmemHeapInit')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: p
      integer(C_INT), intent(IN), value :: nwords
      type(C_PTR) :: h
    end function ShmemHeapInit

    function ShmemHeapCheck(p, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeapCheck')
      import :: C_INT, C_PTR, C_SIZE_T
      implicit none
      type(C_PTR), intent(IN), value :: p
      integer(C_INT), intent(OUT) :: free_blocks, used_blocks
      integer(C_SIZE_T), intent(OUT) :: free_space, used_space
      integer(C_INT) :: status
    end function ShmemHeapCheck

    function ShmemHeapAllocBlock(p, nwords, safe) result(b) bind(C,name='ShmemHeapAllocBlock')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: p
      integer(C_INT), intent(IN), value :: nwords
      integer(C_INT), intent(IN), value :: safe
      type(C_PTR) :: b
    end function ShmemHeapAllocBlock

    function ShmemHeapFreeBlock(addr) result(status) bind(C,name='ShmemHeapFreeBlock')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(C_INT) :: status
    end function ShmemHeapFreeBlock

    function ShmemHeapRegister(p) result(status) bind(C,name='ShmemHeapRegister')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: p
      integer(C_INT) :: status
    end function ShmemHeapRegister

    function ShmemHeapContains(addr) result(p) bind(C,name='ShmemHeapContains')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      type(C_PTR) :: p
    end function ShmemHeapContains

    function ShmemHeapValidBlock(addr) result(status) bind(C,name='ShmemHeapValidBlock')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(C_INT) :: status
    end function ShmemHeapValidBlock

    function ShmemHeapPtr2Offset(addr) result(offset) bind(C,name='ShmemHeapPtr2Offset')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(C_INT) :: offset
    end function ShmemHeapPtr2Offset

    function ShmemHeapPtr(addr, offset) result(p) bind(C,name='ShmemHeapPtr')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: addr
      integer(C_INT), intent(IN), value :: offset
      type(C_PTR) :: p
    end function ShmemHeapPtr

  end interface
#endif

  contains

  !> \brief create a heap, perform a full setup of this heap, register heap
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%create(nwords)
  function create(h, addr, nwords) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the heap
    type(C_PTR) :: p                                        !< address of created heap
    h%p = ShmemHeapInit(addr, nwords)
    p = h%p
  end function create 

  !> \brief create a heap object using the address of an existing heap (NO SETUP)
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%clone(nwords)
  function clone(h, addr) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    type(C_PTR) :: p                                        !< address of created heap
    h%p = addr
    p = h%p
  end function clone 

  !> \brief allocate a memory block in a heap
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%alloc(nwords, safe)
  function alloc(h, nwords, safe) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_INT), intent(IN), value :: nwords             !< size in 32 bit elements of the desired block
    integer(C_INT), intent(IN), value :: safe               !< if nonzero perform operation under lock (atomic operation)
    type(C_PTR) :: p                                        !< address of created heap
    p = ShmemHeapAllocBlock(h%p, nwords, safe)
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
    integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
    integer(C_SIZE_T), intent(OUT) :: free_space, used_space
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
  !> <br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%validblock(addr)
  function validblock(addr) result(status)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(C_INT) :: status                    !< 0 if valid block from registered heap, 
                                                !< -1 if unknown heap, 
                                                !< 1 if inside a registered heap but not a proper block pointer
    status = ShmemHeapValidBlock(addr)
  end function validblock 
  
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

#ifndef DOXYGEN_SHOULD_SKIP_THIS

#if defined(SELF_TEST)
#define NPTEST 125
#define MAXINDEXES  1024
program demo
  use shmem_heap
  implicit none
  include 'mpif.h'
  integer :: myrank, nprocs, ierr, win, disp_unit, i, status
  type(heap) :: h
  type(C_PTR) :: p
  type(C_PTR), dimension(128) :: blocks
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, baseptr, mybase, mysize
  integer(C_INT)    :: free_blocks, used_blocks
  integer(C_SIZE_T) :: free_space, used_space
  type, bind(C) :: mem_layout
    integer(C_INT) :: nindexes
    type(C_PTR)    :: pindex
    type(C_PTR)    :: pheap
  end type mem_layout
  type(mem_layout) :: memory
  integer(C_INT), dimension(:), pointer :: index, ram, myheap
!   integer(KIND=MPI_ADDRESS_KIND) :: disp
  logical, parameter :: bugged = .false.

  myrank = 0
  nprocs = 1
  ! MPI multiprocess test
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
  print *,'this is PE', myrank+1, ' of', nprocs

  winsize = 1024*1024
  disp_unit = 4
  call MPI_Win_allocate_shared(winsize, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, baseptr, win, ierr)
  call MPI_Win_shared_query(win, 0, mysize, disp_unit, mybase,   ierr)  ! get my base address

  p = transfer(mybase, C_NULL_PTR)
  call c_f_pointer(p, ram,[winsize/4])              ! ram points to shared memory segment
  if(myrank == 0) then
    ram(1) = MAXINDEXES
  endif
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  memory%nindexes = ram(1)                          ! sizze of index table
  memory%pindex   = C_LOC(ram(2))                   ! index array
  memory%pheap    = C_LOC(ram(MAXINDEXES+2))        ! start of heap
  call c_f_pointer(memory%pindex, index, [MAXINDEXES]) ! index now points to index table
  p = memory%pheap                                  ! p points to heap
  call c_f_pointer(p, myheap, [1024*8]);
  if(bugged) then
    index = -1                                       ! this is a bug (potential race condition)
  endif

  if(myrank == 0) then
    index = -1
    p = h%create(p, 1024*32)           ! create heap
    do i = 1, 10
      blocks(i) = h%alloc(1025, 0)     ! attempt to allocate block
      if( .not. C_ASSOCIATED(blocks(i)) ) then
        print *,'allocation failed for block',i
        exit
      endif
!       index(i) = (transfer(blocks(i),disp) - transfer(memory%pheap,disp)) / 4
      index(i) = h%offset(blocks(i))
      print *,'index =',index(i),h%offset(h%address(index(i))),h%offset(blocks(i))
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if(myrank .ne. 0) then
    do i = 1, MAXINDEXES
      if(index(i) > 0) print *,'index',i,index(i)
    enddo
    i = h%register(p)
    print *,'process',myrank,', nheaps =',i
    print '(A,Z16.16)' , 'RAM address  :',loc(ram)
    print '(A,Z16.16)' , 'HEAP address :',p
    status = h%check(free_blocks, free_space, used_blocks, used_space)
    print *,free_blocks,' free blocks,',used_blocks,' used blocks'
    print *,free_space,' free space,',used_space,' used space'
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if(myrank .ne. 0) then
    do i = myrank, used_blocks-1,nprocs-1
      if(index(i) > 0) then
!         status = h%free(C_LOC(myheap(1+index(i))))
        status = h%freebyoffset(index(i))
!         print *,'h%free status =',status,h%offset(C_LOC(myheap(1+index(i))))
        print *,'h%free status =',status,h%offset(h%address(index(i)))
      endif
    enddo
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

    status = h%check(free_blocks, free_space, used_blocks, used_space)
    print *,free_blocks,' free blocks,',used_blocks,' used blocks'
    print *,free_space,' free space,',used_space,' used space'

  call Mpi_Finalize(ierr)
  stop
end program
#endif

#endif
