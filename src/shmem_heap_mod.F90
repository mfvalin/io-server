!> \file
!> \brief shared memory heap Fortran module (object oriented)
module shmem_heap
  use ISO_C_BINDING
  !> \brief C compatible data block metadata
  type, bind(C) :: block_meta_c
    private
    integer(C_INT), dimension(5) :: d   !< array dimensions
    integer(C_INT) :: tkr               !< array type, kind, rank
  end type
  !> \brief Fortran 2008 data block metadata (using the C layout)
  type, public :: block_meta_f08
    private
    type(block_meta_c) :: a         !< array descriptor
  contains
    !> \return array type code (1=integer, 2=real)
    procedure :: t
    !> \return array kind (1/2/4/8 bytes)
    procedure :: k
    !> \return array rank (1/2/3/4/5)
    procedure :: r
    !> \return array dimensions
    procedure :: dims
    !> \return status, 0 if O.K. non zero otherwise
    procedure :: meta
  end type
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
    function c_malloc(sz) result(p) BIND(C,name='malloc')
      import :: C_PTR, C_SIZE_T
      integer(C_SIZE_T), value :: sz
      type(C_PTR) :: p
    end function c_malloc

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

    function ShmemHeapSetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapSetBlockMeta')
      import :: C_PTR, C_INT, block_meta_c
      implicit none
      type(C_PTR), intent(IN), value    :: block
      type(block_meta_c), intent(IN)    :: metadata
      integer(C_INT), intent(IN), value :: msz
      integer(C_INT) :: status
    end function ShmemHeapSetBlockMeta

    function ShmemHeapGetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapGetBlockMeta')
      import :: C_PTR, C_INT, block_meta_c
      implicit none
      type(C_PTR), intent(IN), value    :: block
      type(block_meta_c), intent(OUT)    :: metadata
      integer(C_INT), intent(IN), value :: msz
      integer(C_INT) :: status
    end function ShmemHeapGetBlockMeta

  end interface

!> \endcond
  contains

  !> \brief get array type from Fortran block metadata
  function meta(this, block) result(status)
    implicit none
    class(block_meta_f08), intent(OUT) :: this         !< block object
    type(C_PTR), intent(IN), value :: block            !< address of block
    integer(C_INT) :: status                           !< 0 if O.K., nonzero if error
    integer :: msz
    type(block_meta_c) :: t
    msz = C_SIZEOF(t)
    status = ShmemHeapGetBlockMeta(block, this%a, msz)
  end function meta

  !> \brief get array type from Fortran block metadata
  function t(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array type
    n = and(ishft(this%a%tkr,-4), 15)
  end function t
  !> \brief get array type from C block metadata (C callable version)
  function BlockMeta_t(this) result(n) BIND(C,name='BlockMeta_t')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array type
    n = and(ishft(this%tkr,-4), 15)
  end function BlockMeta_t

  !> \brief get array kind from Fortran block metadata
  function k(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array kind (1/2/4/8 bytes)
    n = and(ishft(this%a%tkr,-8), 15)
  end function k
  !> \brief get array kind from C block metadata (C callable version)
  function BlockMeta_k(this) result(n) BIND(C,name='BlockMeta_k')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array kind (1/2/4/8 bytes)
    n = and(ishft(this%tkr,-8), 15)
  end function BlockMeta_k

  !> \brief get array rank from Fortran block metadata
  function r(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array rank
    n = and(this%a%tkr, 15)
  end function r
  !> \brief get array rank from C block metadata (C callable version)
  function BlockMeta_r(this) result(n) BIND(C,name='BlockMeta_r')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array rank
    n = and(this%tkr, 15)
  end function BlockMeta_r

  !> \brief get array dimensions from Fortran block metadata
  function dims(this) result(d)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT), dimension(5) :: d               !< array dimensions
    d = this%a%d
  end function dims
  !> \brief get array dimensions from C block metadata (C callable version)
  subroutine BlockMeta_dims(this, dims) BIND(C,name='BlockMeta_dims')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT), intent(OUT), dimension(5) :: dims   !< array dimensions
    dims = this%d
  end subroutine BlockMeta_dims

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
