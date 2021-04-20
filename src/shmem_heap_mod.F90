! Copyright (C) 2021  Environnement Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

!> \file
!> \brief shared memory heap Fortran module (object oriented)
module shmem_heap
  use ISO_C_BINDING
  implicit none

!   private :: DATA_ELEMENT   ! prevent ambiguous definition if made available by another module
!   include 'io-server/common.inc'

#define CB_ELEMENT 4
! the above line is temporary (waiting for module defining CB_ELEMENT)

  !> \brief maximum number of allowed dimensions for an array in this heap type
  integer, parameter :: MAX_ARRAY_RANK = 5

! type of a heap element (must be consistent with io-server definition)
  integer, parameter :: HEAP_ELEMENT =  CB_ELEMENT  !<  type of a heap element (must be consistent with C code)

  !> \brief C compatible data block metadata
  type, public, bind(C) :: block_meta_c
    private
    !> \private
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: d  = [0, 0, 0, 0, 0] !< array dimensions
    !> \private
    integer(C_INT)    :: tkr = 0           !< array type, kind, rank
    integer(C_SIZE_T) :: offset = 0        !< offset in bytes from reference address (memory arena most likely)
  end type block_meta_c

  !> \brief Fortran 2008 data block metadata (using the C layout)
  type, public :: block_meta_f08
    private
    !> \private
    type(block_meta_c) :: a           !< array descriptor, C interoperable
    !> \private
    type(C_PTR) :: p   = C_NULL_PTR   !< array address

  contains
    !> \return array type code (1=integer, 2=real)
    procedure :: t
    !> \return array kind (1/2/4/8 bytes)
    procedure :: k
    !> \return array rank (1/2/3/../MAX_ARRAY_RANK)
    procedure :: r
    !> \return array(MAX_ARRAY_RANK) containing dimensions
    procedure :: dims
    !> \return status, 0 if O.K. non zero otherwise
    procedure :: meta
    !> \return                              none
    procedure :: reset                      !< nullify operator
    !> \return                              none
    procedure :: assign                     !< assignment operator
    !> \return                              none
    GENERIC :: ASSIGNMENT(=) => assign      !< assignment operator
    !> \return                              .true. if equal, .false. if not equal
    procedure :: equal                      !< equality operator
    !> \return                              .true. if equal, .false. if not equal
    GENERIC :: operator(==) => equal        !< equality operator
    !> \return                              .false. if equal, .true. if not equal
    procedure :: unequal_meta                !< non equality operator
    !> \return                              .false. if equal, .true. if not equal
    GENERIC :: operator(/=) => unequal_meta !< non equality operator
  end type block_meta_f08

  !> \brief heap user defined type
  type, public :: heap
    private
    !> \private
    type(C_PTR) :: p   = C_NULL_PTR      !< address of storage used by heap
    type(C_PTR) :: ref = C_NULL_PTR      !< reference address to compute offsets into memory arena for this heap
  contains

    !> \return                          none
    procedure :: set_base               !< set reference address to compute offsets into memory arena

    !> \return                          base address to compute offsets for this heap
    procedure :: get_base               !< set reference address to compute offsets into memory arena

    !> \return                          address of heap
    procedure :: createb                !< create, initialize, register a heap at specified address (size in uint64_t bytes)

    !> \return                          address of heap
    procedure :: createw               !< create, initialize, register a heap at specified address (size in uint32_t 32 bit words)

    GENERIC   :: create => createb, createw

    !> \return                          address of default heap
    procedure :: get_default            !< get address of the default heap

    !> \return                          index in table, -1 if unknown heap
    procedure :: set_default            !< make this heap the default heap

    !> \return                          address of heap
    procedure :: clone_h                !< clone a heap object using the address of an existing heap
    GENERIC   :: clone => clone_h

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

    !> \return                          internal pointer to heap, NULL if not a heap
    procedure :: ptr                    !< get internal pointer to heap

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
    procedure, NOPASS :: freebyaddress  !< free an allocated block by address in memory

    !> \return                          0 if O.K., nonzero if error
    procedure, NOPASS :: freebymeta     !< free an allocated block using its metadata

    GENERIC   :: free => freebyaddress, freebymeta, freebyoffset

    !> \return                           0 if O.K., nonzero if error
    procedure :: freebyoffset           !< free space associated to offset into heap

    !> \return                           index in table, -1 if unknown heap
    procedure :: GetIndex                !< get heap index in registered table

    !> \return                           0 if O.K., nonzero if error
    procedure :: GetInfo                 !< get heap statistics using heap address

    !> \return                           0 if O.K., nonzero if error
    procedure, NOPASS :: GetInfoReg      !< get heap statistics using index in registered teable

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
    !> \return     a fortran pointer to integer and real arrays of 1 to MAX_ARRAY_RANK dimension (see f_alloc.inc)
    GENERIC   :: allocate =>  &
                              I1_5D, I1_4D, I1_3D, I1_2D, I1_1D, &
                              I2_5D, I2_4D, I2_3D, I2_2D, I2_1D, &
                              I4_5D, I4_4D, I4_3D, I4_2D, I4_1D, &
                              I8_5D, I8_4D, I8_3D, I8_2D, I8_1D, &
                              R4_5D, R4_4D, R4_3D, R4_2D, R4_1D, &
                              R8_5D, R8_4D, R8_3D, R8_2D, R8_1D       !< generic Fortran array type associated allocatior
  end type heap

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

    function ShmemHeapInit(heap, nbytes) result(h) bind(C,name='ShmemHeapInit')
      import :: C_PTR, C_SIZE_T
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_SIZE_T), intent(IN), value :: nbytes
      type(C_PTR) :: h
    end function ShmemHeapInit

    function ShmemHeapSetDefault(heap) result(ix) bind(C,name='ShmemHeapSetDefault')
      import :: C_PTR, C_INT
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_INT) :: ix
    end function ShmemHeapSetDefault

    function ShmemHeapGetDefault() result(h) bind(C,name='ShmemHeapGetDefault')
      import :: C_PTR
      implicit none
      type(C_PTR) :: h
    end function ShmemHeapGetDefault

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

    function ShmemHeapIndex(heap) result(ix) bind(C,name='ShmemHeapIndex')
      import :: C_INT, C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: heap
      integer(C_INT) :: ix
    end function ShmemHeapIndex

    function ShmemHeapGetInfo(ix, sz, max, nblk, nbyt) result(status) bind(C,name='ShmemHeapGetInfo')
      import :: C_INT, C_LONG_LONG
      implicit none
      integer(C_INT), intent(IN), value :: ix
      integer(C_LONG_LONG), intent(OUT) :: sz
      integer(C_LONG_LONG), intent(OUT) :: max
      integer(C_LONG_LONG), intent(OUT) :: nblk
      integer(C_LONG_LONG), intent(OUT) :: nbyt
      integer(C_INT) :: status
    end function ShmemHeapGetInfo

    subroutine ShmemHeapDumpInfo() bind(C,name='ShmemHeapDumpInfo')
    end subroutine ShmemHeapDumpInfo

    function Pointer_offset(ref, to, szeof) result(offset) bind(C,name='Pointer_offset')
      import :: C_INTPTR_T, C_PTR, C_INT
      implicit none
      type(C_PTR), intent(IN), value    :: ref
      type(C_PTR), intent(IN), value    :: to
      integer(C_INT), intent(IN), value :: szeof
      integer(C_INTPTR_T)               :: offset
    end function Pointer_offset

    function Pointer_add_offset(ref, offset, szeof) result(to) bind(C,name='Pointer_add_offset')
      import :: C_INTPTR_T, C_PTR, C_INT
      implicit none
      type(C_PTR), intent(IN), value          :: ref
      integer(C_INTPTR_T), intent(IN), value  :: offset
      integer(C_INT), intent(IN), value       :: szeof
      type(C_PTR)                             :: to
    end function Pointer_add_offset

  end interface

!> \endcond
  contains

  function get_base(this) result(addr)
    implicit none
    class(heap), intent(IN) :: this                      ! heap object
    type(C_PTR) :: addr                                  ! reference address
    addr = this % ref
  end function get_base

  subroutine set_base(this, addr)
    implicit none
    class(heap), intent(INOUT) :: this                   ! heap object
    type(C_PTR), intent(IN), value :: addr               ! reference address
    this % ref = addr
  end subroutine set_base

  !> get heap index in registered table
  function GetIndex(h) result(ix)
    implicit none
    class(heap), intent(IN) :: h                         !< heap object
    integer(C_INT) :: ix                                 !< index in registered heap table
    ix = ShmemHeapIndex(h%p)
  end function GetIndex

  !> get heap statistics
  function GetInfoReg(ix, sz, max, nblk, nbyt) result(status)
    implicit none
    integer(C_INT), intent(IN), value :: ix                 !< index into registered heap table
    integer(C_LONG_LONG), intent(OUT) :: sz                 !< size of heap (bytes)
    integer(C_LONG_LONG), intent(OUT) :: max                !< high water mark in heap  (highest allocation point) (bytes)
    integer(C_LONG_LONG), intent(OUT) :: nblk               !< number of blocks that have been allocated
    integer(C_LONG_LONG), intent(OUT) :: nbyt               !< total number of bytes used by allocated blocks
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    status = ShmemHeapGetInfo(ix, sz, max, nblk, nbyt)
  end function GetInfoReg

  !> get heap statistics
  function GetInfo(h, sz, max, nblk, nbyt) result(status)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_LONG_LONG), intent(OUT) :: sz                 !< size of heap (bytes)
    integer(C_LONG_LONG), intent(OUT) :: max                !< high water mark in heap  (highest allocation point) (bytes)
    integer(C_LONG_LONG), intent(OUT) :: nblk               !< number of blocks that have been allocated
    integer(C_LONG_LONG), intent(OUT) :: nbyt               !< total number of bytes used by allocated blocks
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    integer(C_INT) :: ix
    status = -1
    ix = ShmemHeapIndex(h%p)
    if(ix < 0) return
    status = ShmemHeapGetInfo(ix, sz, max, nblk, nbyt)
  end function GetInfo

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
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: d         !< array dimensions
    d = this%a%d
  end function dims
  !> \brief get array dimensions from C block metadata (C callable version)
  subroutine BlockMeta_dims(this, dims) BIND(C,name='BlockMeta_dims')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT), intent(OUT), dimension(MAX_ARRAY_RANK) :: dims   !< array dimensions
    dims = this%d
  end subroutine BlockMeta_dims

  !> \brief nullify operator for type block_meta_f08
  subroutine reset(this)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata object
    this%p     = C_NULL_PTR
    this%a%tkr = 0
    this%a%d   = 0
  end subroutine reset

  !> \brief assignment operator for type block_meta_f08
  subroutine assign(this, other)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata object
    type(block_meta_f08), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    this%p     = other%p
    this%a%tkr = other%a%tkr
    this%a%d   = other%a%d
  end subroutine assign

  !> \brief equality operator for type block_meta_f08
  function equal(this, other) result(isequal)
    implicit none
    class(block_meta_f08), intent(IN)    :: this              !< metadata object
    type(block_meta_f08), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    logical :: isequal                                        !< true if equal
    integer :: i
    isequal = C_ASSOCIATED(this%p, other%p)
    isequal = isequal .and. (this%a%tkr == other%a%tkr)
    do i = 1, size(this%a%d)
      isequal = isequal .and. (this%a%d(i)   == other%a%d(i))
    enddo
  end function equal

  !> \brief non equality operator for type block_meta_f08
  function unequal_meta(this, other) result(isequal)
    implicit none
    class(block_meta_f08), intent(IN)    :: this              !< metadata object
    type(block_meta_f08), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    logical :: isequal                                        !< true if equal
    integer :: i
    isequal = .not. C_ASSOCIATED(this%p, other%p)
    isequal = isequal .and. (this%a%tkr /= other%a%tkr)
    do i = 1, size(this%a%d)
      isequal = isequal .and. (this%a%d(i)   /= other%a%d(i))
    enddo
  end function unequal_meta

  include 'io-server/f_alloc.inc'

  !> \brief create, initialize, and register a heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p, addr<br>integer(C_SIZE_T) :: nwds<br>
  !> p = h\%createw(addr, nwds)
  function createw(h, addr, nwds) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    integer, intent(IN), value     :: nwds                  !< size in 32 bit units of the heap
    type(C_PTR) :: p                                        !< address of created heap
    integer(C_SIZE_T) :: nbytes
    nbytes = nwds * 4_8
    h%p = ShmemHeapInit(addr, nbytes)
    p = h%p
  end function createw

  !> \brief create, initialize, and register a heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p, addr<br>integer(C_SIZE_T) :: nbytes<br>
  !> p = h\%createb(addr, nbytes)
  function createb(h, addr, nbytes) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the heap
    type(C_PTR) :: p                                        !< address of created heap
    h%p = ShmemHeapInit(addr, nbytes)
    p = h%p
  end function createb

  !> \brief get the address of the default heap, set heap object to default heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%get_default()
  function get_default(h) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR) :: p                                        !< address of heap
    h%p = ShmemHeapGetDefault()                             ! set internal pointer to default heap address
    p = h%p                                                 ! and return that address
  end function get_default 

  !> \brief set the default heap to this heap
  !> <br>example :<br>type(heap) :: h<br>type(C_INT) :: ix<br>
  !> ix = h\%set_default()
  function set_default(h) result(ix)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_INT) :: ix                                    !< index in registered heap table
    ix = ShmemHeapSetDefault(h%p)                           ! index of default heap in registered table (-1 if unknown)
  end function set_default 

  !> \brief create a new heap object using the address of an existing heap (NO SETUP)
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p, addr<br>
  !> p = h\%clone_h(addr)
  function clone_h(h, addr) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address (must be an existing heap address)
    type(C_PTR) :: p                                        !< address of already created heap
    h%p = addr
    p = h%p
  end function clone_h

  !> \brief allocate a memory block in a heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p<br>integer(C_SIZE_T) :: nbytes<br>
  !> p = h\%alloc(nbytes, 0)
  function alloc(h, nbytes, safe) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the desired block
    integer(C_INT), intent(IN), value :: safe               !< if nonzero perform operation under lock (atomic operation)
    type(C_PTR) :: p                                        !< address of created heap
    p = ShmemHeapAllocBlock(h%p, nbytes, safe)
  end function alloc 
  
  !> \brief free block by address in memory
  !> <br>example :<br>type(heap) :: h<br>integer(C_INT) :: status<br>type(C_PTR) :: addr<br>
  !> status = h\%freebyaddress(addr)
  function freebyaddress(addr) result(status)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< address of block to free
    integer(C_INT) :: status                    !< 0 if O.K., nonzero if error
    status = ShmemHeapFreeBlock(addr)
  end function freebyaddress 
  
  !> \brief free block by offset in heap
  !> <br>example :<br>type(heap) :: h<br>integer(HEAP_ELEMENT) :: offset<br> integer(C_INT) :: status<br>
  !> status = h\%freebyoffset(offset)
  function freebyoffset(h, offset) result(status)
    implicit none
    class(heap), intent(INOUT) :: h                   !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset       !< offset into heap of block to free
    integer(C_INT) :: status                          !< 0 if O.K., nonzero if error
    type(C_PTR) :: addr

    addr   = ShmemHeapPtr(h%p, offset)
    status = ShmemHeapFreeBlock(addr)
  end function freebyoffset 
  
  !> \brief free block by metadata in heap
  !> <br>example :<br>type(heap) :: h<br>type(block_meta_f08) :: meta08<br> integer(C_INT) :: status<br>
  !> status = h\%freebymeta(meta08)
  function freebymeta(meta08) result(status)
    implicit none
    type(block_meta_f08), intent(IN) :: meta08        !< metadata associated to memory block
    integer(C_INT) :: status                          !< 0 if O.K., nonzero if error
    type(C_PTR) :: addr

    addr   = meta08%p
    status = ShmemHeapFreeBlock(addr)
  end function freebymeta 
  
  !> \brief register a heap, set address
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: nheaps<br>
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
  !> <br>example :<br>type(heap) :: h<brinteger(C_INT) :: free_blocks,used_blocks>
  !> <br>integer(C_SIZE_T) :: free_space,used_space<br>integer(C_INT) :: status<br>
  !> status = h\%check(free_blocks, free_space, used_blocks, used_space)
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
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>type(C_PTR) :: p<br>
  !> p = h\%inheap(addr)
  function inheap(addr) result(p)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    type(C_PTR) :: p                            !< address of heap (NULL if not in a registered heap)
    p = ShmemHeapContains(addr)
  end function inheap 
  
  !> \brief find if address belongs to a block from a registered heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: status<br>
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
  !> <br>example :<br>type(heap) :: h<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%heapsize()
  function heapsize(h) result(bsz)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(C_SIZE_T) :: bsz                    !< size if known heap, -1 otherwise
    bsz = ShmemHeapSize(h%p)
  end function heapsize
  
  !> \brief get the size code of a heap block
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(HEAP_ELEMENT) :: offset<br>integer(C_SIZE_T) :: bsz<br>
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
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(HEAP_ELEMENT) :: bsz<br>
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
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: off<br>
  !> off = h\%offset(addr)
  function offset(addr) result(off)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(HEAP_ELEMENT) :: off                       !< offset from base of registered heap, 
                                                !< -1 if unknown heap, 
    off = ShmemHeapPtr2Offset(addr)
  end function offset 
  
  !> \brief get internal pointer(address) to heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%ptr()
  function ptr(h) result(p)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    type(C_PTR) :: p                            !< internal address, NULL if not initialized
    p = h%p
  end function ptr 
  
  !> \brief translate offset in heap into address
  !> <br>example :<br>type(heap) :: h<br>integer(HEAP_ELEMENT) :: offset<br>type(C_PTR) :: p<br>
  !> p = h\%address(offset)
  function address(h, offset) result(p)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset    !< offset into heap
    type(C_PTR) :: p                            !< address, NULL if invalid offset/heap combination
    p = ShmemHeapPtr(h%p, offset)
  end function address 
  
end module shmem_heap
