! Copyright (C) 2022  Environnement et Changement climatique Canada
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

!> \file
!> \brief shared memory heap Fortran module (object oriented)
module heap_module
  use ISO_C_BINDING
  implicit none

  private

  integer, parameter, public :: HEAP_ELEMENT =  C_INT64_T !<  type of a heap element (must be consistent with C code)
  integer, parameter, public :: MAX_ARRAY_RANK = 5        !< maximum number of allowed dimensions for an array in this heap type
  integer, parameter         :: TKR_INTEGER = 1           !< tkr code for integer arrays
  integer, parameter         :: TKR_REAL    = 2           !< tkr code for real arrays


  public :: Pointer_offset, bm_to_ptr, ptr_to_bm
  public :: get_heap_from_address
  !   ===========================  metadata types and type bound procedures ===========================
  !> \brief C interoperable data block metadata
  type, private, bind(C) :: block_meta_c
    private
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: d  = [0, 0, 0, 0, 0] !< array dimensions \private
    integer(C_INT)    :: tkr = 0           !< array type, kind, rank \private
    integer(C_SIZE_T) :: offset = 0        !< offset in bytes from reference address (memory arena most likely) \private
  end type block_meta_c


  include 'io-server/shmem_heap.inc'

  !> \brief C interoperable version of block_meta_f08
  type, private, bind(C) :: block_meta
    private
    type(block_meta_c) :: a           !< array descriptor, C interoperable \private
    type(C_PTR) :: p   = C_NULL_PTR   !< array address \private
  end type block_meta

  !> \brief Fortran 2008 data block metadata (using the C layout)
  type, public :: block_meta_f08
    private
    type(block_meta_c) :: a           !< array descriptor, C interoperable \private
    type(C_PTR) :: p   = C_NULL_PTR   !< array address \private

  contains
    procedure :: print => block_meta_print
    
    procedure :: get_ptr => block_meta_get_ptr !< \return The stored pointer
    procedure :: get_offset => block_meta_get_offset !< \return The offset of this block within its heap
    procedure :: t              !< \return array type code (1=integer, 2=real)
    procedure :: get_kind       !< \return array kind (1/2/4/8 bytes)
    procedure :: r              !< \return array rank (1/2/3/../MAX_ARRAY_RANK)
    procedure :: get_dimensions !< \return array(MAX_ARRAY_RANK) containing dimensions
    procedure :: metadata !< \return status, 0 if O.K. non zero otherwise

    procedure :: free => free_by_meta

    procedure :: reset                      !< nullify operator
    procedure :: assign                     !< assignment operator, block_meta_f08 = block_meta_f08
    procedure :: assign_meta                !< assignment operator, block_meta_f08 = block_meta
    GENERIC :: ASSIGNMENT(=) => assign, assign_meta      !< generic assignment operator

    procedure :: equal                      !< equality operator
    GENERIC :: operator(==) => equal        !< equality operator
    procedure :: unequal_meta               !< non equality operator
    GENERIC :: operator(/=) => unequal_meta !< non equality operator
  end type block_meta_f08

!   ===========================  heap type and type bound procedures ===========================
  !> \brief heap user defined type
  type, public :: heap
    private
    type(C_PTR) :: p   = C_NULL_PTR      !< address of storage used by heap \private
    type(C_PTR) :: ref = C_NULL_PTR      !< reference address to compute offsets into memory arena for this heap \private
  contains

    procedure :: set_base     !< Set reference address to compute offsets into memory arena
    procedure :: get_base     !< \return Base address to compute offsets for this heap

    procedure :: createb      !< create, initialize, register a heap at specified address (size in uint64_t bytes). \return address of heap
    procedure :: createw      !< create, initialize, register a heap at specified address (size in uint32_t 32 bit words). \return address of heap
    GENERIC   :: create => createb, createw

    procedure :: get_default  !< \return Address of the default heap
    procedure :: set_default  !< Make this heap the default heap. \return Index in heap table, -1 if unknown heap

    procedure :: clone_h      !< Clone a heap object using the address of an existing heap. \return The given address
    GENERIC   :: clone => clone_h

    procedure :: register     !< Register an existing heap at specified address. \return Number of registered heap if successful, -1 otherwise
    procedure :: check        !< Check integrity of heap. \return 0 if OK, something else otherwise

    procedure, NOPASS :: get_heap_from_address    !< Find if address belongs to a registered heap. \return Address of heap, NULL if error
    ! procedure, NOPASS :: get_offset_from_address  !< Translate address to offset in a heap. \return Offset within the correct heap, -1 if error

    procedure, NOPASS :: is_valid_block           !< \return Whether the given address belongs to a registered heap
    procedure, NOPASS :: get_blocksize            !< \return block size (bytes) of a given heap block (negative if error)

    procedure :: get_address_from_offset  !< \return Block address that corresponds to the given offset in the heap. NULL if error
    procedure :: get_ptr => heap_get_ptr  !< \return Internal pointer to heap

    procedure :: get_size               !< \return Size of heap, -1 if error
    procedure :: alloc                  !< Allocate a block from a heap.  \return block address, NULL if allocation fails

    GENERIC   :: free => free_by_address, free_by_offset
    procedure, NOPASS :: free_by_address  !< Free an allocated block by address in memory. \return 0 if O.K., nonzero if error
    procedure :: free_by_offset           !< Free space associated to offset into heap. \return 0 if O.K., nonzero if error

    procedure :: GetInfo                 !< Get heap statistics using heap address. \return 0 if O.K., nonzero if error
    procedure, NOPASS :: GetInfoReg      !< Get heap statistics using index in registered table. \return 0 if O.K., nonzero if error
    procedure, NOPASS :: dumpinfo        !< Dump information about all known heaps

!> \cond DOXYGEN_SHOULD_SKIP_THIS
!   ===========================  interfaces to script generated functions  ===========================
    !> \return                           a fortran pointer
    procedure   ::            &  !< specific procedures needed for generic type bound allocator
                              allocate_I1_5D, allocate_I1_4D, allocate_I1_3D, allocate_I1_2D, allocate_I1_1D, &
                              allocate_I2_5D, allocate_I2_4D, allocate_I2_3D, allocate_I2_2D, allocate_I2_1D, &
                              allocate_I4_5D, allocate_I4_4D, allocate_I4_3D, allocate_I4_2D, allocate_I4_1D, &
                              allocate_I8_5D, allocate_I8_4D, allocate_I8_3D, allocate_I8_2D, allocate_I8_1D, &
                              allocate_R4_5D, allocate_R4_4D, allocate_R4_3D, allocate_R4_2D, allocate_R4_1D, &
                              allocate_R8_5D, allocate_R8_4D, allocate_R8_3D, allocate_R8_2D, allocate_R8_1D 
!> \endcond
    !> \return     a fortran pointer to integer and real arrays of 1 to MAX_ARRAY_RANK dimension (see f_alloc.inc)
    GENERIC   :: allocate =>  &  !< generic Fortran array type bound allocator
                              allocate_I1_5D, allocate_I1_4D, allocate_I1_3D, allocate_I1_2D, allocate_I1_1D, &
                              allocate_I2_5D, allocate_I2_4D, allocate_I2_3D, allocate_I2_2D, allocate_I2_1D, &
                              allocate_I4_5D, allocate_I4_4D, allocate_I4_3D, allocate_I4_2D, allocate_I4_1D, &
                              allocate_I8_5D, allocate_I8_4D, allocate_I8_3D, allocate_I8_2D, allocate_I8_1D, &
                              allocate_R4_5D, allocate_R4_4D, allocate_R4_3D, allocate_R4_2D, allocate_R4_1D, &
                              allocate_R8_5D, allocate_R8_4D, allocate_R8_3D, allocate_R8_2D, allocate_R8_1D
  end type heap

! tell doxygen to ignore the following block (for now)
!> \cond DOXYGEN_SHOULD_SKIP_THIS
  ! interface sm_allocate   ! generic non type bound procedure
  !   module procedure I1_5D, I1_4D, I1_3D, I1_2D, I1_1D, &   !  8 bit integer functions
  !                    I2_5D, I2_4D, I2_3D, I2_2D, I2_1D, &   ! 16 bit integer functions
  !                    I4_5D, I4_4D, I4_3D, I4_2D, I4_1D, &   ! 32 bit integer functions
  !                    I8_5D, I8_4D, I8_3D, I8_2D, I8_1D, &   ! 64 bit integer functions
  !                    R4_5D, R4_4D, R4_3D, R4_2D, R4_1D, &   ! 32 bit real functions, 
  !                    R8_5D, R8_4D, R8_3D, R8_2D, R8_1D      ! 64 bit real functions
  ! end interface

  interface ptr_to_bm
    module procedure &
      ptr_to_blockmeta_I15D, ptr_to_blockmeta_I14D, ptr_to_blockmeta_I13D, ptr_to_blockmeta_I12D, ptr_to_blockmeta_I11D, &   !  8 bit integer functions
      ptr_to_blockmeta_I25D, ptr_to_blockmeta_I24D, ptr_to_blockmeta_I23D, ptr_to_blockmeta_I22D, ptr_to_blockmeta_I21D, &   ! 16 bit integer functions
      ptr_to_blockmeta_I45D, ptr_to_blockmeta_I44D, ptr_to_blockmeta_I43D, ptr_to_blockmeta_I42D, ptr_to_blockmeta_I41D, &   ! 32 bit integer functions
      ptr_to_blockmeta_I85D, ptr_to_blockmeta_I84D, ptr_to_blockmeta_I83D, ptr_to_blockmeta_I82D, ptr_to_blockmeta_I81D, &   ! 64 bit integer functions
      ptr_to_blockmeta_R45D, ptr_to_blockmeta_R44D, ptr_to_blockmeta_R43D, ptr_to_blockmeta_R42D, ptr_to_blockmeta_R41D, &   ! 32 bit real functions, 
      ptr_to_blockmeta_R85D, ptr_to_blockmeta_R84D, ptr_to_blockmeta_R83D, ptr_to_blockmeta_R82D, ptr_to_blockmeta_R81D      ! 64 bit real functions
  end interface

  interface bm_to_ptr
    module procedure &
      blockmeta_to_ptr_I15D, blockmeta_to_ptr_I14D, blockmeta_to_ptr_I13D, blockmeta_to_ptr_I12D, blockmeta_to_ptr_I11D, &   !  8 bit integer functions
      blockmeta_to_ptr_I25D, blockmeta_to_ptr_I24D, blockmeta_to_ptr_I23D, blockmeta_to_ptr_I22D, blockmeta_to_ptr_I21D, &   ! 16 bit integer functions
      blockmeta_to_ptr_I45D, blockmeta_to_ptr_I44D, blockmeta_to_ptr_I43D, blockmeta_to_ptr_I42D, blockmeta_to_ptr_I41D, &   ! 32 bit integer functions
      blockmeta_to_ptr_I85D, blockmeta_to_ptr_I84D, blockmeta_to_ptr_I83D, blockmeta_to_ptr_I82D, blockmeta_to_ptr_I81D, &   ! 64 bit integer functions
      blockmeta_to_ptr_R45D, blockmeta_to_ptr_R44D, blockmeta_to_ptr_R43D, blockmeta_to_ptr_R42D, blockmeta_to_ptr_R41D, &   ! 32 bit real functions, 
      blockmeta_to_ptr_R85D, blockmeta_to_ptr_R84D, blockmeta_to_ptr_R83D, blockmeta_to_ptr_R82D, blockmeta_to_ptr_R81D      ! 64 bit real functions
  end interface
  
!   ===========================  type bound procedures used by heap user type ===========================
!> \endcond
  contains

  subroutine block_meta_print(this)
    implicit none
    class(block_meta_f08), intent(in) :: this
    print *, 'BLOCK META tkr, kind: ', this % a % tkr, this % get_kind()
  end subroutine block_meta_print

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

  !> get heap statistics
  function GetInfoReg(ix, sz, max, nblk, nbyt) result(status)
    implicit none
    integer(C_INT), intent(IN), value :: ix                 !< index into registered heap table
    integer(C_LONG_LONG), intent(OUT) :: sz                 !< [out] size of heap (bytes)
    integer(C_LONG_LONG), intent(OUT) :: max                !< [out] high water mark in heap  (highest allocation point) (bytes)
    integer(C_LONG_LONG), intent(OUT) :: nblk               !< [out] number of blocks that have been allocated
    integer(C_LONG_LONG), intent(OUT) :: nbyt               !< [out] total number of bytes used by allocated blocks
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    status = ShmemHeapGetInfo(ix, sz, max, nblk, nbyt)
  end function GetInfoReg

  !> get heap statistics
  function GetInfo(h, sz, max, nblk, nbyt) result(status)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_LONG_LONG), intent(OUT) :: sz                 !< [out] size of heap (bytes)
    integer(C_LONG_LONG), intent(OUT) :: max                !< [out] high water mark in heap  (highest allocation point) (bytes)
    integer(C_LONG_LONG), intent(OUT) :: nblk               !< [out] number of blocks that have been allocated
    integer(C_LONG_LONG), intent(OUT) :: nbyt               !< [out] total number of bytes used by allocated blocks
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    integer(C_INT) :: ix
    status = -1
    ix = ShmemHeapIndex(h%p)
    if(ix < 0) return
    status = ShmemHeapGetInfo(ix, sz, max, nblk, nbyt)
  end function GetInfo

  !> dump info about all known heaps
  subroutine dumpinfo
    implicit none
    call ShmemHeapDumpInfo()
  end subroutine dumpinfo

  !> \brief get array type from Fortran block metadata
  function metadata(this, block) result(status)
    implicit none
    class(block_meta_f08), intent(OUT) :: this         !< block object
    type(C_PTR), intent(IN), value :: block            !< address of block
    integer(C_INT) :: status                           !< 0 if O.K., nonzero if error
    integer :: msz
    type(block_meta_c) :: dummy_meta
    msz = C_SIZEOF(dummy_meta)
    status = ShmemHeapGetBlockMeta(block, this%a, msz)
  end function metadata

  function block_meta_get_ptr(this) result(this_ptr)
    implicit none
    class(block_meta_f08), intent(IN) :: this
    type(C_PTR) :: this_ptr
    this_ptr = this % p
  end function block_meta_get_ptr

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
  function get_kind(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array kind (1/2/4/8 bytes)
    n = and(ishft(this%a%tkr,-8), 15)
  end function get_kind
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
  function get_dimensions(this) result(d)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: d     !< array dimensions
    d = this%a%d
  end function get_dimensions
  !> \brief get array dimensions from C block metadata (C callable version)
  subroutine BlockMeta_dims(this, dims) BIND(C,name='BlockMeta_dims')
    implicit none
    type(block_meta_c), intent(IN) :: this              !< block object
    integer(C_INT64_T), intent(OUT), dimension(MAX_ARRAY_RANK) :: dims   !< array dimensions
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
    this%p          = other%p
    this%a%tkr      = other%a%tkr
    this%a%d        = other%a%d
    this%a%offset   = other%a%offset
  end subroutine assign

  !> \brief assignment operator for type block_meta_f08
  subroutine assign_meta(this, other)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata object
    type(block_meta), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    this%p          = other%p
    this%a%tkr      = other%a%tkr
    this%a%d        = other%a%d
    this%a%offset   = other%a%offset
  end subroutine assign_meta

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

  ! include code generated by script shape_f_pointer.sh (multiple versions of allocate)
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
  !> status = h\%free_by_address(addr)
  function free_by_address(addr) result(success)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< address of block to free
    logical :: success                          !< .true. if the operation succeeded, .false. otherwise
    integer(C_INT) :: status                    !< 0 if O.K., nonzero if error
    status  = ShmemHeapFreeBlock(addr)
    success = (status == 0)
  end function free_by_address 
  
  !> \brief free block by offset in heap
  !> <br>example :<br>type(heap) :: h<br>integer(HEAP_ELEMENT) :: offset<br> integer(C_INT) :: status<br>
  !> status = h\%free_by_offset(offset)
  function free_by_offset(h, offset) result(success)
    implicit none
    class(heap), intent(INOUT) :: h                     !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset into heap of block to free
    logical :: success                                  !< .true. if the operation succeeded, .false. otherwise

    integer(C_INT) :: status ! 0 if O.K., nonzero if error
    type(C_PTR)    :: addr

    addr    = ShmemHeapPtr(h%p, offset)
    status  = ShmemHeapFreeBlock(addr)
    success = (status == 0)
  end function free_by_offset 
  
  !> \brief Free block from heap
  !> <br>example :<br>type(block_meta_f08) :: meta08<br> logical :: success<br>
  !> success = meta08\%free()
  function free_by_meta(this) result(success)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this      !< metadata associated to memory block
    logical :: success                                !< .true. if call to underlying C function had no error

    integer(C_INT) :: status                          !< 0 if O.K., nonzero if error
    type(C_PTR)    :: block_address

    block_address = this % p
    status        = ShmemHeapFreeBlock(block_address)
    success       = (status == 0)
  end function free_by_meta 
  
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
  !> <br>example :<br>type(heap) :: h<br>
  !> <br>integer(C_SIZE_T) :: free_space,used_space<br>integer(C_INT) :: status<br>
  !> status = h\%check(free_blocks, free_space, used_blocks, used_space)
  function check(h, free_blocks_out, free_space_out, used_blocks_out, used_space_out) result(is_ok)
    implicit none
    class(heap),       intent(INOUT)         :: h                 !< heap object
    integer(C_INT),    intent(OUT), optional :: free_blocks_out   !< number of free blocks in heap
    integer(C_SIZE_T), intent(OUT), optional :: free_space_out    !< available space in heap (bytes)
    integer(C_INT),    intent(OUT), optional :: used_blocks_out   !< number of used blocks in heap
    integer(C_SIZE_T), intent(OUT), optional :: used_space_out    !< used space in heap (bytes)

    integer(C_INT)    :: free_blocks   !< number of free blocks in heap
    integer(C_INT)    :: used_blocks   !< number of used blocks in heap
    integer(C_SIZE_T) :: free_space    !< available space in heap (bytes)
    integer(C_SIZE_T) :: used_space    !< used space in heap (bytes)
    integer(C_INT)    :: status        !< 0 if O.K., nonzero if error
    logical           :: is_ok

    status = ShmemHeapCheck(h%p, free_blocks, free_space, used_blocks, used_space)
    is_ok = (status == 0)

    if (present(free_blocks_out)) free_blocks_out = free_blocks
    if (present(free_space_out))  free_space_out  = free_space
    if (present(used_blocks_out)) used_blocks_out = used_blocks
    if (present(used_space_out))  used_space_out  = used_space
  end function check 
  
  !> \brief find if address belongs to a registered heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>type(C_PTR) :: p<br>
  !> p = h\%get_heap_from_address(addr)
  function get_heap_from_address(addr) result(p)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    type(C_PTR) :: p                            !< address of heap (NULL if not in a registered heap)
    p = ShmemHeapContains(addr)
  end function get_heap_from_address 
  
  !> \brief Find if given address belongs to a block from a registered heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: status<br>
  !> status = h\%is_valid_block(addr)
  function is_valid_block(addr) result(is_valid)
    implicit none
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    logical :: is_valid                         !< Whether the given address points to a valid block in a known heap
    integer(C_INT) :: status                    !< 0 if valid block from registered heap, 
                                                !< -1 if unknown heap, 
                                                !< 1 if not a proper block pointer
    status = ShmemHeapValidBlock(addr)
    is_valid = (status == 0)
  end function is_valid_block 
  
  !> \brief get the size of a heap
  !> <br>example :<br>type(heap) :: h<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%get_size()
  function get_size(h) result(bsz)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(C_SIZE_T) :: bsz                    !< size if known heap, -1 otherwise
    bsz = ShmemHeapSize(h%p)
  end function get_size
  
  !> \brief get the size code of a heap block
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(HEAP_ELEMENT) :: offset<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%get_blocksize(p, addr, offset)<br>
  !> bsz = h\%get_blocksize(h\%address(0), addr, offset)
  function get_blocksize(p, addr, offset) result(bsz)
    implicit none
    type(C_PTR), intent(IN), value :: p         !< address of heap (if C_NULL_PTR, addr is needed)
    type(C_PTR), intent(IN), value :: addr      !< memory address to check
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset from base of registered heap, 
    integer(C_SIZE_T) :: bsz                    !< size if valid block from known heap,
                                                !< -1 if unknown heap, 
                                                !< 1 if not a proper block pointer
    bsz = ShmemHeapBlockSize(p, addr, offset)
  end function get_blocksize
  
  !> \brief Get offset into a heap for a given memory address
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: off<br>
  !> off = h\%offset(addr)
  ! function get_offset_from_address(addr) result(off)
  !   implicit none
  !   type(C_PTR), intent(IN), value :: addr      !< memory address to check
  !   integer(HEAP_ELEMENT) :: off                !< offset from base of *some* registered heap, -1 if unknown heap
  !   off = ShmemHeapPtrToOffset(addr)
  ! end function get_offset_from_address 
  
  !> \brief get internal pointer(address) to heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p<br>
  !> p = h\%get_ptr()
  function heap_get_ptr(h) result(p)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    type(C_PTR) :: p                            !< internal address, NULL if not initialized
    p = h%p
  end function heap_get_ptr 
  
  !> \brief translate offset in heap into address
  !> <br>example :<br>type(heap) :: h<br>integer(HEAP_ELEMENT) :: offset<br>type(C_PTR) :: p<br>
  !> p = h\%get_address_from_offset(offset)
  function get_address_from_offset(h, offset) result(p)
    implicit none
    class(heap), intent(INOUT) :: h                     !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset into heap
    type(C_PTR) :: p                            !< address, NULL if invalid offset/heap combination
    p = ShmemHeapPtr(h%p, offset)
  end function get_address_from_offset

  function block_meta_get_offset(this) result(offset)
    implicit none
    class(block_meta_f08), intent(inout) :: this
    integer(C_SIZE_T) :: offset !< The offset of this block within its heap
    offset = this % a % offset
  end function block_meta_get_offset

end module heap_module
