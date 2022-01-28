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
    
    procedure :: get_ptr    => block_meta_get_ptr    !< \return The stored pointer
    procedure :: get_offset => block_meta_get_offset !< \return The offset of this block within its heap
    procedure :: get_type   => block_meta_get_type   !< \return Array type code (1=integer, 2=real)
    procedure :: get_kind   => block_meta_get_kind   !< \return Array kind (1/2/4/8 bytes)
    procedure :: get_rank   => block_meta_get_rank   !< \return Array rank (1/2/3/../MAX_ARRAY_RANK)
    procedure :: get_dimensions                      !< \return array(MAX_ARRAY_RANK) containing dimensions
    procedure :: retrieve_metadata                   !< Get metadata from what's stored in the heap. \return 0 if O.K. non zero otherwise

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
  contains

    procedure :: createb      !< create, initialize, register a heap at specified address (size in uint64_t bytes). \return address of heap
    GENERIC   :: create => createb !, createw

    procedure :: clone_h      !< Clone a heap object using the address of an existing heap. \return The given address
    GENERIC   :: clone => clone_h

    procedure :: check        !< Check integrity of heap. \return 0 if OK, something else otherwise

    GENERIC :: get_block_size => get_block_size_from_pointer, get_block_size_from_offset
    procedure :: get_block_size_from_pointer
    procedure :: get_block_size_from_offset
    procedure :: is_valid_block           !< \return Whether the given address points to a valid block in this heap

    procedure :: get_address_from_offset  !< \return Block address that corresponds to the given offset in the heap. NULL if error
    procedure :: get_ptr => heap_get_ptr  !< \return Internal pointer to heap

    procedure :: get_size               !< \return Size of heap, -1 if error
    procedure :: alloc                  !< Allocate a block from a heap.  \return block address, NULL if allocation fails

    GENERIC   :: free => free_by_address, free_by_offset, free_by_meta
    procedure :: free_by_address        !< Free an allocated block by address in memory. \return .true. if O.K., .false. if error
    procedure :: free_by_offset         !< Free space associated to offset into heap. \return .true. if O.K., .false. if error
    procedure :: free_by_meta           !< Free block in heap from its metadata. \return .true. if O.K., .false. if error

    procedure :: get_info               !< Get heap statistics using heap address. \return 0 if O.K., nonzero if error
    procedure :: dump_info              !< Dump information about this heap

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

  !> get heap statistics
  function get_info(h, sz, max, nblk, nbyt) result(status)
    implicit none
    class(heap), intent(INOUT) :: h                         !< heap object
    integer(C_LONG_LONG), intent(OUT) :: sz                 !< [out] size of heap (bytes)
    integer(C_LONG_LONG), intent(OUT) :: max                !< [out] high water mark in heap  (highest allocation point) (bytes)
    integer(C_LONG_LONG), intent(OUT) :: nblk               !< [out] number of blocks that have been allocated
    integer(C_LONG_LONG), intent(OUT) :: nbyt               !< [out] total number of bytes used by allocated blocks
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    status = ShmemHeap_get_info(h % p, sz, max, nblk, nbyt)
  end function get_info

  !> dump info about this heap
  subroutine dump_info(this)
    implicit none
    class(heap), intent(inout) :: this
    call ShmemHeap_dump_info(this % p)
  end subroutine dump_info

  !> \brief get array type from Fortran block metadata
  function retrieve_metadata(this, block) result(status)
    implicit none
    class(block_meta_f08), intent(OUT) :: this         !< block object
    type(C_PTR), intent(IN), value :: block            !< address of block
    integer(C_INT) :: status                           !< 0 if O.K., nonzero if error
    integer :: msz
    type(block_meta_c) :: dummy_meta
    msz = C_SIZEOF(dummy_meta)
    status = ShmemHeap_get_block_meta(this % p, block, this%a, msz)
  end function retrieve_metadata

  function block_meta_get_ptr(this) result(this_ptr)
    implicit none
    class(block_meta_f08), intent(IN) :: this
    type(C_PTR) :: this_ptr
    this_ptr = this % p
  end function block_meta_get_ptr

  !> \brief get array type from Fortran block metadata
  function block_meta_get_type(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array type
    n = and(ishft(this%a%tkr,-4), 15)
  end function block_meta_get_type

  !> \brief get array kind from Fortran block metadata
  function block_meta_get_kind(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array kind (1/2/4/8 bytes)
    n = and(ishft(this%a%tkr,-8), 15)
  end function block_meta_get_kind

  !> \brief get array rank from Fortran block metadata
  function block_meta_get_rank(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array rank
    n = and(this%a%tkr, 15)
  end function block_meta_get_rank

  !> \brief get array dimensions from Fortran block metadata
  function get_dimensions(this) result(d)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: d     !< array dimensions
    d = this%a%d
  end function get_dimensions

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
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p, addr<br>integer(C_SIZE_T) :: nbytes<br>
  !> p = h\%createb(addr, nbytes)
  function createb(this, addr, nbytes) result(success)
    implicit none
    class(heap),       intent(INOUT)     :: this            !< heap object
    type(C_PTR),       intent(IN), value :: addr            !< memory address
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the heap
    logical :: success
    this % p = ShmemHeap_init_from_scratch(addr, nbytes)
    success = c_associated(this % p)
  end function createb

  !> \brief create a new heap object using the address of an existing heap (NO SETUP)
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: p, addr<br>
  !> p = h\%clone_h(addr)
  function clone_h(this, addr) result(success)
    implicit none
    class(heap), intent(INOUT) :: this                         !< heap object
    type(C_PTR), intent(IN), value :: addr                  !< memory address (must be an existing heap address)
    logical :: success
    this % p = ShmemHeap_clone(addr)
    success = c_associated(this % p)
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
    p = ShmemHeap_alloc_block(h%p, nbytes, safe)
  end function alloc 
  
  !> \brief free block by address in memory
  function free_by_address(this, block_address) result(success)
    implicit none
    class(heap), intent(inout)     :: this          !< Heap to which the block belongs
    type(C_PTR), intent(IN), value :: block_address !< address of block to free
    logical :: success         !< .true. if the operation succeeded, .false. otherwise
    integer(C_INT) :: status   !< 0 if O.K., nonzero if error
    status  = ShmemHeap_free_block(this % p, block_address)
    success = (status == 0)
  end function free_by_address 
  
  !> \brief free block by offset in heap
  function free_by_offset(this, offset) result(success)
    implicit none
    class(heap),           intent(INOUT)     :: this    !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset into heap of block to free
    logical :: success                                  !< .true. if the operation succeeded, .false. otherwise

    integer(C_INT) :: status ! 0 if O.K., nonzero if error
    type(C_PTR)    :: addr

    addr    = ShmemHeap_ptr_from_offset(this % p, offset)
    status  = ShmemHeap_free_block(this % p, addr)
    success = (status == 0)
  end function free_by_offset 
  
  !> \brief Free block from heap
  function free_by_meta(this, block_info) result(success)
    implicit none
    class(heap),           intent(inout) :: this
    type(block_meta_f08), intent(in)    :: block_info !< metadata associated to memory block
    logical :: success                                !< .true. if call to underlying C function had no error

    integer(C_INT) :: status                          !< 0 if O.K., nonzero if error

    status        = ShmemHeap_free_block(this % p, block_info % get_ptr())
    success       = (status == 0)
  end function free_by_meta 
  
  !> \brief check integrity of a heap
  !> <br>example :<br>type(heap) :: h<br>
  !> <br>integer(C_SIZE_T) :: free_space,used_space<br>integer(C_INT) :: status<br>
  !> status = h\%check(free_blocks, free_space, used_blocks, used_space)
  function check(h, free_blocks_out, free_space_out, used_blocks_out, used_space_out) result(is_ok)
    implicit none
    class(heap),       intent(INOUT)         :: h                 !< heap object
    integer(C_INT),    intent(OUT), optional :: free_blocks_out   !< [out] number of free blocks in heap
    integer(C_SIZE_T), intent(OUT), optional :: free_space_out    !< [out] available space in heap (bytes)
    integer(C_INT),    intent(OUT), optional :: used_blocks_out   !< [out] number of used blocks in heap
    integer(C_SIZE_T), intent(OUT), optional :: used_space_out    !< [out] used space in heap (bytes)

    integer(C_INT)    :: free_blocks   !< number of free blocks in heap
    integer(C_INT)    :: used_blocks   !< number of used blocks in heap
    integer(C_SIZE_T) :: free_space    !< available space in heap (bytes)
    integer(C_SIZE_T) :: used_space    !< used space in heap (bytes)
    integer(C_INT)    :: status        !< 0 if O.K., nonzero if error
    logical           :: is_ok

    status = ShmemHeap_check(h%p, free_blocks, free_space, used_blocks, used_space)
    is_ok = (status == 0)

    if (present(free_blocks_out)) free_blocks_out = free_blocks
    if (present(free_space_out))  free_space_out  = free_space
    if (present(used_blocks_out)) used_blocks_out = used_blocks
    if (present(used_space_out))  used_space_out  = used_space
  end function check 
  
  !> \brief Find if given address belongs to a block from a registered heap
  !> <br>example :<br>type(heap) :: h<br>type(C_PTR) :: addr<br>integer(C_INT) :: status<br>
  !> status = h\%is_valid_block(addr)
  function is_valid_block(this, block_addr) result(is_valid)
    implicit none
    class(heap), intent(inout) :: this
    type(C_PTR), intent(IN), value :: block_addr  !< memory address to check
    logical :: is_valid                           !< Whether the given address points to a valid block in a known heap
    integer(C_INT) :: status                      !< 0 if valid block, -1 if not
    status = ShmemHeap_is_block_valid(this % p, block_addr)
    is_valid = (status == 0)
  end function is_valid_block 
  
  !> \brief get the size of a heap
  !> <br>example :<br>type(heap) :: h<br>integer(C_SIZE_T) :: bsz<br>
  !> bsz = h\%get_size()
  function get_size(h) result(bsz)
    implicit none
    class(heap), intent(INOUT) :: h             !< heap object
    integer(HEAP_ELEMENT)      :: bsz           !< size (in bytes) if heap is OK, -1 otherwise
    bsz = ShmemHeap_get_size(h%p)
  end function get_size

  !> Retrieve the size of a block, given its address
  function get_block_size_from_pointer(this, block) result(num_bytes)
    implicit none
    class(heap), intent(inout)     :: this
    type(C_PTR), intent(in), value :: block !< Address of the block we are querying
    integer(C_SIZE_T) :: num_bytes !< Size of the block in bytes, 0 if there was an error (invalid heap or block offset)
    num_bytes = ShmemHeap_block_size_from_pointer(this % p, block)
  end function get_block_size_from_pointer

  !> Retrieve the size of a block, given its offset in a heap
  function get_block_size_from_offset(this, offset) result(num_bytes)
    implicit none
    class(heap),           intent(inout) :: this   !< [in,out] This heap
    integer(HEAP_ELEMENT), intent(in)    :: offset !< [in] Offset of the block we're querying
    integer(C_SIZE_T) :: num_bytes !< Size of the block in bytes. 0 if there was an error (invalid heap or block offset)
    num_bytes = ShmemHeap_block_size_from_offset(this % p, offset)
  end function get_block_size_from_offset
  
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
  function get_address_from_offset(this, offset) result(p)
    implicit none
    class(heap),           intent(INOUT)     :: this    !< heap object
    integer(HEAP_ELEMENT), intent(IN), value :: offset  !< offset into heap
    type(C_PTR) :: p                            !< address, NULL if invalid offset/heap combination
    p = ShmemHeap_ptr_from_offset(this % p, offset)
  end function get_address_from_offset

  !> Get the offset of the allocated block within its heap
  function block_meta_get_offset(this) result(offset)
    implicit none
    class(block_meta_f08), intent(in) :: this
    integer(C_SIZE_T) :: offset !< The offset of this block within its heap
    offset = this % a % offset
  end function block_meta_get_offset

end module heap_module
