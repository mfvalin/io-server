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
module shmem_heap_module
  use ISO_C_BINDING
  implicit none

  private

  integer, parameter, public :: HEAP_ELEMENT =  C_INT64_T !<  type of a heap element (must be consistent with C code)
  integer, parameter, public :: MAX_ARRAY_RANK = 5        !< maximum number of allowed dimensions for an array in this heap type
  integer, parameter         :: TKR_INTEGER = 1           !< tkr code for integer arrays
  integer, parameter         :: TKR_REAL    = 2           !< tkr code for real arrays

  !   ===========================  metadata types and type bound procedures ===========================
  !> \brief C interoperable data block metadata
  type, bind(C) :: block_meta_c
    private
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: d  = [0, 0, 0, 0, 0] !< array dimensions \private
    integer(C_INT)    :: tkr = 0           !< array type, kind, rank \private
    integer(C_SIZE_T) :: offset = 0        !< offset in bytes from reference address (memory arena most likely) \private
  end type block_meta_c

  include 'io-server/shmem_heap.inc'

  !> \brief Fortran 2008 data block metadata (using the C layout)
  type, public :: block_meta_f08
    private
    type(block_meta_c) :: a           !< array descriptor, C interoperable \private

  contains
    procedure :: print => block_meta_print !< \copydoc shmem_heap_module::block_meta_print

    procedure :: get_offset => block_meta_get_offset !< \copydoc shmem_heap_module::block_meta_get_offset
    procedure :: get_type   => block_meta_get_type   !< \copydoc block_meta_get_type
    procedure :: get_kind   => block_meta_get_kind   !< \copydoc block_meta_get_kind
    procedure :: get_rank   => block_meta_get_rank   !< \copydoc block_meta_get_rank
    procedure :: get_dimensions                      !< \copydoc shmem_heap_module::get_dimensions

    procedure :: reset                      !< \copydoc shmem_heap_module::reset
    procedure :: assign                     !< \copydoc shmem_heap_module::assign
    procedure :: assign_c                   !< \copydoc shmem_heap_module::assign_c
    GENERIC :: ASSIGNMENT(=) => assign, assign_c      !< generic assignment operator

    procedure :: equal                      !< \copydoc shmem_heap_module::equal
    GENERIC :: operator(==) => equal        !< \copydoc shmem_heap_module::equal
    procedure :: unequal_meta               !< \copydoc shmem_heap_module::unequal_meta
    GENERIC :: operator(/=) => unequal_meta !< \copydoc shmem_heap_module::unequal_meta
  end type block_meta_f08

  type, public :: heap_stats
    integer(C_INT64_T) :: size_byte           !< Total size of heap (bytes)
    integer(C_INT64_T) :: max_fill_byte       !< High water mark in heap  (highest allocation point) (bytes)
    integer(C_INT64_T) :: total_alloc_block   !< Total number of blocks that have been allocated over the life of the heap
    integer(C_INT64_T) :: total_alloc_byte    !< Total number of bytes used by allocated blocks over the life of the heap
  end type heap_stats

!   ===========================  heap type and type bound procedures ===========================
  !> \brief shmem_heap user defined type
  type, public :: shmem_heap
    private
    type(C_PTR) :: p   = C_NULL_PTR      !< address of storage used by shmem_heap \private
  contains

    procedure :: createb      !< \copydoc shmem_heap_module::createb
    GENERIC   :: create => createb !< \copydoc shmem_heap_module::createb

    procedure :: clone_h      !< \copydoc shmem_heap_module::clone_h
    GENERIC   :: clone => clone_h !< \copydoc shmem_heap_module::clone_h

    procedure :: check        !< \copydoc shmem_heap_module::check

    GENERIC :: get_block_size => get_block_size_from_pointer, get_block_size_from_offset
    procedure :: get_block_size_from_pointer !< \copydoc shmem_heap_module::get_block_size_from_pointer
    procedure :: get_block_size_from_offset  !< \copydoc shmem_heap_module::get_block_size_from_offset
    procedure :: is_valid_block           !< \copydoc shmem_heap_module::is_valid_block

    procedure :: get_address_from_offset  !< \copydoc shmem_heap_module::get_address_from_offset
    procedure :: get_ptr => heap_get_ptr  !< \copydoc shmem_heap_module::head_get_ptr

    procedure :: get_size               !< \copydoc shmem_heap_module::get_size
    procedure :: alloc                  !< \copydoc shmem_heap_module::alloc

    GENERIC   :: free => free_by_address, free_by_offset, free_by_meta
    procedure :: free_by_address        !< \copydoc shmem_heap_module::free_by_address
    procedure :: free_by_offset         !< \copydoc shmem_heap_module::free_by_offset
    procedure :: free_by_meta           !< \copydoc shmem_heap_module::free_by_meta

    procedure :: get_stats              !< \copydoc shmem_heap_module::get_stats
    procedure :: dump_info              !< \copydoc shmem_heap_module::dump_info

!> \cond DOXYGEN_SHOULD_SKIP_THIS
!   ===========================  interfaces to script generated functions  ===========================
    procedure   ::            &  !< specific procedures needed for generic type-bound allocator (specify array size)
                              allocate_I1_5D, allocate_I1_4D, allocate_I1_3D, allocate_I1_2D, allocate_I1_1D, &
                              allocate_I2_5D, allocate_I2_4D, allocate_I2_3D, allocate_I2_2D, allocate_I2_1D, &
                              allocate_I4_5D, allocate_I4_4D, allocate_I4_3D, allocate_I4_2D, allocate_I4_1D, &
                              allocate_I8_5D, allocate_I8_4D, allocate_I8_3D, allocate_I8_2D, allocate_I8_1D, &
                              allocate_R4_5D, allocate_R4_4D, allocate_R4_3D, allocate_R4_2D, allocate_R4_1D, &
                              allocate_R8_5D, allocate_R8_4D, allocate_R8_3D, allocate_R8_2D, allocate_R8_1D, &
                              allocate_I1_5D_integer, allocate_I1_4D_integer, allocate_I1_3D_integer, allocate_I1_2D_integer, allocate_I1_1D_integer, &
                              allocate_I2_5D_integer, allocate_I2_4D_integer, allocate_I2_3D_integer, allocate_I2_2D_integer, allocate_I2_1D_integer, &
                              allocate_I4_5D_integer, allocate_I4_4D_integer, allocate_I4_3D_integer, allocate_I4_2D_integer, allocate_I4_1D_integer, &
                              allocate_I8_5D_integer, allocate_I8_4D_integer, allocate_I8_3D_integer, allocate_I8_2D_integer, allocate_I8_1D_integer, &
                              allocate_R4_5D_integer, allocate_R4_4D_integer, allocate_R4_3D_integer, allocate_R4_2D_integer, allocate_R4_1D_integer, &
                              allocate_R8_5D_integer, allocate_R8_4D_integer, allocate_R8_3D_integer, allocate_R8_2D_integer, allocate_R8_1D_integer 

    procedure   ::            &  !< specific procedures needed for generic type-bound allocator (specify array bounds)
                              allocate_I1_5D_bounds, allocate_I1_4D_bounds, allocate_I1_3D_bounds, allocate_I1_2D_bounds, allocate_I1_1D_bounds, &
                              allocate_I2_5D_bounds, allocate_I2_4D_bounds, allocate_I2_3D_bounds, allocate_I2_2D_bounds, allocate_I2_1D_bounds, &
                              allocate_I4_5D_bounds, allocate_I4_4D_bounds, allocate_I4_3D_bounds, allocate_I4_2D_bounds, allocate_I4_1D_bounds, &
                              allocate_I8_5D_bounds, allocate_I8_4D_bounds, allocate_I8_3D_bounds, allocate_I8_2D_bounds, allocate_I8_1D_bounds, &
                              allocate_R4_5D_bounds, allocate_R4_4D_bounds, allocate_R4_3D_bounds, allocate_R4_2D_bounds, allocate_R4_1D_bounds, &
                              allocate_R8_5D_bounds, allocate_R8_4D_bounds, allocate_R8_3D_bounds, allocate_R8_2D_bounds, allocate_R8_1D_bounds, &
                              allocate_I1_5D_bounds_int4, allocate_I1_4D_bounds_int4, allocate_I1_3D_bounds_int4, allocate_I1_2D_bounds_int4, allocate_I1_1D_bounds_int4, &
                              allocate_I2_5D_bounds_int4, allocate_I2_4D_bounds_int4, allocate_I2_3D_bounds_int4, allocate_I2_2D_bounds_int4, allocate_I2_1D_bounds_int4, &
                              allocate_I4_5D_bounds_int4, allocate_I4_4D_bounds_int4, allocate_I4_3D_bounds_int4, allocate_I4_2D_bounds_int4, allocate_I4_1D_bounds_int4, &
                              allocate_I8_5D_bounds_int4, allocate_I8_4D_bounds_int4, allocate_I8_3D_bounds_int4, allocate_I8_2D_bounds_int4, allocate_I8_1D_bounds_int4, &
                              allocate_R4_5D_bounds_int4, allocate_R4_4D_bounds_int4, allocate_R4_3D_bounds_int4, allocate_R4_2D_bounds_int4, allocate_R4_1D_bounds_int4, &
                              allocate_R8_5D_bounds_int4, allocate_R8_4D_bounds_int4, allocate_R8_3D_bounds_int4, allocate_R8_2D_bounds_int4, allocate_R8_1D_bounds_int4 
!> \endcond

    !> Generic Fortran array type bound allocator.
    !> There are two available interfaces: one that specifies the size of the desired array (in each dimension), and
    !> the other that specifies the min and max bounds of the desired array (in each dimension). For both interfaces,
    !> two optional parameters are available
    !>   - to choose a "multi-process" safe allocation
    !>   - to choose a timeout, in case the heap is full and we want to wait for space to become available. The 
    !>     default timeout is 30 seconds. Setting the timeout to zero will make the function return immediately
    !>     with an error if the heap is full. Setting it to a negative value will make the function wait until
    !>     the requested space becomes available.
    !>
    !> For example:
    !> ```
    !> type(shmem_heap)     :: h
    !> type(block_meta_f08) :: info
    !> integer, dimension(:),       contiguous, pointer :: i1, i1_nowait, i1_safe
    !> real,    dimension(:, :, :), contiguous, pointer :: r3
    !> 
    !> info = h % allocate(i1, [50])                                              ! Allocate a 1D array of 50 integers
    !> info = h % allocate(r3, min_bound = [0, 0, 5], max_bound = [49, 49, 100])  ! Allocate a 3D array of 50x50x96 reals
    !> info = h % allocate(i1_nowait, [5], [34], timeout_ms = 0)                  ! Allocate a 1D array of 30 integers, and return immediately if the heap is full
    !> info = h % allocate(i1_safe, [40], safe = .true.)                          ! Allocate a 1D array of 40 integers safely, in case other processes are allocating from the same heap
    !> !
    !> ```
    !> \return     a fortran pointer to integer and real arrays of 1 to MAX_ARRAY_RANK dimension (see f_alloc.inc)
    GENERIC   :: allocate =>  &
                              allocate_I1_5D, allocate_I1_4D, allocate_I1_3D, allocate_I1_2D, allocate_I1_1D, &
                              allocate_I2_5D, allocate_I2_4D, allocate_I2_3D, allocate_I2_2D, allocate_I2_1D, &
                              allocate_I4_5D, allocate_I4_4D, allocate_I4_3D, allocate_I4_2D, allocate_I4_1D, &
                              allocate_I8_5D, allocate_I8_4D, allocate_I8_3D, allocate_I8_2D, allocate_I8_1D, &
                              allocate_R4_5D, allocate_R4_4D, allocate_R4_3D, allocate_R4_2D, allocate_R4_1D, &
                              allocate_R8_5D, allocate_R8_4D, allocate_R8_3D, allocate_R8_2D, allocate_R8_1D, &
                              allocate_I1_5D_integer, allocate_I1_4D_integer, allocate_I1_3D_integer, allocate_I1_2D_integer, allocate_I1_1D_integer, &
                              allocate_I2_5D_integer, allocate_I2_4D_integer, allocate_I2_3D_integer, allocate_I2_2D_integer, allocate_I2_1D_integer, &
                              allocate_I4_5D_integer, allocate_I4_4D_integer, allocate_I4_3D_integer, allocate_I4_2D_integer, allocate_I4_1D_integer, &
                              allocate_I8_5D_integer, allocate_I8_4D_integer, allocate_I8_3D_integer, allocate_I8_2D_integer, allocate_I8_1D_integer, &
                              allocate_R4_5D_integer, allocate_R4_4D_integer, allocate_R4_3D_integer, allocate_R4_2D_integer, allocate_R4_1D_integer, &
                              allocate_R8_5D_integer, allocate_R8_4D_integer, allocate_R8_3D_integer, allocate_R8_2D_integer, allocate_R8_1D_integer, &
                              allocate_I1_5D_bounds, allocate_I1_4D_bounds, allocate_I1_3D_bounds, allocate_I1_2D_bounds, allocate_I1_1D_bounds, &
                              allocate_I2_5D_bounds, allocate_I2_4D_bounds, allocate_I2_3D_bounds, allocate_I2_2D_bounds, allocate_I2_1D_bounds, &
                              allocate_I4_5D_bounds, allocate_I4_4D_bounds, allocate_I4_3D_bounds, allocate_I4_2D_bounds, allocate_I4_1D_bounds, &
                              allocate_I8_5D_bounds, allocate_I8_4D_bounds, allocate_I8_3D_bounds, allocate_I8_2D_bounds, allocate_I8_1D_bounds, &
                              allocate_R4_5D_bounds, allocate_R4_4D_bounds, allocate_R4_3D_bounds, allocate_R4_2D_bounds, allocate_R4_1D_bounds, &
                              allocate_R8_5D_bounds, allocate_R8_4D_bounds, allocate_R8_3D_bounds, allocate_R8_2D_bounds, allocate_R8_1D_bounds, &
                              allocate_I1_5D_bounds_int4, allocate_I1_4D_bounds_int4, allocate_I1_3D_bounds_int4, allocate_I1_2D_bounds_int4, allocate_I1_1D_bounds_int4, &
                              allocate_I2_5D_bounds_int4, allocate_I2_4D_bounds_int4, allocate_I2_3D_bounds_int4, allocate_I2_2D_bounds_int4, allocate_I2_1D_bounds_int4, &
                              allocate_I4_5D_bounds_int4, allocate_I4_4D_bounds_int4, allocate_I4_3D_bounds_int4, allocate_I4_2D_bounds_int4, allocate_I4_1D_bounds_int4, &
                              allocate_I8_5D_bounds_int4, allocate_I8_4D_bounds_int4, allocate_I8_3D_bounds_int4, allocate_I8_2D_bounds_int4, allocate_I8_1D_bounds_int4, &
                              allocate_R4_5D_bounds_int4, allocate_R4_4D_bounds_int4, allocate_R4_3D_bounds_int4, allocate_R4_2D_bounds_int4, allocate_R4_1D_bounds_int4, &
                              allocate_R8_5D_bounds_int4, allocate_R8_4D_bounds_int4, allocate_R8_3D_bounds_int4, allocate_R8_2D_bounds_int4, allocate_R8_1D_bounds_int4 
  end type shmem_heap

  contains

  !> Print metadata content
  subroutine block_meta_print(this)
    implicit none
    class(block_meta_f08), intent(in) :: this !< block_meta instance
    print *, this % get_offset(), this % get_type(), this % get_kind(), this % get_rank(), this % get_dimensions()
  end subroutine block_meta_print

  !> Get heap statistics
  function get_stats(h, stats) result(success)
    implicit none
    class(shmem_heap),    intent(INOUT) :: h                !< heap instance
    type(heap_stats),     intent(OUT)   :: stats            !< [out] The returned stats
    logical :: success
    integer(C_INT) :: status                                !< 0 if O.K., nonzero if error
    status = ShmemHeap_get_info(h % p, stats % size_byte, stats % max_fill_byte, stats % total_alloc_block, stats % total_alloc_byte)
    success = (status == 0)
  end function get_stats

  !> Dump info about this heap
  subroutine dump_info(this)
    implicit none
    class(shmem_heap), intent(inout) :: this !< shmem_heap instance
    call ShmemHeap_dump_info(this % p)
  end subroutine dump_info

  !> \brief Get array type from Fortran block metadata
  function block_meta_get_type(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array type code
    n = and(ishft(this%a%tkr,-4), 15)
  end function block_meta_get_type

  !> \brief Get array kind from Fortran block metadata
  function block_meta_get_kind(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array kind (1/2/4/8 bytes)
    n = and(ishft(this%a%tkr,-8), 15)
  end function block_meta_get_kind

  !> \brief Get array rank from Fortran block metadata
  function block_meta_get_rank(this) result(n)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT) :: n                                !< array rank
    n = and(this%a%tkr, 15)
  end function block_meta_get_rank

  !> \brief Get array dimensions from Fortran block metadata
  function get_dimensions(this) result(d)
    implicit none
    class(block_meta_f08), intent(IN) :: this              !< block object
    integer(C_INT64_T), dimension(MAX_ARRAY_RANK) :: d     !< array dimensions
    d = this%a%d
  end function get_dimensions

  !> \brief Nullify operator for type block_meta_f08
  subroutine reset(this)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata object
    this % a % tkr    = 0
    this % a % d      = 0
    this % a % offset = 0
  end subroutine reset

  !> \brief Assignment operator for type block_meta_f08
  subroutine assign(this, other)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata object
    type(block_meta_f08),  intent(IN)    :: other             !< metadata object assigned to this (this = other)
    this % a % tkr      = other % a % tkr
    this % a % d        = other % a % d
    this % a % offset   = other % a % offset
  end subroutine assign

  !> \brief Assignment operator for type block_meta_f08
  subroutine assign_c(this, other)
    implicit none
    class(block_meta_f08), intent(INOUT) :: this              !< metadata instance
    type(block_meta_c),    intent(IN)    :: other             !< metadata object assigned to this (this = other)
    this % a % tkr      = other % tkr
    this % a % d        = other % d
    this % a % offset   = other % offset
  end subroutine assign_c

  !> \brief Equality operator for type block_meta_f08
  function equal(this, other) result(isequal)
    implicit none
    class(block_meta_f08), intent(IN)    :: this              !< metadata object
    type(block_meta_f08), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    logical :: isequal                                        !< true if equal
    isequal = (this % a % tkr == other % a % tkr)
    isequal = isequal .and. (all(this % a % d == other % a % d))
  end function equal

  !> \brief Non equality operator for type block_meta_f08
  function unequal_meta(this, other) result(is_unequal)
    implicit none
    class(block_meta_f08), intent(IN)    :: this              !< metadata object
    type(block_meta_f08), intent(IN)     :: other             !< metadata object assigned to this (this = other)
    logical :: is_unequal                                     !< true if unequal
    is_unequal = (this % a % tkr /= other % a % tkr)
    is_unequal = is_unequal .or. any(this % a % d /= other % a % d)
  end function unequal_meta

  ! include code generated by script shape_f_pointer.sh (multiple versions of allocate)
  include 'io-server/f_alloc.inc'

  !> \brief Create, initialize, and register a heap
  !> 
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> type(C_PTR) :: p, addr
  !> integer(C_SIZE_T) :: nbytes
  !> p = h % createb(addr, nbytes)
  !> !
  !> ```
  function createb(this, addr, nbytes) result(success)
    implicit none
    class(shmem_heap), intent(INOUT)     :: this            !< shmem_heap instance
    type(C_PTR),       intent(IN), value :: addr            !< memory address
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< size in bytes of the heap
    logical :: success
    this % p = ShmemHeap_init_from_scratch(addr, nbytes)
    success = c_associated(this % p)
  end function createb

  !> \brief Create a new shmem_heap object using the address of an existing heap (NO SETUP)
  !>
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> type(C_PTR) :: p, addr
  !> p = h % clone_h(addr)
  !> !
  !> ```
  function clone_h(this, addr) result(success)
    implicit none
    class(shmem_heap), intent(INOUT) :: this                  !< shmem_heap instance
    type(C_PTR), intent(IN), value   :: addr                  !< memory address (must be an existing heap address)
    logical :: success !< Whether the returned pointer is actually associated (i.e. not NULL)
    this % p = ShmemHeap_clone(addr)
    success = c_associated(this % p)
  end function clone_h

  !> \brief Allocate a memory block in the heap.
  !>
  !> Example :
  !> ```
  !> type(shmem_heap) :: h
  !> type(C_PTR) :: p 
  !> integer(C_SIZE_T) :: nbytes
  !> p = h % alloc(nbytes, 0)
  !> ```
  function alloc(h, nbytes, safe) result(p)
    implicit none
    class(shmem_heap), intent(INOUT)     :: h               !< shmem_heap instance
    integer(C_SIZE_T), intent(IN), value :: nbytes          !< Size in bytes of the desired block
    integer(C_INT),    intent(IN), value :: safe            !< If nonzero perform operation under lock (atomic operation)
    type(C_PTR) :: p                                        !< Address of created heap (C_PTR)
    p = ShmemHeap_alloc_block(h%p, nbytes, safe)
  end function alloc 
  
  !> \brief free block by address in memory
  function free_by_address(this, block_address) result(success)
    implicit none
    class(shmem_heap), intent(inout) :: this          !< Heap to which the block belongs
    type(C_PTR), intent(IN), value   :: block_address !< address of block to free
    logical :: success         !< .true. if the operation succeeded, .false. otherwise
    integer(C_INT) :: status   !< 0 if O.K., nonzero if error
    status  = ShmemHeap_free_block(this % p, block_address)
    success = (status == 0)
  end function free_by_address 
  
  !> \brief Free block by offset in heap
  function free_by_offset(this, offset) result(success)
    implicit none
    class(shmem_heap),     intent(INOUT)     :: this    !< shmem_heap instance
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
    class(shmem_heap),    intent(inout) :: this       !< shmem_heap instance
    type(block_meta_f08), intent(in)    :: block_info !< metadata associated to memory block
    logical :: success                                !< .true. if call to underlying C function had no error
    success = this % free(block_info % get_offset())
  end function free_by_meta 
  
  !> \brief Check integrity of a heap
  !>
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> integer(C_SIZE_T) :: free_space, used_space
  !> integer(C_INT) :: status
  !> status = h % check(free_blocks, free_space, used_blocks, used_space)
  !> !
  !> ```
  function check(h, free_blocks_out, free_space_out, used_blocks_out, used_space_out) result(is_ok)
    implicit none
    class(shmem_heap), intent(INOUT)         :: h                 !< shmem_heap instance
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
  
  !> \brief Find if given address belongs to a block from the given shmem_heap
  !> 
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> type(C_PTR) :: addr
  !> integer(C_INT) :: status
  !> status = h\%is_valid_block(addr)
  !> !
  !> ```
  function is_valid_block(this, block_addr) result(is_valid)
    implicit none
    class(shmem_heap), intent(inout) :: this        !< shmem_heap instance
    type(C_PTR), intent(IN), value   :: block_addr  !< memory address to check
    logical :: is_valid                             !< Whether the given address points to a valid block in the given heap
    integer(C_INT) :: status                        !< 0 if valid block, -1 if not
    status = ShmemHeap_is_block_valid(this % p, block_addr)
    is_valid = (status == 0)
  end function is_valid_block 
  
  !> \brief Get the size of a heap
  !> 
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> integer(C_SIZE_T) :: bsz
  !> bsz = h % get_size()
  !> !
  !> ```
  function get_size(h) result(bsz)
    implicit none
    class(shmem_heap), intent(INOUT) :: h             !< shmem_heap instance
    integer(HEAP_ELEMENT)            :: bsz           !< size (in bytes) if heap is OK, -1 otherwise
    bsz = ShmemHeap_get_size(h%p)
  end function get_size

  !> Retrieve the size of a block, given its address
  function get_block_size_from_pointer(this, block) result(num_bytes)
    implicit none
    class(shmem_heap), intent(inout) :: this  !< shmem_heap instance
    type(C_PTR), intent(in), value   :: block !< Address of the block we are querying
    integer(C_SIZE_T) :: num_bytes !< Size of the block in bytes, 0 if there was an error (invalid heap or block offset)
    num_bytes = ShmemHeap_block_size_from_pointer(this % p, block)
  end function get_block_size_from_pointer

  !> Retrieve the size of a block, given its offset in the given heap
  function get_block_size_from_offset(this, offset) result(num_bytes)
    implicit none
    class(shmem_heap),     intent(inout) :: this   !< [in,out] shmem_heap instance
    integer(HEAP_ELEMENT), intent(in)    :: offset !< [in] Offset of the block we're querying
    integer(C_SIZE_T) :: num_bytes !< Size of the block in bytes. 0 if there was an error (invalid heap or block offset)
    num_bytes = ShmemHeap_block_size_from_offset(this % p, offset)
  end function get_block_size_from_offset
  
  !> \brief Get internal pointer (address) to heap
  !> 
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> type(C_PTR) :: p
  !> p = h\%get_ptr()
  !> !
  !> ```
  function heap_get_ptr(h) result(p)
    implicit none
    class(shmem_heap), intent(inout) :: h       !< shmem_heap instance
    type(C_PTR) :: p                            !< internal address as a C_PTR, NULL if not initialized
    p = h%p
  end function heap_get_ptr 
  
  !> \brief Translate offset in heap into absolute address (C_PTR)
  !>
  !> Example:
  !> ```
  !> type(shmem_heap) :: h
  !> integer(HEAP_ELEMENT) :: offset
  !> type(C_PTR) :: p
  !> p = h % get_address_from_offset(offset)
  !> !
  !> ```
  function get_address_from_offset(this, offset) result(p)
    implicit none
    class(shmem_heap),     intent(inout)     :: this    !< shmem_heap instance
    integer(HEAP_ELEMENT), intent(in), value :: offset  !< offset into heap
    type(C_PTR) :: p     !< Block address that corresponds to the given offset in the heap, NULL if invalid offset/heap combination
    p = ShmemHeap_ptr_from_offset(this % p, offset)
  end function get_address_from_offset

  !> Get the offset of the allocated block within its heap
  function block_meta_get_offset(this) result(offset)
    implicit none
    class(block_meta_f08), intent(in) :: this
    integer(C_SIZE_T) :: offset !< The offset of this block within its heap
    offset = this % a % offset
  end function block_meta_get_offset

end module shmem_heap_module
