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
!  function LocalAllocBlock(heap, nbytes, safe) result(b)
!    implicit none
!    type(C_PTR), intent(IN), value :: heap
!    integer(C_SIZE_T), intent(IN), value :: nbytes
!    integer(C_INT), intent(IN), value :: safe
!    type(C_PTR) :: b
!    b = malloc(nbytes)
!  end function LocalAllocBlock

!! ====================  using ShmemHeap_alloc_block allocator ====================

!! ===============================  integer*1 5D  ===============================
!> \brief integer*1 5D array allocator
function allocate_I1_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT8_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*1 + 16*I + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I1_5D

!> Wrapper around allocate_I1_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_I1_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I1_5D(array_ptr, di_int8)
  end if
end function allocate_I1_5D_integer

!> integer*1 5D array allocator (with custom bounds)
function allocate_I1_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT8_T), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I1_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_I1_5D_bounds

!> Wrapper on integer*1 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I1_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*1 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I1_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I1_5D_bounds_int4

!! ===============================  integer*1 4D  ===============================
!> \brief integer*1 4D array allocator
function allocate_I1_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT8_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*1 + 16*I + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I1_4D

!> Wrapper around allocate_I1_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_I1_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I1_4D(array_ptr, di_int8)
  end if
end function allocate_I1_4D_integer

!> integer*1 4D array allocator (with custom bounds)
function allocate_I1_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT8_T), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I1_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_I1_4D_bounds

!> Wrapper on integer*1 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I1_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*1 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I1_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I1_4D_bounds_int4

!! ===============================  integer*1 3D  ===============================
!> \brief integer*1 3D array allocator
function allocate_I1_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT8_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*1 + 16*I + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I1_3D

!> Wrapper around allocate_I1_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_I1_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I1_3D(array_ptr, di_int8)
  end if
end function allocate_I1_3D_integer

!> integer*1 3D array allocator (with custom bounds)
function allocate_I1_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT8_T), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I1_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_I1_3D_bounds

!> Wrapper on integer*1 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I1_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*1 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I1_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I1_3D_bounds_int4

!! ===============================  integer*1 2D  ===============================
!> \brief integer*1 2D array allocator
function allocate_I1_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT8_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*1 + 16*I + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I1_2D

!> Wrapper around allocate_I1_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_I1_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I1_2D(array_ptr, di_int8)
  end if
end function allocate_I1_2D_integer

!> integer*1 2D array allocator (with custom bounds)
function allocate_I1_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT8_T), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I1_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_I1_2D_bounds

!> Wrapper on integer*1 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I1_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*1 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I1_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I1_2D_bounds_int4

!! ===============================  integer*1 1D  ===============================
!> \brief integer*1 1D array allocator
function allocate_I1_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT8_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*1 + 16*I + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I1_1D

!> Wrapper around allocate_I1_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_I1_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I1_1D(array_ptr, di_int8)
  end if
end function allocate_I1_1D_integer

!> integer*1 1D array allocator (with custom bounds)
function allocate_I1_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT8_T), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I1_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_I1_1D_bounds

!> Wrapper on integer*1 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I1_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*1 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT8_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I1_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I1_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I1_1D_bounds_int4

!! ===============================  integer*2 5D  ===============================
!> \brief integer*2 5D array allocator
function allocate_I2_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT16_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*2 + 16*I + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I2_5D

!> Wrapper around allocate_I2_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_I2_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I2_5D(array_ptr, di_int8)
  end if
end function allocate_I2_5D_integer

!> integer*2 5D array allocator (with custom bounds)
function allocate_I2_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT16_T), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I2_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_I2_5D_bounds

!> Wrapper on integer*2 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I2_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*2 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I2_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I2_5D_bounds_int4

!! ===============================  integer*2 4D  ===============================
!> \brief integer*2 4D array allocator
function allocate_I2_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT16_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*2 + 16*I + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I2_4D

!> Wrapper around allocate_I2_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_I2_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I2_4D(array_ptr, di_int8)
  end if
end function allocate_I2_4D_integer

!> integer*2 4D array allocator (with custom bounds)
function allocate_I2_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT16_T), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I2_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_I2_4D_bounds

!> Wrapper on integer*2 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I2_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*2 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I2_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I2_4D_bounds_int4

!! ===============================  integer*2 3D  ===============================
!> \brief integer*2 3D array allocator
function allocate_I2_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT16_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*2 + 16*I + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I2_3D

!> Wrapper around allocate_I2_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_I2_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I2_3D(array_ptr, di_int8)
  end if
end function allocate_I2_3D_integer

!> integer*2 3D array allocator (with custom bounds)
function allocate_I2_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT16_T), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I2_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_I2_3D_bounds

!> Wrapper on integer*2 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I2_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*2 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I2_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I2_3D_bounds_int4

!! ===============================  integer*2 2D  ===============================
!> \brief integer*2 2D array allocator
function allocate_I2_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT16_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*2 + 16*I + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I2_2D

!> Wrapper around allocate_I2_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_I2_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I2_2D(array_ptr, di_int8)
  end if
end function allocate_I2_2D_integer

!> integer*2 2D array allocator (with custom bounds)
function allocate_I2_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT16_T), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I2_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_I2_2D_bounds

!> Wrapper on integer*2 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I2_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*2 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I2_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I2_2D_bounds_int4

!! ===============================  integer*2 1D  ===============================
!> \brief integer*2 1D array allocator
function allocate_I2_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT16_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*2 + 16*I + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I2_1D

!> Wrapper around allocate_I2_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_I2_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I2_1D(array_ptr, di_int8)
  end if
end function allocate_I2_1D_integer

!> integer*2 1D array allocator (with custom bounds)
function allocate_I2_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT16_T), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I2_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_I2_1D_bounds

!> Wrapper on integer*2 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I2_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*2 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT16_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I2_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I2_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I2_1D_bounds_int4

!! ===============================  integer*4 5D  ===============================
!> \brief integer*4 5D array allocator
function allocate_I4_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT32_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*I + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I4_5D

!> Wrapper around allocate_I4_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_I4_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I4_5D(array_ptr, di_int8)
  end if
end function allocate_I4_5D_integer

!> integer*4 5D array allocator (with custom bounds)
function allocate_I4_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT32_T), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I4_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_I4_5D_bounds

!> Wrapper on integer*4 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I4_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*4 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I4_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I4_5D_bounds_int4

!! ===============================  integer*4 4D  ===============================
!> \brief integer*4 4D array allocator
function allocate_I4_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT32_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*I + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I4_4D

!> Wrapper around allocate_I4_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_I4_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I4_4D(array_ptr, di_int8)
  end if
end function allocate_I4_4D_integer

!> integer*4 4D array allocator (with custom bounds)
function allocate_I4_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT32_T), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I4_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_I4_4D_bounds

!> Wrapper on integer*4 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I4_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*4 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I4_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I4_4D_bounds_int4

!! ===============================  integer*4 3D  ===============================
!> \brief integer*4 3D array allocator
function allocate_I4_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT32_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*I + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I4_3D

!> Wrapper around allocate_I4_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_I4_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I4_3D(array_ptr, di_int8)
  end if
end function allocate_I4_3D_integer

!> integer*4 3D array allocator (with custom bounds)
function allocate_I4_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT32_T), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I4_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_I4_3D_bounds

!> Wrapper on integer*4 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I4_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*4 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I4_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I4_3D_bounds_int4

!! ===============================  integer*4 2D  ===============================
!> \brief integer*4 2D array allocator
function allocate_I4_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT32_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*I + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I4_2D

!> Wrapper around allocate_I4_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_I4_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I4_2D(array_ptr, di_int8)
  end if
end function allocate_I4_2D_integer

!> integer*4 2D array allocator (with custom bounds)
function allocate_I4_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT32_T), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I4_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_I4_2D_bounds

!> Wrapper on integer*4 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I4_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*4 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I4_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I4_2D_bounds_int4

!! ===============================  integer*4 1D  ===============================
!> \brief integer*4 1D array allocator
function allocate_I4_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT32_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*I + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I4_1D

!> Wrapper around allocate_I4_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_I4_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I4_1D(array_ptr, di_int8)
  end if
end function allocate_I4_1D_integer

!> integer*4 1D array allocator (with custom bounds)
function allocate_I4_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT32_T), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I4_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_I4_1D_bounds

!> Wrapper on integer*4 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I4_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*4 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT32_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I4_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I4_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I4_1D_bounds_int4

!! ===============================  integer*8 5D  ===============================
!> \brief integer*8 5D array allocator
function allocate_I8_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT64_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*I + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I8_5D

!> Wrapper around allocate_I8_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_I8_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I8_5D(array_ptr, di_int8)
  end if
end function allocate_I8_5D_integer

!> integer*8 5D array allocator (with custom bounds)
function allocate_I8_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I8_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_I8_5D_bounds

!> Wrapper on integer*8 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I8_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*8 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I8_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I8_5D_bounds_int4

!! ===============================  integer*8 4D  ===============================
!> \brief integer*8 4D array allocator
function allocate_I8_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT64_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*I + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I8_4D

!> Wrapper around allocate_I8_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_I8_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I8_4D(array_ptr, di_int8)
  end if
end function allocate_I8_4D_integer

!> integer*8 4D array allocator (with custom bounds)
function allocate_I8_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I8_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_I8_4D_bounds

!> Wrapper on integer*8 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I8_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*8 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I8_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I8_4D_bounds_int4

!! ===============================  integer*8 3D  ===============================
!> \brief integer*8 3D array allocator
function allocate_I8_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT64_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*I + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I8_3D

!> Wrapper around allocate_I8_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_I8_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I8_3D(array_ptr, di_int8)
  end if
end function allocate_I8_3D_integer

!> integer*8 3D array allocator (with custom bounds)
function allocate_I8_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I8_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_I8_3D_bounds

!> Wrapper on integer*8 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I8_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*8 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I8_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I8_3D_bounds_int4

!! ===============================  integer*8 2D  ===============================
!> \brief integer*8 2D array allocator
function allocate_I8_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT64_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*I + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I8_2D

!> Wrapper around allocate_I8_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_I8_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I8_2D(array_ptr, di_int8)
  end if
end function allocate_I8_2D_integer

!> integer*8 2D array allocator (with custom bounds)
function allocate_I8_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I8_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_I8_2D_bounds

!> Wrapper on integer*8 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I8_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*8 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I8_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I8_2D_bounds_int4

!! ===============================  integer*8 1D  ===============================
!> \brief integer*8 1D array allocator
function allocate_I8_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  integer(C_INT64_T)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: I = TKR_INTEGER

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*I + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_I8_1D

!> Wrapper around allocate_I8_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_I8_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_I8_1D(array_ptr, di_int8)
  end if
end function allocate_I8_1D_integer

!> integer*8 1D array allocator (with custom bounds)
function allocate_I8_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_I8_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_I8_1D_bounds

!> Wrapper on integer*8 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_I8_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! integer*8 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  integer(C_INT64_T), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to integer array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_I8_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_I8_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_I8_1D_bounds_int4

!! ===============================  real*4 5D  ===============================
!> \brief real*4 5D array allocator
function allocate_R4_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_FLOAT)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*R + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R4_5D

!> Wrapper around allocate_R4_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_R4_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R4_5D(array_ptr, di_int8)
  end if
end function allocate_R4_5D_integer

!> real*4 5D array allocator (with custom bounds)
function allocate_R4_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_FLOAT), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R4_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_R4_5D_bounds

!> Wrapper on real*4 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R4_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*4 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R4_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R4_5D_bounds_int4

!! ===============================  real*4 4D  ===============================
!> \brief real*4 4D array allocator
function allocate_R4_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_FLOAT)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*R + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R4_4D

!> Wrapper around allocate_R4_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_R4_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R4_4D(array_ptr, di_int8)
  end if
end function allocate_R4_4D_integer

!> real*4 4D array allocator (with custom bounds)
function allocate_R4_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_FLOAT), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R4_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_R4_4D_bounds

!> Wrapper on real*4 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R4_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*4 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R4_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R4_4D_bounds_int4

!! ===============================  real*4 3D  ===============================
!> \brief real*4 3D array allocator
function allocate_R4_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_FLOAT)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*R + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R4_3D

!> Wrapper around allocate_R4_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_R4_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R4_3D(array_ptr, di_int8)
  end if
end function allocate_R4_3D_integer

!> real*4 3D array allocator (with custom bounds)
function allocate_R4_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_FLOAT), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R4_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_R4_3D_bounds

!> Wrapper on real*4 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R4_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*4 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R4_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R4_3D_bounds_int4

!! ===============================  real*4 2D  ===============================
!> \brief real*4 2D array allocator
function allocate_R4_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_FLOAT)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*R + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R4_2D

!> Wrapper around allocate_R4_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_R4_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R4_2D(array_ptr, di_int8)
  end if
end function allocate_R4_2D_integer

!> real*4 2D array allocator (with custom bounds)
function allocate_R4_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_FLOAT), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R4_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_R4_2D_bounds

!> Wrapper on real*4 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R4_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*4 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R4_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R4_2D_bounds_int4

!! ===============================  real*4 1D  ===============================
!> \brief real*4 1D array allocator
function allocate_R4_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_FLOAT)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*4 + 16*R + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R4_1D

!> Wrapper around allocate_R4_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_R4_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R4_1D(array_ptr, di_int8)
  end if
end function allocate_R4_1D_integer

!> real*4 1D array allocator (with custom bounds)
function allocate_R4_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_FLOAT), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R4_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_R4_1D_bounds

!> Wrapper on real*4 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R4_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*4 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_FLOAT), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R4_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R4_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R4_1D_bounds_int4

!! ===============================  real*8 5D  ===============================
!> \brief real*8 5D array allocator
function allocate_R8_5D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_DOUBLE)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 5) then
    if (.not. all(di(5+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*R + 5 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 5) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:5)        = di(1:5)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R8_5D

!> Wrapper around allocate_R8_5D to be able to call it and specify dimensions with an array of integer*4
function allocate_R8_5D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_5D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R8_5D(array_ptr, di_int8)
  end if
end function allocate_R8_5D_integer

!> real*8 5D array allocator (with custom bounds)
function allocate_R8_5D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_DOUBLE), dimension(:,:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_5D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R8_5D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4), min_bound(5):max_bound(5)) => &
        tmp_ptr(:,:,:,:,:)
  end if
end function allocate_R8_5D_bounds

!> Wrapper on real*8 5D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R8_5D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*8 5D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 5 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_5D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R8_5D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R8_5D_bounds_int4

!! ===============================  real*8 4D  ===============================
!> \brief real*8 4D array allocator
function allocate_R8_4D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_DOUBLE)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 4) then
    if (.not. all(di(4+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*R + 4 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 4) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:4)        = di(1:4)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R8_4D

!> Wrapper around allocate_R8_4D to be able to call it and specify dimensions with an array of integer*4
function allocate_R8_4D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_4D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R8_4D(array_ptr, di_int8)
  end if
end function allocate_R8_4D_integer

!> real*8 4D array allocator (with custom bounds)
function allocate_R8_4D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_DOUBLE), dimension(:,:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_4D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R8_4D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3), min_bound(4):max_bound(4)) => &
        tmp_ptr(:,:,:,:)
  end if
end function allocate_R8_4D_bounds

!> Wrapper on real*8 4D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R8_4D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*8 4D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 4 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_4D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R8_4D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R8_4D_bounds_int4

!! ===============================  real*8 3D  ===============================
!> \brief real*8 3D array allocator
function allocate_R8_3D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_DOUBLE)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 3) then
    if (.not. all(di(3+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*R + 3 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 3) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:3)        = di(1:3)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R8_3D

!> Wrapper around allocate_R8_3D to be able to call it and specify dimensions with an array of integer*4
function allocate_R8_3D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_3D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R8_3D(array_ptr, di_int8)
  end if
end function allocate_R8_3D_integer

!> real*8 3D array allocator (with custom bounds)
function allocate_R8_3D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_DOUBLE), dimension(:,:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_3D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R8_3D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2), min_bound(3):max_bound(3)) => &
        tmp_ptr(:,:,:)
  end if
end function allocate_R8_3D_bounds

!> Wrapper on real*8 3D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R8_3D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*8 3D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), contiguous, pointer :: array_ptr !< 3 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_3D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R8_3D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R8_3D_bounds_int4

!! ===============================  real*8 2D  ===============================
!> \brief real*8 2D array allocator
function allocate_R8_2D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_DOUBLE)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 2) then
    if (.not. all(di(2+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*R + 2 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 2) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:2)        = di(1:2)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R8_2D

!> Wrapper around allocate_R8_2D to be able to call it and specify dimensions with an array of integer*4
function allocate_R8_2D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_2D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R8_2D(array_ptr, di_int8)
  end if
end function allocate_R8_2D_integer

!> real*8 2D array allocator (with custom bounds)
function allocate_R8_2D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_DOUBLE), dimension(:,:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_2D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R8_2D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1), min_bound(2):max_bound(2)) => &
        tmp_ptr(:,:)
  end if
end function allocate_R8_2D_bounds

!> Wrapper on real*8 2D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R8_2D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*8 2D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:,:), intent(OUT), contiguous, pointer :: array_ptr !< 2 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_2D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R8_2D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R8_2D_bounds_int4

!! ===============================  real*8 1D  ===============================
!> \brief real*8 1D array allocator
function allocate_R8_1D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  real(C_DOUBLE)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter  :: R = TKR_REAL

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. 1) then
    if (.not. all(di(1+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info)                        ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ShmemHeap_alloc_block(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*8 + 16*R + 1 ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr        = tkr              ! build C interoperable metadata
  alloc_info % d(:)       = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < 1) then
    alloc_info % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % d(1:1)        = di(1:1)
  end if

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  status = ShmemHeap_set_block_meta(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block
end function allocate_R8_1D

!> Wrapper around allocate_R8_1D to be able to call it and specify dimensions with an array of integer*4
function allocate_R8_1D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_1D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_R8_1D(array_ptr, di_int8)
  end if
end function allocate_R8_1D_integer

!> real*8 1D array allocator (with custom bounds)
function allocate_R8_1D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  real(C_DOUBLE), dimension(:), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_1D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_R8_1D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr(min_bound(1):max_bound(1)) => &
        tmp_ptr(:)
  end if
end function allocate_R8_1D_bounds

!> Wrapper on real*8 1D to be able to call it, specifying bounds with integer*4 arrays
function allocate_R8_1D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! real*8 1D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  real(C_DOUBLE), dimension(:), intent(OUT), contiguous, pointer :: array_ptr !< 1 dimensional pointer to real array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_R8_1D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_R8_1D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_R8_1D_bounds_int4

