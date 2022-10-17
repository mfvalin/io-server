#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sed -e 's/^\(.*\)$/! &/' < ${SCRIPT_DIR}/common_header.txt
echo

MALLOC=ShmemHeap_alloc_block
METADATA=ShmemHeap_set_block_meta
# MALLOC=LocalAllocBlock
# same calling sequence as shared memory heap allocator, but calls malloc
cat <<EOT
!> \file
!  function LocalAllocBlock(heap, nbytes, safe) result(b)
!    implicit none
!    type(C_PTR), intent(IN), value :: heap
!    integer(C_SIZE_T), intent(IN), value :: nbytes
!    integer(C_INT), intent(IN), value :: safe
!    type(C_PTR) :: b
!    b = malloc(nbytes)
!  end function LocalAllocBlock

!! ====================  using ${MALLOC} allocator ====================

EOT
#
# generate allocator code for integer 1/2/4/8 bytes, real 4/8 bytes, 1 to 5 dimensions
#
for RI in I R ; do
  [[ $RI == I ]] && RANGE="1 2 4 8"
  [[ $RI == R ]] && RANGE="4 8"
  [[ $RI == I ]] && PARAM_DECLARE="integer, parameter  :: I = TKR_INTEGER"
  [[ $RI == R ]] && PARAM_DECLARE="integer, parameter  :: R = TKR_REAL"
  for L in $RANGE ; do
    for D in 5 4 3 2 1 ; do
    if [[ $RI == I ]] ; then
      TYPE=integer
      [[ $L == 1 ]] && KIND='C_INT8_T'
      [[ $L == 2 ]] && KIND='C_INT16_T'
      [[ $L == 4 ]] && KIND='C_INT32_T'
      [[ $L == 8 ]] && KIND='C_INT64_T'
    else
      TYPE=real
      [[ $L == 4 ]] && KIND='C_FLOAT'
      [[ $L == 8 ]] && KIND='C_DOUBLE'
    fi
    [[ $D == 5 ]] && DIMENSION=':,:,:,:,:'
    [[ $D == 4 ]] && DIMENSION=':,:,:,:'
    [[ $D == 3 ]] && DIMENSION=':,:,:'
    [[ $D == 2 ]] && DIMENSION=':,:'
    [[ $D == 1 ]] && DIMENSION=':'
    cat <<EOT
!! ===============================  ${TYPE}*${L} ${D}D  ===============================
!> \brief ${TYPE}*${L} ${D}D array allocator
function allocate_${RI}${L}_${D}D(this, array_ptr, dims_in, use_safe_alloc, timeout_ms) result(alloc_info)
  use rpn_extra_module, only: sleep_us
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT64_T), dimension(:), intent(IN) :: dims_in  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  !> When heap is full, how long to wait for space to be freed before declaring failure.
  !> 0 to fail immediately (no wait), -1 to wait indefinitely
  integer(C_INT32_T),  optional     :: timeout_ms

  type(block_meta_c)                :: alloc_info     !< metadata for allocated block

  $TYPE($KIND)      :: pref
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  ${PARAM_DECLARE}

  integer, parameter :: SINGLE_WAIT_TIME_MS = 10
  integer :: i_wait, num_waits

  integer(C_INT64_T), dimension(${D}) :: dims
  integer :: num_dim

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta_c([0,0,0,0,0], 0, 0, SHMEM_HEAP_ALLOC_INVALID)

  dims(:) = 1
  num_dim = min(${D}, size(dims_in))
  dims(1:num_dim) = dims_in(1:num_dim)

  ! Given dimensions not consistent with array rank
  if(size(dims_in) .gt. ${D}) then
    if (.not. all(dims_in(${D}+1:size(dims_in)) == 1)) then
      alloc_info % status = SHMEM_HEAP_INVALID_BOUNDS
      return
    end if
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(dims) <= 0) then
    alloc_info % status = SHMEM_HEAP_INVALID_BOUNDS
    return
  end if

  metadata_size = C_SIZEOF(alloc_info)                          ! size of C metadata
  alloc_size    = PRODUCT(dims)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ! First attempt at allocating block
  ptr_c = ${MALLOC}(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap

  ! Determine max number of times to try allocating
  num_waits = (DEFAULT_ALLOC_TIMEOUT_MS + SINGLE_WAIT_TIME_MS - 1) / SINGLE_WAIT_TIME_MS
  if (present(timeout_ms)) then
    if (timeout_ms < 0) then
      num_waits = huge(num_waits)
    else
      num_waits = (timeout_ms + SINGLE_WAIT_TIME_MS - 1)/ SINGLE_WAIT_TIME_MS
    end if
  end if

  do i_wait = 1, num_waits
    if (C_ASSOCIATED(ptr_c)) exit
    call sleep_us(SINGLE_WAIT_TIME_MS * 1000)
    ptr_c = ${MALLOC}(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  end do

  ! Check whether allocation failed
  if (.not. C_ASSOCIATED(ptr_c) ) then
    alloc_info % status = SHMEM_HEAP_ALLOC_TIMEOUT
    return
  end if

  call C_F_POINTER(ptr_c, array_ptr, [dims]) ! make Fortran array pointer from C pointer

  tkr                       = 256*${L} + 16*${RI} + ${D} ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % tkr          = tkr              ! build C interoperable metadata
  alloc_info % d(:)         = 1                ! set all 5 dimensions to 1
  alloc_info % d(1:num_dim) = dims(:)

  alloc_info % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)      ! minus reference address  (offset in number of heap elements)
  alloc_info % status = SHMEM_HEAP_ALLOC_SUCCESS
  status = ${METADATA}(this % p, ptr_c, alloc_info, metadata_size) ! insert metadata into data block

end function allocate_${RI}${L}_${D}D

!> Wrapper around allocate_${RI}${L}_${D}D to be able to call it and specify dimensions with an array of integer*4
function allocate_${RI}${L}_${D}D_integer(this, array_ptr, di, use_safe_alloc, timeout_ms) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  !> When heap is full, how long to wait for space to be freed before declaring failure.
  !> 0 to fail immediately (no wait), -1 to wait indefinitely
  integer(C_INT32_T),  optional     :: timeout_ms
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8, use_safe_alloc = use_safe_alloc, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8, use_safe_alloc = use_safe_alloc)
    end if
  else
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8)
    end if
  end if
end function allocate_${RI}${L}_${D}D_integer

!> ${TYPE}*${L} ${D}D array allocator (with custom bounds)
function allocate_${RI}${L}_${D}D_bounds(this, array_ptr, min_bound_in, max_bound_in, use_safe_alloc, timeout_ms) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound_in  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound_in  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  !> When heap is full, how long to wait for space to be freed before declaring failure.
  !> 0 to fail immediately (no wait), -1 to wait indefinitely
  integer(C_INT32_T),  optional     :: timeout_ms
  type(block_meta_c)                :: bmi !< metadata for allocated block

  $TYPE($KIND), dimension($DIMENSION), contiguous, pointer :: tmp_ptr

  integer(C_INT64_T), dimension(${D}) :: min_bound, max_bound, array_size
  integer :: min_dim, max_dim

  nullify(array_ptr)
  bmi = block_meta_c([0,0,0,0,0], 0, 0, SHMEM_HEAP_ALLOC_INVALID)

  if (size(min_bound_in) .ne. size(max_bound_in))  then
    bmi % status = SHMEM_HEAP_INVALID_BOUNDS
    return
  end if

  ! Setup min/max bounds in a clean way
  min_bound(:) = 1
  max_bound(:) = 1
  min_dim = min(size(min_bound_in), ${D})
  max_dim = min(size(max_bound_in), ${D})
  min_bound(1:min_dim) = min_bound_in(1:min_dim)
  max_bound(1:max_dim) = max_bound_in(1:max_dim)
  array_size(:) = max_bound - min_bound + 1

  if (present(use_safe_alloc)) then
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size, use_safe_alloc = use_safe_alloc, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size, use_safe_alloc = use_safe_alloc)
    end if
  else
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size)
    end if
  end if

  if (associated(tmp_ptr)) then
    array_ptr($(dims="min_bound(1):max_bound(1)"; for i in $(seq 2 ${D}); do dims="${dims}, min_bound(${i}):max_bound(${i})"; done; echo ${dims})) => &
        tmp_ptr($DIMENSION)
  end if
end function allocate_${RI}${L}_${D}D_bounds

!> Wrapper on ${TYPE}*${L} ${D}D to be able to call it, specifying bounds with integer*4 arrays
function allocate_${RI}${L}_${D}D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc, timeout_ms) result(bmi) ! ${TYPE}*${L} ${D}D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  !> When heap is full, how long to wait for space to be freed before declaring failure.
  !> 0 to fail immediately (no wait), -1 to wait indefinitely
  integer(C_INT32_T),  optional     :: timeout_ms
  type(block_meta_c)                :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc = use_safe_alloc, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc = use_safe_alloc)
    end if
  else
    if (present(timeout_ms)) then
      bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8, timeout_ms = timeout_ms)
    else
      bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8)
    end if
  end if
end function allocate_${RI}${L}_${D}D_bounds_int4

EOT
    done
  done
done
