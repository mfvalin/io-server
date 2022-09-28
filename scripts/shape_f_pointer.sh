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
function allocate_${RI}${L}_${D}D(this, array_ptr, di, use_safe_alloc) result(alloc_info)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta)                  :: alloc_info     !< metadata for allocated block

  $TYPE($KIND)      :: pref
  type(block_meta_c)  :: alloc_info_c
  type(C_PTR)         :: ptr_c
  integer(C_SIZE_T)   :: alloc_size
  integer             :: tkr, status
  integer             :: metadata_size
  integer(C_INT)      :: safe_alloc_flag
  ${PARAM_DECLARE}

  nullify(array_ptr)                        ! in case of allocation failure
  alloc_info = block_meta(block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  ! Given dimensions not consistent with array rank
  if(size(di) .gt. ${D}) then
    if (.not. all(di(${D}+1:size(di)) == 1)) return
  end if

  ! Gotta allocate more than zero
  if (PRODUCT(di) <= 0) return

  metadata_size = C_SIZEOF(alloc_info_c)                      ! size of C metadata
  alloc_size    = PRODUCT(di)*C_SIZEOF(pref) + metadata_size  ! size of data + size of C metadata

  ! Determine value of "safe allocation" flag to use
  safe_alloc_flag = 0
  if (present(use_safe_alloc)) then
    if (use_safe_alloc) safe_alloc_flag = 1
  end if

  ptr_c = ${MALLOC}(this % p, alloc_size, safe_alloc_flag) ! allocate block in heap
  if(.not. C_ASSOCIATED(ptr_c) ) return        ! allocation failed

  call C_F_POINTER(ptr_c, array_ptr, [di]) ! make Fortran array pointer from C pointer

  tkr                     = 256*${L} + 16*${RI} + ${D} ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  alloc_info % a % tkr    = tkr              ! build C interoperable metadata
  alloc_info % a % d(:)   = 1                ! set all 5 dimensions to 1
  ! set relevant dimensions to correct value
  if (size(di) < ${D}) then
    alloc_info % a % d(1:size(di)) = di(1:size(di))
  else
    alloc_info % a % d(1:${D})        = di(1:${D})
  end if

  alloc_info % a % offset = ShmemHeap_offset_from_pointer(this % p, ptr_c)          ! minus reference address  (offset in number of heap elements)
  alloc_info % p          = ptr_c                                                   ! actual address of array
  status = ${METADATA}(this % p, ptr_c, alloc_info % a, metadata_size) ! insert metadata into data block
end function allocate_${RI}${L}_${D}D

!> Wrapper around allocate_${RI}${L}_${D}D to be able to call it and specify dimensions with an array of integer*4
function allocate_${RI}${L}_${D}D_integer(this, array_ptr, di, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT32_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta)                  :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(di)) :: di_int8
  di_int8(:) = di(:)

  if (present(use_safe_alloc)) then
    bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8, use_safe_alloc)
  else
    bmi = this % allocate_${RI}${L}_${D}D(array_ptr, di_int8)
  end if
end function allocate_${RI}${L}_${D}D_integer

!> ${TYPE}*${L} ${D}D array allocator (with custom bounds)
function allocate_${RI}${L}_${D}D_bounds(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT64_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT64_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta)                  :: bmi !< metadata for allocated block

  $TYPE($KIND), dimension($DIMENSION), contiguous, pointer :: tmp_ptr
  integer(C_INT64_T), dimension(size(min_bound)) :: array_size

  nullify(array_ptr)
  bmi = block_meta(block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if (size(min_bound) .ne. size(max_bound))  then
    print *, 'ERROR: min and max bounds must have the same number of bounds to be able to allocate an array'
    return
  end if

  array_size(:) = max_bound - min_bound + 1
  if (present(use_safe_alloc)) then
    bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size, use_safe_alloc)
  else
    bmi = this % allocate_${RI}${L}_${D}D(tmp_ptr, array_size)
  end if

  if (associated(tmp_ptr)) then
    array_ptr($(dims="min_bound(1):max_bound(1)"; for i in $(seq 2 ${D}); do dims="${dims}, min_bound(${i}):max_bound(${i})"; done; echo ${dims})) => &
        tmp_ptr($DIMENSION)
  end if
end function allocate_${RI}${L}_${D}D_bounds

!> Wrapper on ${TYPE}*${L} ${D}D to be able to call it, specifying bounds with integer*4 arrays
function allocate_${RI}${L}_${D}D_bounds_int4(this, array_ptr, min_bound, max_bound, use_safe_alloc) result(bmi) ! ${TYPE}*${L} ${D}D array allocator (with custom bounds)
  implicit none
  class(shmem_heap), intent(INOUT)  :: this    !< shmem_heap instance
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), contiguous, pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT32_T), dimension(:), intent(IN) :: min_bound  !< min bounds of array array_ptr
  integer(C_INT32_T), dimension(:), intent(IN) :: max_bound  !< max bounds of array array_ptr
  logical, intent(in), optional     :: use_safe_alloc !< Whether to lock the heap when doing the allocation (for multiple allocator processes)
  type(block_meta)                  :: bmi !< metadata for allocated block

  integer(C_INT64_T), dimension(size(min_bound)) :: min_i8
  integer(C_INT64_T), dimension(size(max_bound)) :: max_i8
  min_i8(:) = min_bound(:)
  max_i8(:) = max_bound(:)
  if (present(use_safe_alloc)) then
    bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8, use_safe_alloc)
  else
    bmi = this % allocate_${RI}${L}_${D}D_bounds(array_ptr, min_i8, max_i8)
  end if
end function allocate_${RI}${L}_${D}D_bounds_int4

!> Convert a fortran pointer to the corresponding heap block metadata 
function ptr_to_blockmeta_${RI}${L}${D}D(p, bm) result(status)
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(IN), pointer :: p !< Fortran pointer to convert
  type(block_meta), intent(OUT) :: bm   !< Corresponding block metadata
  integer :: status

  ${PARAM_DECLARE}
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*${L}+16*${RI}+${D}
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, ${D}                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_${RI}${L}${D}D

!> Retrieve the fortran pointer from a given heap block metadata
function blockmeta_to_ptr_${RI}${L}${D}D(p, bm, strict) result(status)
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p !< [out] Fortran pointer corresponding to the given block
  type(block_meta), intent(IN) :: bm !< [in] Block to convert to a pointer
  logical, intent(IN), value :: strict !< [in] Whether to fail if the given Fortran pointer does not have the correct type, kind and rank
  integer :: status !< To indicate whether the conversion was successful

  ${PARAM_DECLARE}
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*${L}+16*${RI}+${D}                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. ${L}) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. ${RI}) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > ${D}) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:${D}))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_${RI}${L}${D}D

EOT
    done
  done
done
