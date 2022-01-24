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
  [[ $RI == I ]] && PARAM_DECLARE="integer, parameter :: I = TKR_INTEGER"
  [[ $RI == R ]] && PARAM_DECLARE="integer, parameter :: R = TKR_REAL"
  for L in $RANGE ; do
    for D in 5 4 3 2 1 ; do
    if [[ $RI == I ]] ; then
      TYPE=integer
      [[ $L == 1 ]] && KIND='C_INT8_T'
      [[ $L == 2 ]] && KIND='C_SHORT'
      [[ $L == 4 ]] && KIND='C_INT'
      [[ $L == 8 ]] && KIND='C_LONG_LONG'
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
function allocate_${RI}${L}_${D}D(this, array_ptr, di, use_safe_alloc) result(bmi) ! ${TYPE}*${L} ${D}D array allocator
  implicit none
  class(heap), intent(INOUT)        :: this    !< heap object
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: array_ptr !< ${D} dimensional pointer to $TYPE array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array array_ptr (size(di) must be the same as rank of array_ptr)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  $TYPE($KIND) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  ${PARAM_DECLARE}

  nullify(array_ptr)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. ${D}) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',${D}, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ${MALLOC}(this % p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, array_ptr, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*${L}+16*${RI}+${D}     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:${D}) = di(1:${D})          ! set relevant dimensions to correct value
    bmi%a%offset = ShmemHeap_offset_from_pointer(this % p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ${METADATA}(this % p, cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_${RI}${L}_${D}D

function ptr_to_blockmeta_${RI}${L}${D}D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
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

function blockmeta_to_ptr_${RI}${L}${D}D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

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
