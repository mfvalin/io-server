#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sed -e 's/^\(.*\)$/! &/' < ${SCRIPT_DIR}/common_header.txt
echo

MALLOC=ShmemHeapAllocBlock
METADATA=ShmemHeapSetBlockMeta
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
subroutine ${RI}${L}_${D}D(h, p, di, bmi) ! ${TYPE}*${L} ${D}D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p !< ${D} dimensional pointer to $TYPE array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  type(block_meta_f08), intent(OUT) :: bmi !< metadata for allocated block
  $TYPE($KIND) :: pref
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta_f08( block_meta_c([0,0,0,0,0], 0), C_NULL_PTR)
  if(size(di) .ne. ${D}) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',${D}, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ${MALLOC}(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*${L}+16*${RI}+${D}    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:${D}) = di(1:${D})    ! set relevant dimensions to correct value
    bmi%p = cptr
    status = ${METADATA}(cptr, bmi%a, bsz)  ! insert metadata into data block
  endif
end subroutine ${RI}${L}_${D}D

EOT
    done
  done
done
