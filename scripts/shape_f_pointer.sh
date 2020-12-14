#!/bin/bash
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
subroutine ${RI}${L}_${D}D(h, p, di) ! ${TYPE}*${L} ${D}D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p !< ${D} dimensional pointer to $TYPE array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  $TYPE($KIND) :: pref
  type(block_meta_f08) :: bm
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. ${D}) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',${D}, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    asz = PRODUCT(di)*C_SIZEOF(pref) + C_SIZEOF(bmc)
    cptr = ${MALLOC}(h%p, asz, 0)   ! allocate block
!    asz = PRODUCT(di)               ! block size
    tkr = 256*${L}+16*${RI}+${D}
    bm%a%tkr = tkr
    bm%a%d = 1
    bm%a%d(1:${D}) = di(1:${D})
    print 2,'type ',bm%t(),' kind',bm%k(),' rank',bm%r(),' dims',bm%dims()
2   format(3(A,i3),A,5I8)
!    call ${METADATA}(cptr, tkr, asz, di)
    call C_F_POINTER(cptr, p, [di])
  endif
end subroutine ${RI}${L}_${D}D

EOT
    done
  done
done
