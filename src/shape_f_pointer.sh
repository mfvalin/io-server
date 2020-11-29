#!/bin/bash
MALLOC=ShmemHeapAllocBlock
# MALLOC=LocalAllocBlock
# same calling sequence as shared memory heap allocator, but calls malloc
cat <<EOT

  function LocalAllocBlock(heap, nbytes, safe) result(b)
    implicit none
    type(C_PTR), intent(IN), value :: heap
    integer(C_SIZE_T), intent(IN), value :: nbytes
    integer(C_INT), intent(IN), value :: safe
    type(C_PTR) :: b
    b = malloc(nbytes)
  end function LocalAllocBlock

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
subroutine ${RI}${L}_${D}D(h, p, di) ! ${TYPE}*${L} ${D}D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    ! heap
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p
  integer, dimension(:), intent(IN) :: di   ! array dimensions (size must be the same as rank of p)
  $TYPE($KIND) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. ${D}) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',${D}, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(${MALLOC}(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine ${RI}${L}_${D}D

EOT
    done
  done
done
