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
function ${RI}${L}_${D}D(h, p, di) result(bmi) ! ${TYPE}*${L} ${D}D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p !< ${D} dimensional pointer to $TYPE array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  type(block_meta)                  :: bmi !< metadata for allocated block
  $TYPE($KIND) :: pref
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr, ref
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = TKR_INTEGER
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. ${D}) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',${D}, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata
    cptr = ${MALLOC}(h%p, asz, 0)            ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr = 256*${L}+16*${RI}+${D}    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d         = 1             ! set all 5 dimensions to 1
    bmi%a%d(1:${D}) = di(1:${D})    ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = transfer(cptr, bmi%a%offset) ! address of array
    bmi%a%offset = bmi%a%offset - asz           ! minus reference address  (offset in bytes)
    bmi%p        = cptr                         ! actual address of array
    status = ${METADATA}(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function ${RI}${L}_${D}D

function ${RI}${L}${D}D_bm(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer, parameter :: R = TKR_REAL
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
    bm % a % d(i) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ${RI}${L}${D}D_bm

function bm_${RI}${L}${D}D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  $TYPE($KIND), dimension($DIMENSION), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer, parameter :: R = TKR_REAL
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
end function bm_${RI}${L}${D}D

EOT
    done
  done
done
