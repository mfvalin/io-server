! Copyright (C) 2020  Environnement Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020
!     V. Magnoux, Recherche en Prevision Numerique, 2020

!> \file
!  function LocalAllocBlock(heap, nbytes, safe) result(b)
!    implicit none
!    type(C_PTR), intent(IN), value :: heap
!    integer(C_SIZE_T), intent(IN), value :: nbytes
!    integer(C_INT), intent(IN), value :: safe
!    type(C_PTR) :: b
!    b = malloc(nbytes)
!  end function LocalAllocBlock

!! ====================  using ShmemHeapAllocBlock allocator ====================

!! ===============================  integer*1 5D  ===============================
!> \brief integer*1 5D array allocator
subroutine I1_5D(h, p, di) ! integer*1 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*1+16*I+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I1_5D

!! ===============================  integer*1 4D  ===============================
!> \brief integer*1 4D array allocator
subroutine I1_4D(h, p, di) ! integer*1 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*1+16*I+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I1_4D

!! ===============================  integer*1 3D  ===============================
!> \brief integer*1 3D array allocator
subroutine I1_3D(h, p, di) ! integer*1 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*1+16*I+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I1_3D

!! ===============================  integer*1 2D  ===============================
!> \brief integer*1 2D array allocator
subroutine I1_2D(h, p, di) ! integer*1 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*1+16*I+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I1_2D

!! ===============================  integer*1 1D  ===============================
!> \brief integer*1 1D array allocator
subroutine I1_1D(h, p, di) ! integer*1 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*1+16*I+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I1_1D

!! ===============================  integer*2 5D  ===============================
!> \brief integer*2 5D array allocator
subroutine I2_5D(h, p, di) ! integer*2 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*2+16*I+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I2_5D

!! ===============================  integer*2 4D  ===============================
!> \brief integer*2 4D array allocator
subroutine I2_4D(h, p, di) ! integer*2 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*2+16*I+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I2_4D

!! ===============================  integer*2 3D  ===============================
!> \brief integer*2 3D array allocator
subroutine I2_3D(h, p, di) ! integer*2 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*2+16*I+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I2_3D

!! ===============================  integer*2 2D  ===============================
!> \brief integer*2 2D array allocator
subroutine I2_2D(h, p, di) ! integer*2 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*2+16*I+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I2_2D

!! ===============================  integer*2 1D  ===============================
!> \brief integer*2 1D array allocator
subroutine I2_1D(h, p, di) ! integer*2 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*2+16*I+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I2_1D

!! ===============================  integer*4 5D  ===============================
!> \brief integer*4 5D array allocator
subroutine I4_5D(h, p, di) ! integer*4 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*I+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I4_5D

!! ===============================  integer*4 4D  ===============================
!> \brief integer*4 4D array allocator
subroutine I4_4D(h, p, di) ! integer*4 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*I+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I4_4D

!! ===============================  integer*4 3D  ===============================
!> \brief integer*4 3D array allocator
subroutine I4_3D(h, p, di) ! integer*4 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*I+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I4_3D

!! ===============================  integer*4 2D  ===============================
!> \brief integer*4 2D array allocator
subroutine I4_2D(h, p, di) ! integer*4 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*I+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I4_2D

!! ===============================  integer*4 1D  ===============================
!> \brief integer*4 1D array allocator
subroutine I4_1D(h, p, di) ! integer*4 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*I+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I4_1D

!! ===============================  integer*8 5D  ===============================
!> \brief integer*8 5D array allocator
subroutine I8_5D(h, p, di) ! integer*8 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*I+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I8_5D

!! ===============================  integer*8 4D  ===============================
!> \brief integer*8 4D array allocator
subroutine I8_4D(h, p, di) ! integer*8 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*I+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I8_4D

!! ===============================  integer*8 3D  ===============================
!> \brief integer*8 3D array allocator
subroutine I8_3D(h, p, di) ! integer*8 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*I+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I8_3D

!! ===============================  integer*8 2D  ===============================
!> \brief integer*8 2D array allocator
subroutine I8_2D(h, p, di) ! integer*8 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*I+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I8_2D

!! ===============================  integer*8 1D  ===============================
!> \brief integer*8 1D array allocator
subroutine I8_1D(h, p, di) ! integer*8 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*I+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine I8_1D

!! ===============================  real*4 5D  ===============================
!> \brief real*4 5D array allocator
subroutine R4_5D(h, p, di) ! real*4 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*R+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R4_5D

!! ===============================  real*4 4D  ===============================
!> \brief real*4 4D array allocator
subroutine R4_4D(h, p, di) ! real*4 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*R+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R4_4D

!! ===============================  real*4 3D  ===============================
!> \brief real*4 3D array allocator
subroutine R4_3D(h, p, di) ! real*4 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*R+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R4_3D

!! ===============================  real*4 2D  ===============================
!> \brief real*4 2D array allocator
subroutine R4_2D(h, p, di) ! real*4 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*R+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R4_2D

!! ===============================  real*4 1D  ===============================
!> \brief real*4 1D array allocator
subroutine R4_1D(h, p, di) ! real*4 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*4+16*R+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R4_1D

!! ===============================  real*8 5D  ===============================
!> \brief real*8 5D array allocator
subroutine R8_5D(h, p, di) ! real*8 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*R+5    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R8_5D

!! ===============================  real*8 4D  ===============================
!> \brief real*8 4D array allocator
subroutine R8_4D(h, p, di) ! real*8 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*R+4    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R8_4D

!! ===============================  real*8 3D  ===============================
!> \brief real*8 3D array allocator
subroutine R8_3D(h, p, di) ! real*8 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*R+3    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R8_3D

!! ===============================  real*8 2D  ===============================
!> \brief real*8 2D array allocator
subroutine R8_2D(h, p, di) ! real*8 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*R+2    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R8_2D

!! ===============================  real*8 1D  ===============================
!> \brief real*8 1D array allocator
subroutine R8_1D(h, p, di) ! real*8 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref
  type(block_meta_f08) :: bmi
  type(block_meta_f08) :: bmo
  type(block_meta_c)   :: bmc
  type(C_PTR) :: cptr
  integer(C_SIZE_T) :: asz
  integer :: tkr, status, bsz
  integer, parameter :: I = 1
  integer, parameter :: R = 2

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    bsz = C_SIZEOF(bmc)
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   !  size of data + size of metadata
    cptr = ShmemHeapAllocBlock(h%p, asz, 0)            ! allocate block
    if(.not. C_ASSOCIATED(cptr) ) return     ! allocation failed
    call C_F_POINTER(cptr, p, [di]) ! make Fortran pointer from C pointer
    tkr = 256*8+16*R+1    ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr = tkr                 ! build C interoperable metadata
    bmi%a%d = 1                     ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)    ! set relevant dimensions to correct value
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)  ! insert metadata into data block
! start of diagnostic code
    print 2,'type ',bmi%t(),' kind',bmi%k(),' rank',bmi%r(),' dims',bmi%dims(),' bmc',bsz
2   format(3(A,i3),A,5I8,A,i8)
    bmo%a = block_meta_c([-1,-1,-1,-1,-1], 0)         ! nullify metadata
    status = ShmemHeapGetBlockMeta(cptr, bmo%a, bsz)  ! get metadata from data block
    print 2,'type ',bmo%t(),' kind',bmo%k(),' rank',bmo%r(),' dims',bmo%dims()
! end of diagnostic code
  endif
end subroutine R8_1D

