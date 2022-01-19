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

!! ====================  using ShmemHeapAllocBlock allocator ====================

!! ===============================  integer*1 5D  ===============================
!> \brief integer*1 5D array allocator
function allocate_I1_5D(h, p, di, use_safe_alloc) result(bmi) ! integer*1 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT8_T) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*1+16*I+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I1_5D

function ptr_to_blockmeta_I15D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*1+16*I+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I15D

function blockmeta_to_ptr_I15D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*1+16*I+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 1) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I15D

!! ===============================  integer*1 4D  ===============================
!> \brief integer*1 4D array allocator
function allocate_I1_4D(h, p, di, use_safe_alloc) result(bmi) ! integer*1 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT8_T) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*1+16*I+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I1_4D

function ptr_to_blockmeta_I14D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*1+16*I+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I14D

function blockmeta_to_ptr_I14D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*1+16*I+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 1) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I14D

!! ===============================  integer*1 3D  ===============================
!> \brief integer*1 3D array allocator
function allocate_I1_3D(h, p, di, use_safe_alloc) result(bmi) ! integer*1 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT8_T) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*1+16*I+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I1_3D

function ptr_to_blockmeta_I13D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*1+16*I+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I13D

function blockmeta_to_ptr_I13D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*1+16*I+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 1) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I13D

!! ===============================  integer*1 2D  ===============================
!> \brief integer*1 2D array allocator
function allocate_I1_2D(h, p, di, use_safe_alloc) result(bmi) ! integer*1 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT8_T), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT8_T) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*1+16*I+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I1_2D

function ptr_to_blockmeta_I12D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT8_T), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*1+16*I+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I12D

function blockmeta_to_ptr_I12D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT8_T), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*1+16*I+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 1) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I12D

!! ===============================  integer*1 1D  ===============================
!> \brief integer*1 1D array allocator
function allocate_I1_1D(h, p, di, use_safe_alloc) result(bmi) ! integer*1 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT8_T), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT8_T) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*1+16*I+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I1_1D

function ptr_to_blockmeta_I11D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT8_T), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*1+16*I+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I11D

function blockmeta_to_ptr_I11D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT8_T), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*1+16*I+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 1) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I11D

!! ===============================  integer*2 5D  ===============================
!> \brief integer*2 5D array allocator
function allocate_I2_5D(h, p, di, use_safe_alloc) result(bmi) ! integer*2 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_SHORT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*2+16*I+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I2_5D

function ptr_to_blockmeta_I25D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_SHORT), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*2+16*I+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I25D

function blockmeta_to_ptr_I25D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_SHORT), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*2+16*I+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 2) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I25D

!! ===============================  integer*2 4D  ===============================
!> \brief integer*2 4D array allocator
function allocate_I2_4D(h, p, di, use_safe_alloc) result(bmi) ! integer*2 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_SHORT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*2+16*I+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I2_4D

function ptr_to_blockmeta_I24D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_SHORT), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*2+16*I+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I24D

function blockmeta_to_ptr_I24D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_SHORT), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*2+16*I+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 2) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I24D

!! ===============================  integer*2 3D  ===============================
!> \brief integer*2 3D array allocator
function allocate_I2_3D(h, p, di, use_safe_alloc) result(bmi) ! integer*2 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_SHORT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*2+16*I+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I2_3D

function ptr_to_blockmeta_I23D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_SHORT), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*2+16*I+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I23D

function blockmeta_to_ptr_I23D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_SHORT), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*2+16*I+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 2) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I23D

!! ===============================  integer*2 2D  ===============================
!> \brief integer*2 2D array allocator
function allocate_I2_2D(h, p, di, use_safe_alloc) result(bmi) ! integer*2 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_SHORT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_SHORT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*2+16*I+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I2_2D

function ptr_to_blockmeta_I22D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_SHORT), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*2+16*I+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I22D

function blockmeta_to_ptr_I22D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_SHORT), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*2+16*I+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 2) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I22D

!! ===============================  integer*2 1D  ===============================
!> \brief integer*2 1D array allocator
function allocate_I2_1D(h, p, di, use_safe_alloc) result(bmi) ! integer*2 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_SHORT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_SHORT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*2+16*I+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I2_1D

function ptr_to_blockmeta_I21D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_SHORT), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*2+16*I+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I21D

function blockmeta_to_ptr_I21D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_SHORT), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*2+16*I+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 2) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I21D

!! ===============================  integer*4 5D  ===============================
!> \brief integer*4 5D array allocator
function allocate_I4_5D(h, p, di, use_safe_alloc) result(bmi) ! integer*4 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*I+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I4_5D

function ptr_to_blockmeta_I45D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*I+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I45D

function blockmeta_to_ptr_I45D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*I+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I45D

!! ===============================  integer*4 4D  ===============================
!> \brief integer*4 4D array allocator
function allocate_I4_4D(h, p, di, use_safe_alloc) result(bmi) ! integer*4 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*I+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I4_4D

function ptr_to_blockmeta_I44D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*I+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I44D

function blockmeta_to_ptr_I44D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*I+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I44D

!! ===============================  integer*4 3D  ===============================
!> \brief integer*4 3D array allocator
function allocate_I4_3D(h, p, di, use_safe_alloc) result(bmi) ! integer*4 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*I+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I4_3D

function ptr_to_blockmeta_I43D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*I+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I43D

function blockmeta_to_ptr_I43D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*I+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I43D

!! ===============================  integer*4 2D  ===============================
!> \brief integer*4 2D array allocator
function allocate_I4_2D(h, p, di, use_safe_alloc) result(bmi) ! integer*4 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*I+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I4_2D

function ptr_to_blockmeta_I42D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*I+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I42D

function blockmeta_to_ptr_I42D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*I+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I42D

!! ===============================  integer*4 1D  ===============================
!> \brief integer*4 1D array allocator
function allocate_I4_1D(h, p, di, use_safe_alloc) result(bmi) ! integer*4 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_INT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_INT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*I+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I4_1D

function ptr_to_blockmeta_I41D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_INT), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*I+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I41D

function blockmeta_to_ptr_I41D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_INT), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*I+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I41D

!! ===============================  integer*8 5D  ===============================
!> \brief integer*8 5D array allocator
function allocate_I8_5D(h, p, di, use_safe_alloc) result(bmi) ! integer*8 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_LONG_LONG) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*I+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I8_5D

function ptr_to_blockmeta_I85D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*I+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I85D

function blockmeta_to_ptr_I85D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*I+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I85D

!! ===============================  integer*8 4D  ===============================
!> \brief integer*8 4D array allocator
function allocate_I8_4D(h, p, di, use_safe_alloc) result(bmi) ! integer*8 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_LONG_LONG) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*I+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I8_4D

function ptr_to_blockmeta_I84D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*I+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I84D

function blockmeta_to_ptr_I84D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*I+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I84D

!! ===============================  integer*8 3D  ===============================
!> \brief integer*8 3D array allocator
function allocate_I8_3D(h, p, di, use_safe_alloc) result(bmi) ! integer*8 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_LONG_LONG) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*I+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I8_3D

function ptr_to_blockmeta_I83D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*I+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I83D

function blockmeta_to_ptr_I83D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*I+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I83D

!! ===============================  integer*8 2D  ===============================
!> \brief integer*8 2D array allocator
function allocate_I8_2D(h, p, di, use_safe_alloc) result(bmi) ! integer*8 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_LONG_LONG) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*I+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I8_2D

function ptr_to_blockmeta_I82D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*I+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I82D

function blockmeta_to_ptr_I82D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_LONG_LONG), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*I+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I82D

!! ===============================  integer*8 1D  ===============================
!> \brief integer*8 1D array allocator
function allocate_I8_1D(h, p, di, use_safe_alloc) result(bmi) ! integer*8 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  integer(C_LONG_LONG), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  integer(C_LONG_LONG) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: I = TKR_INTEGER

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*I+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_I8_1D

function ptr_to_blockmeta_I81D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  integer(C_LONG_LONG), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*I+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_I81D

function blockmeta_to_ptr_I81D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  integer(C_LONG_LONG), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: I = TKR_INTEGER
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*I+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. I) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_I81D

!! ===============================  real*4 5D  ===============================
!> \brief real*4 5D array allocator
function allocate_R4_5D(h, p, di, use_safe_alloc) result(bmi) ! real*4 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_FLOAT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*R+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R4_5D

function ptr_to_blockmeta_R45D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_FLOAT), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*R+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R45D

function blockmeta_to_ptr_R45D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*R+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R45D

!! ===============================  real*4 4D  ===============================
!> \brief real*4 4D array allocator
function allocate_R4_4D(h, p, di, use_safe_alloc) result(bmi) ! real*4 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_FLOAT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*R+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R4_4D

function ptr_to_blockmeta_R44D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_FLOAT), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*R+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R44D

function blockmeta_to_ptr_R44D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*R+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R44D

!! ===============================  real*4 3D  ===============================
!> \brief real*4 3D array allocator
function allocate_R4_3D(h, p, di, use_safe_alloc) result(bmi) ! real*4 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_FLOAT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*R+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R4_3D

function ptr_to_blockmeta_R43D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_FLOAT), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*R+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R43D

function blockmeta_to_ptr_R43D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_FLOAT), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*R+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R43D

!! ===============================  real*4 2D  ===============================
!> \brief real*4 2D array allocator
function allocate_R4_2D(h, p, di, use_safe_alloc) result(bmi) ! real*4 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_FLOAT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_FLOAT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*R+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R4_2D

function ptr_to_blockmeta_R42D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_FLOAT), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*R+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R42D

function blockmeta_to_ptr_R42D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_FLOAT), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*R+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R42D

!! ===============================  real*4 1D  ===============================
!> \brief real*4 1D array allocator
function allocate_R4_1D(h, p, di, use_safe_alloc) result(bmi) ! real*4 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_FLOAT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_FLOAT) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*4+16*R+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R4_1D

function ptr_to_blockmeta_R41D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_FLOAT), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*4+16*R+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R41D

function blockmeta_to_ptr_R41D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_FLOAT), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*4+16*R+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 4) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R41D

!! ===============================  real*8 5D  ===============================
!> \brief real*8 5D array allocator
function allocate_R8_5D(h, p, di, use_safe_alloc) result(bmi) ! real*8 5D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_DOUBLE) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',5, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*R+5     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:5) = di(1:5)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R8_5D

function ptr_to_blockmeta_R85D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*R+5
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 5                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R85D

function blockmeta_to_ptr_R85D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*R+5                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 5) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:5))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R85D

!! ===============================  real*8 4D  ===============================
!> \brief real*8 4D array allocator
function allocate_R8_4D(h, p, di, use_safe_alloc) result(bmi) ! real*8 4D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_DOUBLE) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',4, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*R+4     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:4) = di(1:4)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R8_4D

function ptr_to_blockmeta_R84D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*R+4
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 4                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R84D

function blockmeta_to_ptr_R84D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*R+4                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 4) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:4))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R84D

!! ===============================  real*8 3D  ===============================
!> \brief real*8 3D array allocator
function allocate_R8_3D(h, p, di, use_safe_alloc) result(bmi) ! real*8 3D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_DOUBLE) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',3, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*R+3     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:3) = di(1:3)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R8_3D

function ptr_to_blockmeta_R83D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*R+3
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 3                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R83D

function blockmeta_to_ptr_R83D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*R+3                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 3) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:3))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R83D

!! ===============================  real*8 2D  ===============================
!> \brief real*8 2D array allocator
function allocate_R8_2D(h, p, di, use_safe_alloc) result(bmi) ! real*8 2D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_DOUBLE), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_DOUBLE) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',2, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*R+2     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:2) = di(1:2)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R8_2D

function ptr_to_blockmeta_R82D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_DOUBLE), dimension(:,:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*R+2
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 2                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R82D

function blockmeta_to_ptr_R82D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_DOUBLE), dimension(:,:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*R+2                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 2) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:2))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R82D

!! ===============================  real*8 1D  ===============================
!> \brief real*8 1D array allocator
function allocate_R8_1D(h, p, di, use_safe_alloc) result(bmi) ! real*8 1D array allocator
  implicit none
  class(heap), intent(INOUT)        :: h    !< heap object
  real(C_DOUBLE), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer(C_INT64_T), dimension(:), intent(IN) :: di  !< dimensions of array p (size(di) must be the same as rank of p)
  logical, intent(in), optional     :: use_safe_alloc
  type(block_meta)                  :: bmi !< metadata for allocated block
  real(C_DOUBLE) :: pref
  type(block_meta_c)  :: bmc
  type(C_PTR)         :: cptr, ref
  integer(C_SIZE_T)   :: asz
  integer             :: tkr, status, bsz
  integer(C_INT)      :: safe_alloc_flag
  integer, parameter :: R = TKR_REAL

  nullify(p)                        ! in case of allocation failure
  bmi = block_meta( block_meta_c([0,0,0,0,0], 0, 0), C_NULL_PTR)

  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?

    print *,'bad rank request, expecting',1, ', got',size(di)

  else                              ! NO, allocate array (size is in BYTES)

    bsz = C_SIZEOF(bmc)                      ! size of C metadata
    asz = PRODUCT(di)*C_SIZEOF(pref) + bsz   ! size of data + size of C metadata

    ! Determine value of "safe allocation" flag to use
    safe_alloc_flag = 0
    if (present(use_safe_alloc)) then
      if (use_safe_alloc) safe_alloc_flag = 1
    end if

    cptr = ShmemHeapAllocBlock(h%p, asz, safe_alloc_flag) ! allocate block in heap
    if(.not. C_ASSOCIATED(cptr) ) return        ! allocation failed

    call C_F_POINTER(cptr, p, [di]) ! make Fortran array pointer from C pointer

    tkr          = 256*8+16*R+1     ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
    bmi%a%tkr    = tkr              ! build C interoperable metadata
    bmi%a%d      = 1                ! set all 5 dimensions to 1
    bmi%a%d(1:1) = di(1:1)          ! set relevant dimensions to correct value
    ref          = h%get_base()                 ! get reference address for this heap
    asz          = transfer(ref, asz)           ! reference address
    bmi%a%offset = ShmemHeapOffsetFromPtr(h%p, cptr)           ! minus reference address  (offset in number of heap elements)
    bmi%p        = cptr                         ! actual address of array
    status = ShmemHeapSetBlockMeta(cptr, bmi%a, bsz)      ! insert metadata into data block

  endif
end function allocate_R8_1D

function ptr_to_blockmeta_R81D(p, bm) result(status)  ! fortran pointer to metadata translation
  implicit none
  real(C_DOUBLE), dimension(:), intent(IN), pointer :: p
  type(block_meta), intent(OUT) :: bm
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, ix

  status = -1
  bm % p = C_NULL_PTR               ! prepare for failure
  bm % a % d   = 0
  bm % a % tkr = 0
  if(.not. ASSOCIATED(p)) return    ! NULL pointer
  tkr = 256*8+16*R+1
  bm % a % tkr = tkr                ! TKR code hex [1/2/4/8] [1/2] [1/2/3/4/5]
  bm % a % d   = 1
  do ix = 1, 1                   ! copy array dimensions
    bm % a % d(ix) = size(p, ix)
  enddo
  bm % p = transfer(LOC(p), bm % p) ! array address
  
  status = 0
end function ptr_to_blockmeta_R81D

function blockmeta_to_ptr_R81D(p, bm, strict) result(status)  ! metadata to pointer translation
  implicit none
  real(C_DOUBLE), dimension(:), intent(OUT), pointer :: p
  type(block_meta), intent(IN) :: bm
  logical, intent(IN), value :: strict
  integer :: status

  integer, parameter :: R = TKR_REAL
  integer :: tkr, tp, kd, rk

  status = -1
  p => NULL()                                       ! prepare for failure
  if(.not. C_ASSOCIATED(bm % p) ) return            ! NULL pointer

  tkr = 256*8+16*R+1                      ! full TKR

  kd = ishft(bm % a % tkr, -8)                      ! expected kind 1/2/4/8
  if(kd .ne. 8) return                           ! wrong kind

  tp = and(15, ishft(bm % a % tkr, -4))             ! expected type 1/2
  if(tp .ne. R) return                          ! wrong type

  rk = and(bm % a % tkr, 15)                        ! expected rank 1/2/3/4/5
  if(rk > 1) return                              ! insufficient rank of pointer

  if((bm % a % tkr  .ne. tkr) .and. strict)    return     ! wrong STRICT type, kind, or rank

  call C_F_POINTER(bm % p, p, bm % a % d(1:1))   ! make Fortran array pointer from C pointer
  status = 0
end function blockmeta_to_ptr_R81D

