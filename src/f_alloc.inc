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
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I1_5D

!! ===============================  integer*1 4D  ===============================
!> \brief integer*1 4D array allocator
subroutine I1_4D(h, p, di) ! integer*1 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I1_4D

!! ===============================  integer*1 3D  ===============================
!> \brief integer*1 3D array allocator
subroutine I1_3D(h, p, di) ! integer*1 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I1_3D

!! ===============================  integer*1 2D  ===============================
!> \brief integer*1 2D array allocator
subroutine I1_2D(h, p, di) ! integer*1 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I1_2D

!! ===============================  integer*1 1D  ===============================
!> \brief integer*1 1D array allocator
subroutine I1_1D(h, p, di) ! integer*1 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT8_T), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT8_T) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I1_1D

!! ===============================  integer*2 5D  ===============================
!> \brief integer*2 5D array allocator
subroutine I2_5D(h, p, di) ! integer*2 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I2_5D

!! ===============================  integer*2 4D  ===============================
!> \brief integer*2 4D array allocator
subroutine I2_4D(h, p, di) ! integer*2 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I2_4D

!! ===============================  integer*2 3D  ===============================
!> \brief integer*2 3D array allocator
subroutine I2_3D(h, p, di) ! integer*2 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I2_3D

!! ===============================  integer*2 2D  ===============================
!> \brief integer*2 2D array allocator
subroutine I2_2D(h, p, di) ! integer*2 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I2_2D

!! ===============================  integer*2 1D  ===============================
!> \brief integer*2 1D array allocator
subroutine I2_1D(h, p, di) ! integer*2 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_SHORT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_SHORT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I2_1D

!! ===============================  integer*4 5D  ===============================
!> \brief integer*4 5D array allocator
subroutine I4_5D(h, p, di) ! integer*4 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I4_5D

!! ===============================  integer*4 4D  ===============================
!> \brief integer*4 4D array allocator
subroutine I4_4D(h, p, di) ! integer*4 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I4_4D

!! ===============================  integer*4 3D  ===============================
!> \brief integer*4 3D array allocator
subroutine I4_3D(h, p, di) ! integer*4 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I4_3D

!! ===============================  integer*4 2D  ===============================
!> \brief integer*4 2D array allocator
subroutine I4_2D(h, p, di) ! integer*4 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I4_2D

!! ===============================  integer*4 1D  ===============================
!> \brief integer*4 1D array allocator
subroutine I4_1D(h, p, di) ! integer*4 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_INT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_INT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I4_1D

!! ===============================  integer*8 5D  ===============================
!> \brief integer*8 5D array allocator
subroutine I8_5D(h, p, di) ! integer*8 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I8_5D

!! ===============================  integer*8 4D  ===============================
!> \brief integer*8 4D array allocator
subroutine I8_4D(h, p, di) ! integer*8 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I8_4D

!! ===============================  integer*8 3D  ===============================
!> \brief integer*8 3D array allocator
subroutine I8_3D(h, p, di) ! integer*8 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I8_3D

!! ===============================  integer*8 2D  ===============================
!> \brief integer*8 2D array allocator
subroutine I8_2D(h, p, di) ! integer*8 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I8_2D

!! ===============================  integer*8 1D  ===============================
!> \brief integer*8 1D array allocator
subroutine I8_1D(h, p, di) ! integer*8 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  integer(C_LONG_LONG), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to integer array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  integer(C_LONG_LONG) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine I8_1D

!! ===============================  real*4 5D  ===============================
!> \brief real*4 5D array allocator
subroutine R4_5D(h, p, di) ! real*4 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R4_5D

!! ===============================  real*4 4D  ===============================
!> \brief real*4 4D array allocator
subroutine R4_4D(h, p, di) ! real*4 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R4_4D

!! ===============================  real*4 3D  ===============================
!> \brief real*4 3D array allocator
subroutine R4_3D(h, p, di) ! real*4 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R4_3D

!! ===============================  real*4 2D  ===============================
!> \brief real*4 2D array allocator
subroutine R4_2D(h, p, di) ! real*4 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R4_2D

!! ===============================  real*4 1D  ===============================
!> \brief real*4 1D array allocator
subroutine R4_1D(h, p, di) ! real*4 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_FLOAT), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_FLOAT) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R4_1D

!! ===============================  real*8 5D  ===============================
!> \brief real*8 5D array allocator
subroutine R8_5D(h, p, di) ! real*8 5D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:,:), intent(OUT), pointer :: p !< 5 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 5) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',5, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R8_5D

!! ===============================  real*8 4D  ===============================
!> \brief real*8 4D array allocator
subroutine R8_4D(h, p, di) ! real*8 4D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:,:), intent(OUT), pointer :: p !< 4 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 4) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',4, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R8_4D

!! ===============================  real*8 3D  ===============================
!> \brief real*8 3D array allocator
subroutine R8_3D(h, p, di) ! real*8 3D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:,:), intent(OUT), pointer :: p !< 3 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 3) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',3, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R8_3D

!! ===============================  real*8 2D  ===============================
!> \brief real*8 2D array allocator
subroutine R8_2D(h, p, di) ! real*8 2D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:,:), intent(OUT), pointer :: p !< 2 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 2) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',2, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R8_2D

!! ===============================  real*8 1D  ===============================
!> \brief real*8 1D array allocator
subroutine R8_1D(h, p, di) ! real*8 1D array allocator
  implicit none
  class(heap), intent(INOUT) :: h    !< heap object
  real(C_DOUBLE), dimension(:), intent(OUT), pointer :: p !< 1 dimensional pointer to real array
  integer, dimension(:), intent(IN) :: di   !< dimensions of array p (size(di) must be the same as rank of p)
  real(C_DOUBLE) :: pref

  nullify(p)                        ! in case of allocation failure
  if(size(di) .ne. 1) then       ! array rank, di dimension mismatch ?
    print *,'bad rank request, expecting',1, ', got',size(di)
  else                              ! NO, allocate array (size is in BYTES)
    call C_F_POINTER(ShmemHeapAllocBlock(h%p, PRODUCT(di)*C_SIZEOF(pref), 0), p, [di])
  endif
end subroutine R8_1D

